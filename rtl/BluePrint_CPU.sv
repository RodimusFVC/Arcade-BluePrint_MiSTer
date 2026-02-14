//============================================================================
//
//  Blue Print main CPU board model
//  Copyright (C) 2026 Rodimus
//
//  Permission is hereby granted, free of charge, to any person obtaining a
//  copy of this software and associated documentation files (the "Software"),
//  to deal in the Software without restriction, including without limitation
//  the rights to use, copy, modify, merge, publish, distribute, sublicense,
//  and/or sell copies of the Software, and to permit persons to whom the
//  Software is furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
//  DEALINGS IN THE SOFTWARE.
//
//============================================================================

module BluePrint_CPU
(
	input         reset,
	input         clk_49m,

	// Video outputs
	output  [4:0] red, green, blue,
	output        video_hsync, video_vsync, video_csync,
	output        video_hblank, video_vblank,
	output        ce_pix,

	// Player controls (active HIGH, directly from MiSTer)
	input   [7:0] p1_controls,    // {coin, start2, start1, btn1, left, down, right, up}
	input   [7:0] p2_controls,    // {3'b111, btn1, left, down, right, up}

	// DIP switch readback from sound board
	input   [7:0] dipsw_readback, // from sound board AY1 port A

	// Sound interface (directly to sound board)
	output  [7:0] sound_cmd,
	output        sound_cmd_wr,

	// Screen centering
	input   [3:0] h_center, v_center,

	// ROM loading
	input         main1_cs_i, main2_cs_i, main3_cs_i, main4_cs_i, main5_cs_i,
	input         tile0_cs_i, tile1_cs_i,
	input         spr_r_cs_i, spr_b_cs_i, spr_g_cs_i,
	input  [24:0] ioctl_addr,
	input   [7:0] ioctl_data,
	input         ioctl_wr,

	input         pause,

	// Hiscore interface
	input  [15:0] hs_address,
	input   [7:0] hs_data_in,
	output  [7:0] hs_data_out,
	input         hs_write
);

//------------------------------------------------------- Clock enables -------------------------------------------------------//

// Generate ~5 MHz pixel clock enable from 49.152 MHz
// 49.152 * 89/875 ≈ 4.997 MHz
wire [1:0] pix_cen_o;
jtframe_frac_cen #(2) pix_cen
(
	.clk(clk_49m),
	.n(10'd89),
	.m(10'd875),
	.cen(pix_cen_o),
	.cenb()
);
wire cen_5m = pix_cen_o[0];

// Generate ~3.5 MHz CPU clock enable from 49.152 MHz
// 49.152 * 5/70 ≈ 3.511 MHz
wire [1:0] cpu_cen_o;
jtframe_frac_cen #(2) cpu_cen
(
	.clk(clk_49m),
	.n(10'd5),
	.m(10'd70),
	.cen(cpu_cen_o),
	.cenb()
);
wire cen_3m5 = cpu_cen_o[0];

assign ce_pix = cen_5m;

//-------------------------------------------------------- Video timing --------------------------------------------------------//

// H counter 0-319, V counter 0-263
// From MAME: set_raw(5MHz, 320, 0, 256, 264, 16, 240)
reg [8:0] h_cnt = 9'd0;
reg [8:0] v_cnt = 9'd0;
always_ff @(posedge clk_49m) begin
	if (cen_5m) begin
		if (h_cnt == 9'd319) begin
			h_cnt <= 9'd0;
			v_cnt <= (v_cnt == 9'd263) ? 9'd0 : v_cnt + 9'd1;
		end else
			h_cnt <= h_cnt + 9'd1;
	end
end

// Blanking
wire hblk = (h_cnt >= 9'd256);
wire vblk = (v_cnt < 9'd16) | (v_cnt >= 9'd240);
assign video_hblank = hblk;
assign video_vblank = vblk;

// Sync generation with screen centering offsets
wire [8:0] hs_start = 9'd280 + {5'd0, h_center};
wire [8:0] hs_end   = hs_start + 9'd32;
wire [8:0] vs_start = 9'd248 + {5'd0, v_center};
wire [8:0] vs_end   = vs_start + 9'd4;
assign video_hsync = (h_cnt >= hs_start && h_cnt < hs_end);
assign video_vsync = (v_cnt >= vs_start && v_cnt < vs_end);
assign video_csync = ~(video_hsync ^ video_vsync);

//------------------------------------------------------------ CPU -------------------------------------------------------------//

// Main CPU - Zilog Z80 (T80s soft core)
wire [15:0] z80_A;
wire [7:0] z80_Dout;
wire n_mreq, n_iorq, n_rd, n_wr, n_rfsh, n_m1;
T80s cpu
(
	.RESET_n(reset),
	.CLK(clk_49m),
	.CEN(cen_3m5 & ~pause),
	.INT_n(n_irq),
	.NMI_n(1'b1),
	.WAIT_n(1'b1),
	.MREQ_n(n_mreq),
	.IORQ_n(n_iorq),
	.RD_n(n_rd),
	.WR_n(n_wr),
	.RFSH_n(n_rfsh),
	.M1_n(n_m1),
	.A(z80_A),
	.DI(z80_Din),
	.DO(z80_Dout)
);

//--------------------------------------------------------- Interrupts ---------------------------------------------------------//

// VBlank IRQ (irq0_line_hold style): assert on VBlank rising edge, clear on IORQ+M1
reg n_irq = 1'b1;
reg vblk_last = 1'b0;
always_ff @(posedge clk_49m) begin
	if (!reset) begin
		n_irq <= 1'b1;
		vblk_last <= 1'b0;
	end else begin
		vblk_last <= vblk;
		if (~reset | ~(n_iorq | n_m1))
			n_irq <= 1'b1;
		else if (vblk & ~vblk_last)
			n_irq <= 1'b0;
	end
end

//------------------------------------------------------ Address decoding ------------------------------------------------------//

wire mem_valid = ~n_mreq & n_rfsh;
wire cs_rom    = mem_valid & ~z80_A[15] & (z80_A[14:13] != 2'b11); // 0x0000-0x5FFF
wire cs_wram   = mem_valid & (z80_A[15:11] == 5'b10000);            // 0x8000-0x87FF
wire cs_vram   = mem_valid & (z80_A[15:12] == 4'h9);                // 0x9000-0x9FFF
wire cs_scroll = mem_valid & (z80_A[15:8]  == 8'hA0);               // 0xA000-0xA0FF
wire cs_sprite = mem_valid & (z80_A[15:8]  == 8'hB0);               // 0xB000-0xB0FF
wire cs_io_c   = mem_valid & (z80_A[15:12] == 4'hC);                // 0xC000-0xCFFF
wire cs_sndcmd = mem_valid & (z80_A[15:12] == 4'hD) & ~n_wr;        // 0xD000 write
wire cs_e000   = mem_valid & (z80_A[15:12] == 4'hE);                // 0xE000
wire cs_cram   = mem_valid & (z80_A[15:12] == 4'hF);                // 0xF000-0xFFFF

//------------------------------------------------------------ ROMs ------------------------------------------------------------//

// Main program ROMs (5x 4KB)
wire [7:0] rom1_D, rom2_D, rom3_D, rom4_D, rom5_D;
eprom_4k main_rom1(.CLK(clk_49m), .ADDR(z80_A[11:0]), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(main1_cs_i), .WR(ioctl_wr), .DATA(rom1_D));
eprom_4k main_rom2(.CLK(clk_49m), .ADDR(z80_A[11:0]), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(main2_cs_i), .WR(ioctl_wr), .DATA(rom2_D));
eprom_4k main_rom3(.CLK(clk_49m), .ADDR(z80_A[11:0]), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(main3_cs_i), .WR(ioctl_wr), .DATA(rom3_D));
eprom_4k main_rom4(.CLK(clk_49m), .ADDR(z80_A[11:0]), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(main4_cs_i), .WR(ioctl_wr), .DATA(rom4_D));
eprom_4k main_rom5(.CLK(clk_49m), .ADDR(z80_A[11:0]), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(main5_cs_i), .WR(ioctl_wr), .DATA(rom5_D));

// ROM data mux based on address
wire [7:0] rom_D = (z80_A[14:12] == 3'd0) ? rom1_D :
                   (z80_A[14:12] == 3'd1) ? rom2_D :
                   (z80_A[14:12] == 3'd2) ? rom3_D :
                   (z80_A[14:12] == 3'd3) ? rom4_D :
                   (z80_A[14:12] == 3'd4) ? rom5_D :
                   8'hFF;

// Tile ROMs (2x 4KB) — address inputs are placeholders, will be connected in Task 3B
wire [7:0] tile0_D, tile1_D;
eprom_4k tile_rom0(.CLK(clk_49m), .ADDR(12'd0), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(tile0_cs_i), .WR(ioctl_wr), .DATA(tile0_D));
eprom_4k tile_rom1(.CLK(clk_49m), .ADDR(12'd0), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(tile1_cs_i), .WR(ioctl_wr), .DATA(tile1_D));

// Sprite ROMs (3x 4KB) — address inputs are placeholders, will be connected in Task 3C
wire [7:0] spr_r_D, spr_b_D, spr_g_D;
eprom_4k spr_rom_r(.CLK(clk_49m), .ADDR(12'd0), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(spr_r_cs_i), .WR(ioctl_wr), .DATA(spr_r_D));
eprom_4k spr_rom_b(.CLK(clk_49m), .ADDR(12'd0), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(spr_b_cs_i), .WR(ioctl_wr), .DATA(spr_b_D));
eprom_4k spr_rom_g(.CLK(clk_49m), .ADDR(12'd0), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(spr_g_cs_i), .WR(ioctl_wr), .DATA(spr_g_D));

//-------------------------------------------------------------- RAM -----------------------------------------------------------//

// Work RAM (2KB, dual-port for hiscore)
wire [7:0] wram_D;
dpram_dc #(.widthad_a(11)) work_ram
(
	.clock_a(clk_49m),
	.wren_a(cs_wram & ~n_wr),
	.address_a(z80_A[10:0]),
	.data_a(z80_Dout),
	.q_a(wram_D),
	.clock_b(clk_49m),
	.wren_b(hs_write),
	.address_b(hs_address[10:0]),
	.data_b(hs_data_in),
	.q_b(hs_data_out)
);

// Video RAM (1KB)
wire [7:0] vram_D;
spram #(8, 10) video_ram
(
	.clk(clk_49m),
	.we(cs_vram & ~n_wr),
	.addr(z80_A[9:0]),
	.data(z80_Dout),
	.q(vram_D)
);

// Color RAM (1KB)
wire [7:0] cram_D;
spram #(8, 10) color_ram
(
	.clk(clk_49m),
	.we(cs_cram & ~n_wr),
	.addr(z80_A[9:0]),
	.data(z80_Dout),
	.q(cram_D)
);

// Scroll RAM (256 bytes)
wire [7:0] scroll_D;
spram #(8, 8) scroll_ram
(
	.clk(clk_49m),
	.we(cs_scroll & ~n_wr),
	.addr(z80_A[7:0]),
	.data(z80_Dout),
	.q(scroll_D)
);

// Sprite RAM (256 bytes)
wire [7:0] sprite_D;
spram #(8, 8) sprite_ram
(
	.clk(clk_49m),
	.we(cs_sprite & ~n_wr),
	.addr(z80_A[7:0]),
	.data(z80_Dout),
	.q(sprite_D)
);

//-------------------------------------------------------- I/O registers -------------------------------------------------------//

// Flip screen and gfx_bank (0xE000 write)
reg flip = 1'b0;
reg gfx_bank = 1'b0;
always_ff @(posedge clk_49m) begin
	if (!reset) begin
		flip <= 1'b0;
		gfx_bank <= 1'b0;
	end else if (cen_3m5 && cs_e000 && ~n_wr) begin
		flip <= z80_Dout[1];
		gfx_bank <= z80_Dout[2];
	end
end

// Sound command (0xD000 write)
reg [7:0] snd_cmd_reg = 8'd0;
reg snd_cmd_wr_reg = 1'b0;
always_ff @(posedge clk_49m) begin
	snd_cmd_wr_reg <= 1'b0;
	if (cen_3m5 && cs_sndcmd) begin
		snd_cmd_reg <= z80_Dout;
		snd_cmd_wr_reg <= 1'b1;
	end
end
assign sound_cmd = snd_cmd_reg;
assign sound_cmd_wr = snd_cmd_wr_reg;

//---------------------------------------------------- CPU data input mux -----------------------------------------------------//

wire [7:0] z80_Din = cs_rom                                ? rom_D :
                     (cs_wram & n_wr)                       ? wram_D :
                     (cs_vram & n_wr)                       ? vram_D :
                     (cs_cram & n_wr)                       ? cram_D :
                     (cs_scroll & n_wr)                     ? scroll_D :
                     (cs_sprite & n_wr)                     ? sprite_D :
                     (cs_io_c & ~n_rd & z80_A[1:0] == 2'b00) ? p1_controls :
                     (cs_io_c & ~n_rd & z80_A[1:0] == 2'b01) ? p2_controls :
                     (cs_io_c & ~n_rd & z80_A[1:0] == 2'b11) ? dipsw_readback :
                     8'hFF;

//----------------------------------------------------- Video output (stub) ----------------------------------------------------//

// Temporary — outputs black. Tilemap rendering (Task 3B) and sprite rendering (Task 3C) will replace this.
assign red   = 5'd0;
assign green = 5'd0;
assign blue  = 5'd0;

endmodule

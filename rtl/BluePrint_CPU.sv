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
wire irq_ack = ~n_iorq & ~n_m1;
always_ff @(posedge clk_49m) begin
	if (!reset) begin
		n_irq <= 1'b1;
		vblk_last <= 1'b0;
	end else begin
		if (irq_ack)
			n_irq <= 1'b1;
		else if (cen_5m && vblk && !vblk_last)
			n_irq <= 1'b0;
		vblk_last <= vblk;
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

// Tile ROMs (2x 4KB) — addressed by rendering pipeline
wire [7:0] tile0_D, tile1_D;
reg  [11:0] tile_render_addr;
eprom_4k tile_rom0(.CLK(clk_49m), .ADDR(tile_render_addr), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(tile0_cs_i), .WR(ioctl_wr), .DATA(tile0_D));
eprom_4k tile_rom1(.CLK(clk_49m), .ADDR(tile_render_addr), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(tile1_cs_i), .WR(ioctl_wr), .DATA(tile1_D));

// Sprite ROMs (3x 4KB) — addressed by sprite scanner
wire [7:0] spr_r_D, spr_b_D, spr_g_D;
reg  [11:0] spr_render_addr;
eprom_4k spr_rom_r(.CLK(clk_49m), .ADDR(spr_render_addr), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(spr_r_cs_i), .WR(ioctl_wr), .DATA(spr_r_D));
eprom_4k spr_rom_b(.CLK(clk_49m), .ADDR(spr_render_addr), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(spr_b_cs_i), .WR(ioctl_wr), .DATA(spr_b_D));
eprom_4k spr_rom_g(.CLK(clk_49m), .ADDR(spr_render_addr), .CLK_DL(clk_49m), .ADDR_DL(ioctl_addr), .DATA_IN(ioctl_data), .CS_DL(spr_g_cs_i), .WR(ioctl_wr), .DATA(spr_g_D));

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

// Video RAM (1KB) — port A: CPU, port B: video rendering
wire [7:0] vram_cpu_D, vram_render_D;
reg  [9:0] vram_render_addr;
dpram_dc #(.widthad_a(10)) video_ram
(
	.clock_a(clk_49m), .wren_a(cs_vram & ~n_wr),
	.address_a(z80_A[9:0]), .data_a(z80_Dout), .q_a(vram_cpu_D),
	.clock_b(clk_49m), .wren_b(1'b0),
	.address_b(vram_render_addr), .data_b(8'd0), .q_b(vram_render_D)
);

// Color RAM (1KB) — port A: CPU, port B: video rendering
wire [7:0] cram_cpu_D, cram_render_D;
reg  [9:0] cram_render_addr;
dpram_dc #(.widthad_a(10)) color_ram
(
	.clock_a(clk_49m), .wren_a(cs_cram & ~n_wr),
	.address_a(z80_A[9:0]), .data_a(z80_Dout), .q_a(cram_cpu_D),
	.clock_b(clk_49m), .wren_b(1'b0),
	.address_b(cram_render_addr), .data_b(8'd0), .q_b(cram_render_D)
);

// Scroll RAM (256 bytes) — port A: CPU, port B: video rendering
wire [7:0] scroll_cpu_D, scroll_render_D;
reg  [7:0] scroll_render_addr;
dpram_dc #(.widthad_a(8)) scroll_ram
(
	.clock_a(clk_49m), .wren_a(cs_scroll & ~n_wr),
	.address_a(z80_A[7:0]), .data_a(z80_Dout), .q_a(scroll_cpu_D),
	.clock_b(clk_49m), .wren_b(1'b0),
	.address_b(scroll_render_addr), .data_b(8'd0), .q_b(scroll_render_D)
);

// Sprite RAM (256 bytes) — port A: CPU, port B: sprite scanner
wire [7:0] sprite_cpu_D, sprite_scan_D;
reg  [7:0] sprite_scan_addr;
dpram_dc #(.widthad_a(8)) sprite_ram
(
	.clock_a(clk_49m), .wren_a(cs_sprite & ~n_wr),
	.address_a(z80_A[7:0]), .data_a(z80_Dout), .q_a(sprite_cpu_D),
	.clock_b(clk_49m), .wren_b(1'b0),
	.address_b(sprite_scan_addr), .data_b(8'd0), .q_b(sprite_scan_D)
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
                     (cs_vram & n_wr)                       ? vram_cpu_D :
                     (cs_cram & n_wr)                       ? cram_cpu_D :
                     (cs_scroll & n_wr)                     ? scroll_cpu_D :
                     (cs_sprite & n_wr)                     ? sprite_cpu_D :
                     (cs_io_c & ~n_rd & z80_A[1:0] == 2'b00) ? p1_controls :
                     (cs_io_c & ~n_rd & z80_A[1:0] == 2'b01) ? p2_controls :
                     (cs_io_c & ~n_rd & z80_A[1:0] == 2'b11) ? dipsw_readback :
                     8'hFF;

//--------------------------------------------------- Tilemap rendering pipeline ------------------------------------------------//

reg [7:0] tile_shift0, tile_shift1;
reg [6:0] tile_color_latch;
reg       tile_priority_latch;
reg       prev_bank_bit;

// Pipeline temporaries
reg [7:0] pipe_tile0, pipe_tile1;
reg [6:0] pipe_color;
reg       pipe_priority;
reg [2:0] pipe_fine_y;

wire [4:0] screen_col = h_cnt[7:3];
wire [4:0] fetch_col = screen_col;
wire [2:0] fine_x = h_cnt[2:0];
wire [7:0] screen_y = v_cnt[7:0] - 8'd16;
wire visible_line = (v_cnt >= 9'd16) && (v_cnt < 9'd240);

// cen_5m-stepped pipeline: fetches tile data for column (screen_col + 1) so it's
// ready to latch into shift registers at the NEXT fine_x==0.
//
// Timeline (all operations on cen_5m):
//   fine_x=0: Latch pipe data into shift regs. Set scroll_render_addr for col+1.
//   fine_x=1: Scroll data ready. Compute scrolled_y. Set vram/cram addr.
//   fine_x=2: Wait for VRAM/CRAM read.
//   fine_x=3: VRAM/CRAM data ready. Compute tile ROM addr. Update prev_bank_bit.
//   fine_x=4: Tile ROM data ready. Save into pipe_tile0/pipe_tile1.
//   fine_x=5,6,7: Just shift pixels.

always_ff @(posedge clk_49m) begin
	if (!reset) begin
		tile_shift0 <= 8'd0;
		tile_shift1 <= 8'd0;
		tile_color_latch <= 7'd0;
		tile_priority_latch <= 1'b0;
		prev_bank_bit <= 1'b0;
		pipe_tile0 <= 8'd0;
		pipe_tile1 <= 8'd0;
		pipe_color <= 7'd0;
		pipe_priority <= 1'b0;
		pipe_fine_y <= 3'd0;
	end else if (cen_5m) begin
		if (!visible_line) begin
			// During VBlank: output black, reset pipeline
			tile_shift0 <= 8'd0;
			tile_shift1 <= 8'd0;
			if (v_cnt == 9'd15 && h_cnt == 9'd0)
				prev_bank_bit <= 1'b0;
		end else begin
			case (fine_x)

				// --------------------------------------------------
				// 0: start fetch for next tile (prefetch)
				// --------------------------------------------------
				3'd7: begin
					if (flip)
						scroll_render_addr <= (8'd31 - {3'd7, fetch_col});
					else
						scroll_render_addr <= {3'd7, fetch_col};

					// shift current pixel
					tile_shift0 <= {1'b0, tile_shift0[7:1]};
					tile_shift1 <= {1'b0, tile_shift1[7:1]};
				end


				// --------------------------------------------------
				// 1: scroll data ready → compute row
				// --------------------------------------------------
				3'd1: begin
					pipe_fine_y <= (screen_y + scroll_render_D) & 8'h07;

					begin
						reg [7:0] scrolled_y_full;
						scrolled_y_full = screen_y + scroll_render_D;

						if (flip) begin
							vram_render_addr <= {(5'd31 - fetch_col), scrolled_y_full[7:3]};
							cram_render_addr <= {(5'd31 - fetch_col), scrolled_y_full[7:3]};
						end else begin
							vram_render_addr <= {fetch_col, scrolled_y_full[7:3]};
							cram_render_addr <= {fetch_col, scrolled_y_full[7:3]};
						end
					end

					tile_shift0 <= {1'b0, tile_shift0[7:1]};
					tile_shift1 <= {1'b0, tile_shift1[7:1]};
				end


				// --------------------------------------------------
				// 2: wait for VRAM/CRAM
				// --------------------------------------------------
				3'd2: begin
					tile_shift0 <= {1'b0, tile_shift0[7:1]};
					tile_shift1 <= {1'b0, tile_shift1[7:1]};
				end


				// --------------------------------------------------
				// 3: tile index + attributes ready
				// --------------------------------------------------
				3'd3: begin
					pipe_color    <= cram_render_D[6:0];
					pipe_priority <= cram_render_D[7];

					if (flip)
						tile_render_addr <= {prev_bank_bit & gfx_bank,
											vram_render_D,
											3'd7 - pipe_fine_y};
					else
						tile_render_addr <= {prev_bank_bit & gfx_bank,
											vram_render_D,
											pipe_fine_y};

					prev_bank_bit <= cram_render_D[6];

					tile_shift0 <= {1'b0, tile_shift0[7:1]};
					tile_shift1 <= {1'b0, tile_shift1[7:1]};
				end


				// --------------------------------------------------
				// 4: tile ROM data ready → LATCH IMMEDIATELY
				// --------------------------------------------------
				3'd4: begin
					if (h_cnt < 9'd256) begin
						if (flip) begin
							tile_shift0 <= {tile0_D[0],tile0_D[1],tile0_D[2],tile0_D[3],
											tile0_D[4],tile0_D[5],tile0_D[6],tile0_D[7]};
							tile_shift1 <= {tile1_D[0],tile1_D[1],tile1_D[2],tile1_D[3],
											tile1_D[4],tile1_D[5],tile1_D[6],tile1_D[7]};
						end else begin
							tile_shift0 <= {tile0_D[0],tile0_D[1],tile0_D[2],tile0_D[3],
											tile0_D[4],tile0_D[5],tile0_D[6],tile0_D[7]};
							tile_shift1 <= {tile1_D[0],tile1_D[1],tile1_D[2],tile1_D[3],
											tile1_D[4],tile1_D[5],tile1_D[6],tile1_D[7]};
						end

						tile_color_latch    <= pipe_color;
						tile_priority_latch <= pipe_priority;
					end else begin
						tile_shift0 <= 8'd0;
						tile_shift1 <= 8'd0;
					end
				end


				// --------------------------------------------------
				// 5,6,7: shift pixels
				// --------------------------------------------------
				default: begin
					tile_shift0 <= {1'b0, tile_shift0[7:1]};
					tile_shift1 <= {1'b0, tile_shift1[7:1]};
				end

			endcase
		end
	end
end

//------------------------------------------------------ Palette computation ----------------------------------------------------//

wire [1:0] tile_pixel = {tile_shift1[0], tile_shift0[0]}; // LSB first
wire tile_intensity = tile_color_latch[6];
wire [2:0] color_hi = tile_color_latch[5:3]; // RBG for pixel==10
wire [2:0] color_lo = tile_color_latch[2:0]; // RBG for pixel==01

wire [2:0] pen_rbg = (tile_pixel == 2'b00) ? 3'b000 :
                     (tile_pixel == 2'b01) ? color_lo :
                     (tile_pixel == 2'b10) ? color_hi :
                     (color_lo | color_hi);  // 11

wire pen_r = pen_rbg[0];
wire pen_b = pen_rbg[1];
wire pen_g = pen_rbg[2];

wire [4:0] r_tile = pen_r ? (tile_intensity ? 5'd24 : 5'd31) : 5'd0;
wire [4:0] g_tile = pen_g ? (tile_intensity ? 5'd24 : 5'd31) : 5'd0;
wire [4:0] b_tile = pen_b ? (tile_intensity ? 5'd24 : 5'd31) : 5'd0;

wire tile_transparent = (tile_pixel == 2'b00);

//--------------------------------------------------- Sprite rendering pipeline -------------------------------------------------//

// Double-buffered line buffers (256 entries x 3 bits each, 0 = transparent)
reg [2:0] linebuf0 [0:255];
reg [2:0] linebuf1 [0:255];
reg       linebuf_sel = 1'b0; // Which buffer is being displayed

// Swap line buffers at start of each scanline
always_ff @(posedge clk_49m) begin
	if (!reset)
		linebuf_sel <= 1'b0;
	else if (cen_5m && h_cnt == 9'd0)
		linebuf_sel <= ~linebuf_sel;
end

// Sprite scanner state machine — runs at clk_49m during HBlank
// Scans all 64 sprites per line, writes matching pixels to line buffer
reg [3:0] spr_state;
reg [5:0] spr_idx;           // Current sprite index (0-63)
reg [7:0] spr_byte0;         // Y position byte
reg [7:0] spr_byte1;         // Tile code
reg [7:0] spr_byte2;         // Flags (bit 6 = flipX, bit 7 = next sprite's flipY)
reg [7:0] spr_byte3;         // X position
reg       spr_flipy;         // FlipY for current sprite (from previous sprite's byte2[7])
reg       prev_sprite_flipy; // Carried forward from previous sprite
reg [7:0] spr_rom_r_lat, spr_rom_b_lat, spr_rom_g_lat; // Latched ROM data
reg [3:0] spr_pix_cnt;       // Pixel counter within sprite row (0-7)
reg [7:0] spr_clear_addr;    // Address for clearing line buffer
reg [7:0] next_scanline;     // The scanline we're preparing sprites for

localparam SPR_IDLE     = 4'd0;
localparam SPR_CLEAR    = 4'd1;
localparam SPR_INIT_RD  = 4'd2; // Read sprite 63 byte 2 for initial flipY
localparam SPR_INIT_LAT = 4'd3;
localparam SPR_RD_B0    = 4'd4; // Read byte 0 (Y)
localparam SPR_RD_B1    = 4'd5; // Read byte 1 (tile code)
localparam SPR_RD_B2    = 4'd6; // Read byte 2 (flags)
localparam SPR_RD_B3    = 4'd7; // Read byte 3 (X)
localparam SPR_ROMADDR  = 4'd8; // Set ROM address, wait for data
localparam SPR_ROMWAIT  = 4'd9; // ROM data available
localparam SPR_PIXELS   = 4'd10; // Write pixels to line buffer
localparam SPR_NEXT     = 4'd11; // Advance to next sprite

always_ff @(posedge clk_49m) begin
	if (!reset) begin
		spr_state <= SPR_IDLE;
		spr_idx <= 6'd0;
		prev_sprite_flipy <= 1'b0;
		spr_clear_addr <= 8'd0;
	end else begin
		case (spr_state)
			SPR_IDLE: begin
				// Start sprite scan at beginning of HBlank
				if (cen_5m && h_cnt == 9'd256) begin
					// Compute next scanline number (the line being prepared)
					next_scanline <= v_cnt[7:0] - 8'd15;
					spr_clear_addr <= 8'd0;
					spr_state <= SPR_CLEAR;
				end
			end

			SPR_CLEAR: begin
				// Clear target line buffer (writing to ~linebuf_sel)
				if (~linebuf_sel)
					linebuf1[spr_clear_addr] <= 3'd0;
				else
					linebuf0[spr_clear_addr] <= 3'd0;
				if (spr_clear_addr == 8'd255) begin
					// Read sprite 63's byte 2 for initial prev_sprite_flipy
					sprite_scan_addr <= 8'hFE; // sprite 63, byte 2
					spr_state <= SPR_INIT_RD;
				end else
					spr_clear_addr <= spr_clear_addr + 8'd1;
			end

			SPR_INIT_RD: begin
				// Wait 1 cycle for RAM read
				spr_state <= SPR_INIT_LAT;
			end

			SPR_INIT_LAT: begin
				prev_sprite_flipy <= sprite_scan_D[7];
				spr_idx <= 6'd0;
				// Start reading sprite 0, byte 0
				sprite_scan_addr <= 8'd0; // sprite 0, byte 0
				spr_state <= SPR_RD_B0;
			end

			SPR_RD_B0: begin
				// Wait for byte 0, request byte 1
				sprite_scan_addr <= {spr_idx, 2'd1};
				spr_state <= SPR_RD_B1;
			end

			SPR_RD_B1: begin
				// Latch byte 0 (Y), request byte 2
				spr_byte0 <= sprite_scan_D;
				sprite_scan_addr <= {spr_idx, 2'd2};
				spr_state <= SPR_RD_B2;
			end

			SPR_RD_B2: begin
				// Latch byte 1 (tile code), request byte 3
				spr_byte1 <= sprite_scan_D;
				sprite_scan_addr <= {spr_idx, 2'd3};
				spr_state <= SPR_RD_B3;
			end

			SPR_RD_B3: begin
				// Latch byte 2 (flags)
				spr_byte2 <= sprite_scan_D;
				spr_flipy <= prev_sprite_flipy;
				spr_state <= SPR_ROMADDR;
			end

			SPR_ROMADDR: begin
				// Latch byte 3 (X), check if sprite is on this scanline
				spr_byte3 <= sprite_scan_D;
				prev_sprite_flipy <= spr_byte2[7]; // Save for next sprite

				// Compute sprite Y and check range
				// sy = 240 - byte0, draw at sy-1, so visible at (sy-1) to (sy-1+15)
				// Check: next_scanline - (240 - byte0 - 1) in range 0-15
				// Equivalent: next_scanline - 239 + byte0 in range 0-15
				begin
					reg [8:0] raw_line;
					raw_line = {1'b0, next_scanline} - 9'd239 + {1'b0, spr_byte0};
					if (raw_line[8] || raw_line[7:4] != 4'd0) begin
						// Not on this scanline — skip to next sprite
						spr_state <= SPR_NEXT;
					end else begin
						// Hit! Compute ROM address
						reg [3:0] line_in_sprite;
						line_in_sprite = spr_flipy ? (4'd15 - raw_line[3:0]) : raw_line[3:0];
						spr_render_addr <= {spr_byte1, line_in_sprite};
						spr_state <= SPR_ROMWAIT;
					end
				end
			end

			SPR_ROMWAIT: begin
				// Wait 1 cycle for ROM data
				spr_state <= SPR_PIXELS;
				spr_pix_cnt <= 4'd0;
				// Latch ROM data
				spr_rom_r_lat <= spr_r_D;
				spr_rom_b_lat <= spr_b_D;
				spr_rom_g_lat <= spr_g_D;
			end

			SPR_PIXELS: begin
				// Write 8 pixels to line buffer
				begin
					reg [2:0] bit_pos;
					reg [2:0] pixel_val;
					reg [7:0] x_pos;

					bit_pos = spr_byte2[6] ? spr_pix_cnt[2:0] : (3'd7 - spr_pix_cnt[2:0]);
					pixel_val = {spr_rom_g_lat[bit_pos], spr_rom_b_lat[bit_pos], spr_rom_r_lat[bit_pos]};
					x_pos = spr_byte3 + {4'd0, spr_pix_cnt[2:0]} + 8'd2; // +2 offset per MAME

					if (pixel_val != 3'd0) begin
						if (~linebuf_sel)
							linebuf1[x_pos] <= pixel_val;
						else
							linebuf0[x_pos] <= pixel_val;
					end

					if (spr_pix_cnt == 4'd7)
						spr_state <= SPR_NEXT;
					else
						spr_pix_cnt <= spr_pix_cnt + 4'd1;
				end
			end

			SPR_NEXT: begin
				if (spr_idx == 6'd63)
					spr_state <= SPR_IDLE;
				else begin
					spr_idx <= spr_idx + 6'd1;
					sprite_scan_addr <= {spr_idx + 6'd1, 2'd0}; // Next sprite byte 0
					spr_state <= SPR_RD_B0;
				end
			end

			default: spr_state <= SPR_IDLE;
		endcase
	end
end

//---------------------------------------------------- Sprite pixel readout -----------------------------------------------------//

wire [2:0] sprite_pixel = linebuf_sel ? linebuf1[h_cnt[7:0]] : linebuf0[h_cnt[7:0]];
wire sprite_transparent = (sprite_pixel == 3'b000);

wire [4:0] r_sprite = sprite_pixel[0] ? 5'd31 : 5'd0; // bit 0 = R
wire [4:0] g_sprite = sprite_pixel[2] ? 5'd31 : 5'd0; // bit 2 = G
wire [4:0] b_sprite = sprite_pixel[1] ? 5'd31 : 5'd0; // bit 1 = B

//------------------------------------------------------- Video output ----------------------------------------------------------//

// Compositing: priority-1 tiles > sprites > priority-0 tiles > black
wire [4:0] r_final = (tile_priority_latch && !tile_transparent) ? r_tile :
                     (!sprite_transparent)                       ? r_sprite :
                     r_tile;
wire [4:0] g_final = (tile_priority_latch && !tile_transparent) ? g_tile :
                     (!sprite_transparent)                       ? g_sprite :
                     g_tile;
wire [4:0] b_final = (tile_priority_latch && !tile_transparent) ? b_tile :
                     (!sprite_transparent)                       ? b_sprite :
                     b_tile;

assign red   = (hblk | vblk) ? 5'd0 : r_final;
assign green = (hblk | vblk) ? 5'd0 : g_final;
assign blue  = (hblk | vblk) ? 5'd0 : b_final;

endmodule

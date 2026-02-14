//============================================================================
//
//  Blue Print top-level module
//  Copyright (C) 2021 Ace
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

//Module declaration, I/O ports
module TimePilot
(
	input                reset,
	input                clk_49m,                  //Actual frequency: 49.152MHz
	input          [1:0] coin,                     //0 = coin 1, 1 = coin 2
	input          [1:0] start_buttons,            //0 = Player 1, 1 = Player 2
	input          [3:0] p1_joystick, p2_joystick, //0 = up, 1 = down, 2 = left, 3 = right
	input                p1_fire,
	input                p2_fire,
	input                btn_service,
	input         [15:0] dip_sw,
	output               video_hsync, video_vsync, video_csync,
	output               video_hblank, video_vblank,
	output               ce_pix,
	output         [4:0] video_r, video_g, video_b,
	output signed [15:0] sound,

	//Screen centering (alters HSync, VSync and VBlank timing in the Konami 082 to reposition the video output)
	input          [3:0] h_center, v_center,

	input         [24:0] ioctl_addr,
	input          [7:0] ioctl_data,
	input                ioctl_wr,
	input          [7:0] ioctl_index,

	input                pause,

	//This input serves to select different fractional dividers to acheive 1.789772MHz for the sound Z80 and AY-3-8910s
	//depending on whether Time Pilot runs with original or underclocked timings to normalize sync frequencies
	input                underclock,

	input         [15:0] hs_address,
	input          [7:0] hs_data_in,
	output         [7:0] hs_data_out,
	input                hs_write
);

//Linking signals between PCBs (main CPU board)
// TODO: A5, A6, irq_trigger, cs_controls_dip1, cs_dip2 are outputs from
// TimePilot_CPU that were consumed by the old TimePilot_SND. They are
// kept here so TimePilot_CPU compiles; they will be removed in Task 3.
wire A5, A6, irq_trigger, cs_sounddata, cs_controls_dip1, cs_dip2;
wire [7:0] cpubrd_D;
// Tie controls_dip to 8'hFF (no input) — old sound board drove this;
// Blue Print reads controls directly on the main CPU. Will be removed in Task 3.
wire [7:0] controls_dip = 8'hFF;

// Sound interface
wire [7:0] sound_cmd = cpubrd_D;       // Sound command byte from main CPU
wire       sound_cmd_wr = cs_sounddata; // Pulse when main CPU writes sound data
wire [7:0] dipsw_readback;             // Sound board → main CPU for 0xC003 reads

//ROM loader signals for MISTer (loads ROMs from SD card)
wire main1_cs_i, main2_cs_i, main3_cs_i, main4_cs_i, main5_cs_i;
wire tile0_cs_i, tile1_cs_i;
wire spr_r_cs_i, spr_b_cs_i, spr_g_cs_i;

// Filter ioctl_wr for index 0 (main board ROMs) and index 1 (sound ROMs)
wire ioctl_wr_main = ioctl_wr && (ioctl_index == 8'd0);
wire ioctl_wr_snd  = ioctl_wr && (ioctl_index == 8'd1);

// Sound ROM chip selects (within index 1's address space)
wire snd_rom1_cs_i = (ioctl_addr < 25'h1000);                              // First 4KB
wire snd_rom2_cs_i = (ioctl_addr >= 25'h1000) && (ioctl_addr < 25'h2000);  // Second 4KB

//MiSTer data write selector (active for ROM index 0 only)
selector DLSEL
(
	.ioctl_addr(ioctl_addr),
	.main1_cs(main1_cs_i),
	.main2_cs(main2_cs_i),
	.main3_cs(main3_cs_i),
	.main4_cs(main4_cs_i),
	.main5_cs(main5_cs_i),
	.tile0_cs(tile0_cs_i),
	.tile1_cs(tile1_cs_i),
	.spr_r_cs(spr_r_cs_i),
	.spr_b_cs(spr_b_cs_i),
	.spr_g_cs(spr_g_cs_i)
);

//TODO: Reconnect when CPU board is rewritten (Task 3)
//Instantiate main PCB
TimePilot_CPU main_pcb
(
	.reset(reset),
	.clk_49m(clk_49m),
	.red(video_r),
	.green(video_g),
	.blue(video_b),
	.video_hsync(video_hsync),
	.video_vsync(video_vsync),
	.video_csync(video_csync),
	.video_hblank(video_hblank),
	.video_vblank(video_vblank),
	.ce_pix(ce_pix),

	.h_center(h_center),
	.v_center(v_center),

	.controls_dip(controls_dip),
	.cpubrd_Dout(cpubrd_D),
	.cpubrd_A5(A5),
	.cpubrd_A6(A6),
	.cs_sounddata(cs_sounddata),
	.irq_trigger(irq_trigger),
	.cs_dip2(cs_dip2),
	.cs_controls_dip1(cs_controls_dip1),

	// TODO: These port mappings are placeholders - will be replaced when
	// CPU board module is rewritten for Blue Print in Task 3.
	// Old Time Pilot used ep1-ep6 (8KB each) + color/lookup PROMs.
	// Blue Print uses main1-main5 + tile0-tile1 + spr_r/b/g (all 4KB).
	.ep1_cs_i(main1_cs_i),
	.ep2_cs_i(main2_cs_i),
	.ep3_cs_i(main3_cs_i),
	.ep4_cs_i(main4_cs_i),
	.ep5_cs_i(main5_cs_i),
	.ep6_cs_i(tile0_cs_i),
	.cp1_cs_i(tile1_cs_i),
	.cp2_cs_i(spr_r_cs_i),
	.tl_cs_i(spr_b_cs_i),
	.sl_cs_i(spr_g_cs_i),
	.ioctl_addr(ioctl_addr),
	.ioctl_wr(ioctl_wr_main),
	.ioctl_data(ioctl_data),

	.pause(pause),

	.hs_address(hs_address),
	.hs_data_out(hs_data_out),
	.hs_data_in(hs_data_in),
	.hs_write(hs_write)
);

//Instantiate sound PCB
BluePrint_SND sound_pcb
(
	.reset(reset),
	.clk_49m(clk_49m),
	.sound_cmd(sound_cmd),
	.sound_cmd_wr(sound_cmd_wr),
	.dip_sw(dip_sw),
	.dipsw_readback(dipsw_readback),
	.sound(sound),
	.vblank(video_vblank),
	.snd_rom1_cs_i(snd_rom1_cs_i),
	.snd_rom2_cs_i(snd_rom2_cs_i),
	.ioctl_addr(ioctl_addr),
	.ioctl_data(ioctl_data),
	.ioctl_wr(ioctl_wr_snd)
);

endmodule

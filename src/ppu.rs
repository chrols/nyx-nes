//! http://wiki.nesdev.com/w/index.php/PPU_rendering

use std::cell::Cell;
use super::ines;

/// The PPU renders 262 scanlines per frame.
const SCANLINES_PER_FRAME: usize = 262;

/// Each scanline lasts for 341 PPU clock cycles
const CYCLES_PER_SCANLINE: usize = 341;

#[derive(Copy, Clone, Debug)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Color {
    pub fn new() -> Color {
        Color { r: 0, g: 0, b: 0 }
    }

    pub fn RGB(r: u8, g: u8, b: u8) -> Color {
        Color { r, g, b }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct OamData {
    top: u8,
    index: u8,
    attr: u8,
    left: u8,
}

impl OamData {
    fn new()  -> OamData {
        OamData {top:0,index:0,attr:0,left:0}
    }

    fn contains(&self, x: u8, y: u8) -> bool {
        let x_hit = x >= self.left && x < (self.left + 8);
        let y_hit = y >= self.top && y < (self.top + 8);
        x_hit && y_hit
    }
}

pub struct Ppu {
    // Background
    address: u16, // Current VRAM address

    // Sprites
    oam: [u8; 0x100],
    oam_addr: u8,
    vblank: Cell<bool>,
    sprite_zero: bool,
    sprite_overflow: bool,
    vram: [u8; 0x4000],
    low_address: bool,
    scroll_x: u8,
    scroll_y: u8,
    pub canvas: [Color; 256 * 240],
    pub updated: bool,
    nmi: bool,
    current_cycle: usize,
    scanline: usize,
    nametable: u8,
    attribute_table: u8,
    bg_pattern_offset: bool,
    tile_low: u8,
    tile_high: u8,
    sprite_offset: bool,
    pub rom: Option<ines::File>,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            oam: [0; 0x100],
            oam_addr: 0,
            vblank: Cell::new(false),
            sprite_zero: false,
            sprite_overflow: false,
            vram: [0; 0x4000],
            address: 0,
            low_address: false,
            scroll_x: 0,
            scroll_y: 0,
            canvas: [Color::new(); 256 * 240],
            updated: false,
            nmi: false,
            scanline: 0,
            current_cycle: 0,
            nametable: 0,
            attribute_table: 0,
            bg_pattern_offset: false,
            tile_low: 0,
            tile_high: 0,
            sprite_offset: false,
            rom: None,
        }
    }

    pub fn read(&self, address: u16) -> u8 {
        println!("PPUWREAD: {:X}", address);
        match address % 8 {
            2 => self.read_status(),
            _ => panic!("Unimplemented register: {:X}", address % 8),
        }
    }

    pub fn write(&mut self, address: u16, byte: u8) {
        println!("PPUWRITE: {:X} {:X}", address, byte);
        match address % 8 {
            0 => self.write_ppuctrl(byte),
            1 => self.write_ppumask(byte),
            3 => self.oam_addr = byte,
            4 => self.write_oamdata(byte),
            5 => self.write_scroll(byte),
            6 => self.write_address(byte),
            7 => {
                self.write_memory(self.address, byte);
                println!("PPUWDATA: {:04X}", self.address);
                self.address = self.address.wrapping_add(1);
            }
            _ => panic!("Unimplemented register: {:X}", address % 8),
        }
    }

    /// The PPU renders 262 scanlines per frame. Each scanline lasts for
    /// 341 PPU clock cycles (113.667 CPU clock cycles; 1 CPU cycle = 3
    /// PPU cycles), with each clock cycle producing one pixel. The line
    /// numbers given here correspond to how the internal PPU frame
    /// counters count lines.
    pub fn cycle(&mut self) {
        match self.scanline {
            0 => (),
            1...239 => self.visible_scanline_cycle(),
            240 => (),
            241 => self.vblank(),
            242...260 => (),
            261 => self.prerender_scanline(),
            _ => panic!("Invalid scanline encountered: {}", self.scanline),
        }

        self.current_cycle += 1;
        self.current_cycle %= CYCLES_PER_SCANLINE;

        if self.current_cycle == 0 {
            self.scanline += 1;
            self.scanline %= SCANLINES_PER_FRAME;
        }


    }

    /// Render a visible scanline
    fn visible_scanline_cycle(&mut self) {
        match self.current_cycle {
            0 => (),
            1...256 => self.visible_scanline_render_cycle(),
            256...320 => (),
            321...336 => (),
            337...340 => (),
            _ => (),
        }
    }


    fn read_tile_address(&self) -> u16 {
        let mut addr = 0x10 * self.nametable as u16;
        addr += (self.scanline as u16 - 1) % 8;

        if self.bg_pattern_offset {
            addr += 0x1000
        }

        addr
    }

    fn visible_scanline_render_cycle(&mut self) {
        self.render_pixel();

        match self.current_cycle {
            7 => {
                self.nametable = self.read_memory(self.address); // Nametable byte
                self.attribute_table = self.fetch_attribute_table(); // Attribute table byte
                let tile_address = self.read_tile_address();
                self.tile_low = self.read_memory(tile_address);
                self.tile_high = self.read_memory(tile_address + 8);
            }
            _ => (),
        }
    }

    /// Attribute tables are 64-bytes per nametable.
    /// Each byte controls 4x4 tiles. 2-bits per 2x2 tiles.
    ///
    /// Tables start at: $23C0, $27C0, $2BC0, or $2FC0,
    fn fetch_attribute_table(&mut self) -> u8 {
        // assert!(self.address >= 0x2000 && self.address < 0x3000);

        let address = (self.address / 0x50) * 8 + (self.address / 4);
        // assert!(address >= 0x2000 && address < 0x3000);

        self.vram[self.address as usize]
    }


    fn sprite_color(&mut self, x: u8, y: u8, oam: OamData) -> Color {
        let mut tile_addr = ((oam.index as u16) << 4);

        if self.sprite_offset {
            tile_addr += 0x1000;
        }

        let x_offset = x - oam.left;
        let y_offset = y - oam.top;

        let low_byte = self.read_memory(tile_addr  + y_offset as u16 );
        let high_byte = self.read_memory(tile_addr + y_offset as u16  + 8);

        let pixel = ((low_byte >> (x_offset as u8)) & 0x01) + 2 * ((high_byte >> (x_offset as u8)) & 0x01);

        match pixel {
            0 => Color::RGB(0,0,0),
            1 => Color::RGB(255,0,0),
            2 => Color::RGB(0,255,0),
            3 => Color::RGB(0,0,255),
            _ => panic!("Invalid result: {:X}", low_byte + high_byte),
        }
    }


    pub fn dump_memory(&mut self, addr: u16, blocks: usize) {
        let mut addr = addr;
        for i in 0..blocks {
            print!("{:04X}:", addr);
            for j in 0..16 {
                print!(" {:02X}", self.read_memory(addr));
                addr += 1;
            }
            println!("");
        }
    }

    pub fn dump_pattern_tables(&mut self) {
        self.dump_memory(0x0000, 512);
    }

    pub fn dump_nametables(&mut self) {
        self.dump_memory(0x2000, 256);
    }

    pub fn dump_oam(&mut self) {
        let mut addr = 0;
        for i in 0..64 {
            print!("{:04X}:", addr);
            for j in 0..4 {
                print!(" {:02X}", self.oam[i*4+j]);
                addr += 1;
            }
            println!("");
        }
    }


    fn read_oam(&self, oam_addr: usize) -> OamData {
        OamData {
            top: self.oam[oam_addr],
            index: self.oam[oam_addr+1],
            attr: self.oam[oam_addr+2],
            left: self.oam[oam_addr+3],
        }
    }

    fn sprite_pixel(&mut self) -> Color {
        let x = (self.current_cycle - 1) as u8;
        let y = (self.scanline - 1) as u8;

        for i in 0..64 {
            let oam = self.read_oam(i*4);
            if oam.contains(x, y) {
                // println!("Sprite color: {:?}", self.sprite_color(x, y, i % 4));
                return self.sprite_color(x, y, oam);
            }
        }

        Color::RGB(0,0,0)
    }


    fn chr_pixel(&mut self) -> Color {
        let x = (self.current_cycle - 1) as u16;
        let y = (self.scanline - 1) as u16;


        let sprite_addr = ((x / 8) * 16 + (y / 8) *  32 * 16 + (y % 8));

        let low_byte = self.read_memory(sprite_addr);
        let high_byte = self.read_memory(sprite_addr + 8);

        let pixel = Ppu::bytes_to_pixel(high_byte, low_byte, (x % 8) as u8);

        match pixel {
            0 => Color::RGB(0,0,0),
            1 => Color::RGB(255,0,0),
            2 => Color::RGB(0,255,0),
            3 => Color::RGB(0,0,255),
            _ => panic!("Invalid result: {:X}", low_byte + high_byte),
        }
    }


    fn bytes_to_pixel(high: u8, low: u8, x: u8) -> u8 {
        ((low >> ((8-x) as u8)) & 0x01) + 2 * ((high >> ((8 - x) as u8)) & 0x01)
    }

    fn bg_pixel(&mut self) -> Color {
        let x = (self.current_cycle - 1) as u16;
        let y = (self.scanline - 1) as u16;


        let index_addr = 0x2000 + (y / 8) * 32 + (x / 8);
        let index = self.read_memory(index_addr);
        let tile_addr = (index as u16) << 4;
        println!("Tile addr {:X}", tile_addr);

        self.tile_high = self.read_memory(tile_addr+ (y % 8) + 8);
        self.tile_low = self.read_memory(tile_addr + (y % 8));

        // println!("{:X} {:X}", self.tile_high, self.tile_low);
        let color = Ppu::bytes_to_pixel(self.tile_high, self.tile_low, (x % 8) as u8);

        match color {
            0 => Color::RGB(0,0,0),
            1 => Color::RGB(255,0,0),
            2 => Color::RGB(0,255,0),
            3 => Color::RGB(0,0,255),
            _ => panic!("Invalid result: {:X}", color),
        }
    }

    fn render_pixel(&mut self) {
        let x = self.current_cycle - 1;
        let y = self.scanline - 1;


        //self.canvas[(y * 256 + x) as usize] = self.sprite_pixel();
        self.canvas[(y * 256 + x) as usize] = self.bg_pixel();
        //self.canvas[(y * 256 + x) as usize] = self.chr_pixel();
    }

    // Each attribute table, starting at $23C0, $27C0, $2BC0, or $2FC0, is arranged as an 8x8 byte array:
    /// 7654 3210
    /// |||| ||++- Color bits 3-2 for top left quadrant of this byte
    /// |||| ++--- Color bits 3-2 for top right quadrant of this byte
    /// ||++------ Color bits 3-2 for bottom left quadrant of this byte
    /// ++-------- Color bits 3-2 for bottom right quadrant of this byte

    /// 7  bit  0
    /// ---- ----
    /// VPHB SINN
    /// |||| ||||
    /// |||| ||++- Base nametable address
    /// |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    /// |||| |+--- VRAM address increment per CPU read/write of PPUDATA
    /// |||| |     (0: add 1, going across; 1: add 32, going down)
    /// |||| +---- Sprite pattern table address for 8x8 sprites
    /// ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
    /// |||+------ Background pattern table address (0: $0000; 1: $1000)
    /// ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
    /// |+-------- PPU master/slave select
    /// |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
    /// +--------- Generate an NMI at the start of the
    ///            vertical blanking interval (0: off; 1: on)
    fn write_ppuctrl(&mut self, byte: u8) {
        self.nmi = (0x80 & byte) != 0;
        //self.slave = (0x40 & byte) != 0;
        // FIXME

        self.sprite_offset = (0x04 & byte) != 0;
    }

    /// 7  bit  0
    /// ---- ----
    /// BGRs bMmG
    /// |||| ||||
    /// |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
    /// |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
    /// |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
    /// |||| +---- 1: Show background
    /// |||+------ 1: Show sprites
    /// ||+------- Emphasize red
    /// |+-------- Emphasize green
    /// +--------- Emphasize blue
    fn write_ppumask(&mut self, byte: u8) {
        // FIXME
    }

    fn write_oamdata(&mut self, byte: u8) {
        self.oam[self.oam_addr as usize] = byte;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    fn write_address(&mut self, byte: u8) {
        println!("PPUADDR: {:X}", byte);
        if self.low_address {
            self.address |= byte as u16;
            self.low_address = false;
        } else {
            self.address = (byte as u16) << 8;
            self.low_address = true;
        }
    }

    fn write_scroll(&mut self, byte: u8) {
        println!("PPUSCROLL: {:X}", byte);
        if self.low_address {
            self.scroll_x = byte;
            self.low_address = false;
        } else {
            self.scroll_y = byte;
            self.low_address = true;
        }
    }

    /// $0000-$0FFF 	$1000 	Pattern table 0
    /// $1000-$1FFF 	$1000 	Pattern table 1
    /// $2000-$23FF 	$0400 	Nametable 0
    /// $2400-$27FF 	$0400 	Nametable 1
    /// $2800-$2BFF 	$0400 	Nametable 2
    /// $2C00-$2FFF 	$0400 	Nametable 3
    /// $3000-$3EFF 	$0F00 	Mirrors of $2000-$2EFF
    /// $3F00-$3F1F 	$0020 	Palette RAM indexes
    /// $3F20-$3FFF 	$00E0 	Mirrors of $3F00-$3F1F
    fn read_memory(&mut self, address: u16) -> u8 {
        match address {
            0x0000...0x1FFF => match &self.rom {
                Some(game) => {
                    game.chr_rom[address as usize]
                },
                None => panic!("No game loaded"),
            },
            0x2000...0x3FFF => self.vram[address as usize],
            _ => panic!("Unimplemented PPU address: {:X}", address),
        }
    }

    fn write_memory(&mut self, address: u16, byte: u8) {
        match address {
            0x0000...0x1FFF => println!("Write to rom ${:04X}!", address),
            0x2000...0x2FFF => self.vram[address as usize] = byte,
            0x3000...0x3EFF => self.vram[address as usize - 0x1000] = byte,
            0x3F00...0x3F1F => self.vram[address as usize] = byte,
            0x3F20...0x3FFF => self.vram[address as usize] = byte, // FIXME
            _ => panic!("Unimplemented PPU address: {:X}", address),
        }
    }

    fn convert_palette(palette: u8) -> Color {
        match palette {
            0 => Color::RGB(84, 84, 84),
            1 => Color::RGB(0, 30, 116),
            2 => Color::RGB(8, 16, 144),
            3 => Color::RGB(48, 0, 136),
            4 => Color::RGB(68, 0, 100),
            5 => Color::RGB(92, 0, 48),
            6 => Color::RGB(84, 4, 0),
            7 => Color::RGB(60, 24, 0),
            8 => Color::RGB(32, 42, 0),
            9 => Color::RGB(8, 58, 0),
            10 => Color::RGB(0, 64, 0),
            11 => Color::RGB(0, 60, 0),
            12 => Color::RGB(0, 50, 60),
            13 => Color::RGB(0, 0, 0),
            14 => Color::RGB(152, 150, 152),
            15 => Color::RGB(8, 76, 196),
            16 => Color::RGB(48, 50, 236),
            17 => Color::RGB(92, 30, 228),
            18 => Color::RGB(136, 20, 176),
            19 => Color::RGB(160, 20, 100),
            20 => Color::RGB(152, 34, 32),
            21 => Color::RGB(120, 60, 0),
            22 => Color::RGB(84, 90, 0),
            23 => Color::RGB(40, 114, 0),
            24 => Color::RGB(8, 124, 0),
            25 => Color::RGB(0, 118, 40),
            26 => Color::RGB(0, 102, 120),
            27 => Color::RGB(0, 0, 0),
            28 => Color::RGB(236, 238, 236),
            29 => Color::RGB(76, 154, 236),
            30 => Color::RGB(120, 124, 236),
            31 => Color::RGB(176, 98, 236),
            32 => Color::RGB(228, 84, 236),
            33 => Color::RGB(236, 88, 180),
            34 => Color::RGB(236, 106, 100),
            35 => Color::RGB(212, 136, 32),
            36 => Color::RGB(160, 170, 0),
            37 => Color::RGB(116, 196, 0),
            38 => Color::RGB(76, 208, 32),
            39 => Color::RGB(56, 204, 108),
            40 => Color::RGB(56, 180, 204),
            41 => Color::RGB(60, 60, 60),
            42 => Color::RGB(236, 238, 236),
            43 => Color::RGB(168, 204, 236),
            44 => Color::RGB(188, 188, 236),
            45 => Color::RGB(212, 178, 236),
            46 => Color::RGB(236, 174, 236),
            47 => Color::RGB(236, 174, 212),
            48 => Color::RGB(236, 180, 176),
            49 => Color::RGB(228, 196, 144),
            50 => Color::RGB(204, 210, 120),
            51 => Color::RGB(180, 222, 120),
            52 => Color::RGB(168, 226, 144),
            53 => Color::RGB(152, 226, 180),
            54 => Color::RGB(160, 214, 228),
            55 => Color::RGB(160, 162, 160),
            _ => panic!("Invalid palette {:X}", palette),
        }
    }

    /// 7  bit  0
    /// ---- ----
    /// VSO. ....
    /// |||| ||||
    /// |||+-++++- Least significant bits previously written into a PPU register
    /// |||        (due to register not being updated for this address)
    /// ||+------- Sprite overflow. The intent was for this flag to be set
    /// ||         whenever more than eight sprites appear on a scanline, but a
    /// ||         hardware bug causes the actual behavior to be more complicated
    /// ||         and generate false positives as well as false negatives; see
    /// ||         PPU sprite evaluation. This flag is set during sprite
    /// ||         evaluation and cleared at dot 1 (the second dot) of the
    /// ||         pre-render line.
    /// |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
    /// |          a nonzero background pixel; cleared at dot 1 of the pre-render
    /// |          line.  Used for raster timing.
    /// +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
    ///            Set at dot 1 of line 241 (the line *after* the post-render
    ///            line); cleared after reading $2002 and at dot 1 of the
    ///            pre-render line.
    fn read_status(&self) -> u8 {
        let mut byte = if self.vblank.get() { 0x80 } else { 0 };
        byte |= if self.sprite_zero { 0x40 } else { 0 };
        byte |= if self.sprite_overflow { 0x20 } else { 0 };
        self.vblank.set(false);
        byte
    }

    pub fn render_scanline(&mut self) {}

    fn read_nametable(&mut self) -> u8 {
        0
    }

    fn read_attribute_table(&mut self) -> u8 {
        0
    }

    fn read_low_tile(&mut self) -> u8 {
        0
    }

    fn read_high_tile(&mut self) -> u8 {
        0
    }

    pub fn vblank(&mut self) {
        self.vblank.set(true);
    }

    fn prerender_scanline(&mut self) {
        self.vblank.set(true);
        self.updated = true;
    }
}

// Cycles 1-256

// The data for each tile is fetched during this phase. Each memory access takes 2 PPU cycles to complete, and 4 must be performed per tile:

//     Nametable byte
//     Attribute table byte
//     Tile bitmap low
//     Tile bitmap high (+8 bytes from tile bitmap low)

// The data fetched from these accesses is placed into internal latches, and then fed to the appropriate shift registers when it's time to do so (every 8 cycles). Because the PPU can only fetch an attribute byte every 8 cycles, each sequential string of 8 pixels is forced to have the same palette attribute.

// Sprite zero hits act as if the image starts at cycle 2 (which is the same cycle that the shifters shift for the first time), so the sprite zero flag will be raised at this point at the earliest. Actual pixel output is delayed further due to internal render pipelining, and the first pixel is output during cycle 4.

// The shifters are reloaded during ticks 9, 17, 25, ..., 257.

// Note: At the beginning of each scanline, the data for the first two tiles is already loaded into the shift registers (and ready to be rendered), so the first tile that gets fetched is Tile 3.

// While all of this is going on, sprite evaluation for the next scanline is taking place as a seperate process, independent to what's happening here.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alternating_ppuaddr() {
        let mut ppu = Ppu::new();
        ppu.write_address(0x20);
        ppu.write_address(0x10);
        assert_eq!(0x2010, ppu.address);
    }
}
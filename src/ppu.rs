//! http://wiki.nesdev.com/w/index.php/PPU_rendering

use std::cell::Cell;
use std::panic;
use crate::mapper::Cartridge;

/// The PPU renders 262 scanlines per frame.
const SCANLINES_PER_FRAME: usize = 262;

/// Each scanline lasts for 341 PPU clock cycles
const CYCLES_PER_SCANLINE: usize = 341;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Color {
    pub fn new() -> Color {
        Color { r: 0, g: 0, b: 0 }
    }

    pub fn rgb(r: u8, g: u8, b: u8) -> Color {
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

fn palette(byte: u8) -> Color {
    match byte {
        0 => Color::rgb(84, 84, 84),
        1 => Color::rgb(0, 30, 116),
        2 => Color::rgb(8, 16, 144),
        3 => Color::rgb(48, 0, 136),
        4 => Color::rgb(68, 0, 100),
        5 => Color::rgb(92, 0, 48),
        6 => Color::rgb(84, 4, 0),
        7 => Color::rgb(60, 24, 0),
        8 => Color::rgb(32, 42, 0),
        9 => Color::rgb(8, 58, 0),
        10 => Color::rgb(0, 64, 0),
        11 => Color::rgb(0, 60, 0),
        12 => Color::rgb(0, 50, 60),
        13 => Color::rgb(0, 0, 0),
        14 => Color::rgb(0, 0, 0),
        15 => Color::rgb(0, 0, 0),
        16 => Color::rgb(152, 150, 152),
        17 => Color::rgb(8, 76, 196),
        18 => Color::rgb(48, 50, 236),
        19 => Color::rgb(92, 30, 228),
        20 => Color::rgb(136, 20, 176),
        21 => Color::rgb(160, 20, 100),
        22 => Color::rgb(152, 34, 32),
        23 => Color::rgb(120, 60, 0),
        24 => Color::rgb(84, 90, 0),
        25 => Color::rgb(40, 114, 0),
        26 => Color::rgb(8, 124, 0),
        27 => Color::rgb(0, 118, 40),
        28 => Color::rgb(0, 102, 120),
        29 => Color::rgb(0, 0, 0),
        30 => Color::rgb(0, 0, 0),
        31 => Color::rgb(0, 0, 0),
        32 => Color::rgb(236, 238, 236),
        33 => Color::rgb(76, 154, 236),
        34 => Color::rgb(120, 124, 236),
        35 => Color::rgb(176, 98, 236),
        36 => Color::rgb(228, 84, 236),
        37 => Color::rgb(236, 88, 180),
        38 => Color::rgb(236, 106, 100),
        39 => Color::rgb(212, 136, 32),
        40 => Color::rgb(160, 170, 0),
        41 => Color::rgb(116, 196, 0),
        42 => Color::rgb(76, 208, 32),
        43 => Color::rgb(56, 204, 108),
        44 => Color::rgb(56, 180, 204),
        45 => Color::rgb(60, 60, 60),
        46 => Color::rgb(0, 0, 0),
        47 => Color::rgb(0, 0, 0),
        48 => Color::rgb(236, 238, 236),
        49 => Color::rgb(168, 204, 236),
        50 => Color::rgb(188, 188, 236),
        51 => Color::rgb(212, 178, 236),
        52 => Color::rgb(236, 174, 236),
        53 => Color::rgb(236, 174, 212),
        54 => Color::rgb(236, 180, 176),
        55 => Color::rgb(228, 196, 144),
        56 => Color::rgb(204, 210, 120),
        57 => Color::rgb(180, 222, 120),
        58 => Color::rgb(168, 226, 144),
        59 => Color::rgb(152, 226, 180),
        60 => Color::rgb(160, 214, 228),
        61 => Color::rgb(160, 162, 160),
        62 => Color::rgb(0, 0, 0),
        63 => Color::rgb(0, 0, 0),
        _ => {
            println!("Illegal palette: {:X}", byte);
            Color::rgb(0,0,0)
        }
    }
}

impl OamData {
    fn new() -> OamData {
        OamData {
            top: 0,
            index: 0,
            attr: 0,
            left: 0,
        }
    }

    fn contains(&self, x: u8, y: u8) -> bool {
        let x_hit = x >= self.left && x < (self.left.saturating_add(8));
        let y_hit = y >= self.top && y < (self.top.saturating_add(8));
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
    pub canvas: [Color; 256 * 240],
    pub prev_canvas: [Color; 256 * 240],
    pub updated: bool,
    generate_nmi: bool,
    pub current_cycle: usize,
    pub scanline: usize,
    bg_pattern_offset: bool,
    tile_low: u8,
    tile_high: u8,
    sprite_offset: bool,
    base_namtable_addr: u8,
    pub cpu_nmi: bool,
    pub rom: Option<Box<Cartridge>>,
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
            canvas: [Color::new(); 256 * 240],
            prev_canvas: [Color::rgb(255,255,255); 256 * 240],
            updated: false,
            generate_nmi: false,
            scanline: 0,
            current_cycle: 0,
            bg_pattern_offset: false,
            tile_low: 0,
            tile_high: 0,
            sprite_offset: false,
            base_namtable_addr: 0,
            cpu_nmi: false,
            rom: None,
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match address % 8 {
            2 => self.read_status(),
            7 => {
                let m = self.read_memory(self.address);
                self.address = self.address.wrapping_add(1);
                m
            }
            _ => panic!("Unimplemented register: {:X}", address % 8),
        }
    }

    pub fn write(&mut self, address: u16, byte: u8) {
        match address % 8 {
            0 => self.write_ppuctrl(byte),
            1 => self.write_ppumask(byte),
            3 => {
                self.oam_addr = byte
            }
            4 => self.write_oamdata(byte),
            5 => self.write_scroll(byte),
            6 => self.write_address(byte),
            7 => {
                self.write_memory(self.address, byte);
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
            241 => if self.current_cycle == 1 { self.vblank() },
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

    fn visible_scanline_render_cycle(&mut self) {
        self.render_pixel();

        match self.current_cycle {
            7 => {
                //self.nametable = self.read_memory(self.address); // Nametable byte
                //self.attribute_table = self.fetch_attribute_table(); // Attribute table byte
                //let tile_address = self.read_tile_address();
                //self.tile_low = self.read_memory(tile_address);
                //self.tile_high = self.read_memory(tile_address + 8);
            }
            _ => (),
        }
    }

    fn mirror_byte(byte: u8) -> u8 {
        let mut mirror = 0;
        if byte & 0x01 != 0 { mirror |= 0x80; }
        if byte & 0x02 != 0 { mirror |= 0x40; }
        if byte & 0x04 != 0 { mirror |= 0x20; }
        if byte & 0x08 != 0 { mirror |= 0x10; }
        if byte & 0x10 != 0 { mirror |= 0x08; }
        if byte & 0x20 != 0 { mirror |= 0x04; }
        if byte & 0x40 != 0 { mirror |= 0x02; }
        if byte & 0x80 != 0 { mirror |= 0x01; }
        mirror
    }

    fn sprite_color(&mut self, x: u8, y: u8, oam: OamData) -> Option<Color> {
        let mut tile_addr = (oam.index as u16) << 4;

        if self.sprite_offset {
            tile_addr += 0x1000;
        }

        let horizontal_mirror =  oam.attr & 0x80 != 0;

        let y_offset = if horizontal_mirror {
            7 - (y - oam.top)
        } else {
            y - oam.top
        };

        let mut low_byte = self.read_memory(tile_addr + y_offset as u16);
        let mut high_byte = self.read_memory(tile_addr + y_offset as u16 + 8);

        let vertical_mirror = oam.attr & 0x40 != 0;

        if vertical_mirror {
            low_byte = Ppu::mirror_byte(low_byte);
            high_byte = Ppu::mirror_byte(high_byte);
        }

        let pixel = Ppu::bytes_to_pixel(high_byte, low_byte, x - oam.left);

        if pixel == 0 {
            return None;
        }

        assert!(pixel < 4);

        let palette_addr = (oam.attr as u16 & 0x3) * 4 + 0x3F10;
        let color = palette(self.read_memory(palette_addr + pixel as u16));
        Some(color)
    }

    pub fn dump_memory(&mut self, addr: u16, blocks: usize) {
        let mut addr = addr;
        for _i in 0..blocks {
            print!("{:04X}:", addr);
            for _j in 0..32 {
                print!(" {:02X}", self.read_memory(addr));
                addr += 1;
            }
            println!("");
        }
    }

    pub fn dump_pattern_tables(&mut self) {
        println!("--- Pattern tables ---");
        self.dump_memory(0x0000, 512);
    }

    pub fn dump_nametables(&mut self) {
        println!("--- Name tables ---");
        self.dump_memory(0x2000, 256);
    }

    pub fn dump_oam(&mut self) {
        println!("--- OAM ---");
        let mut addr = 0;
        for i in 0..64 {
            print!("{:04X}:", addr);
            for j in 0..4 {
                print!(" {:02X}", self.oam[i * 4 + j]);
                addr += 1;
            }
            println!("");
        }
    }

    fn read_oam(&self, oam_addr: usize) -> OamData {
        OamData {
            top: self.oam[oam_addr],
            index: self.oam[oam_addr + 1],
            attr: self.oam[oam_addr + 2],
            left: self.oam[oam_addr + 3],
        }
    }

    fn sprite_pixel(&mut self) -> Option<Color> {
        let x = (self.current_cycle - 1) as u8;
        let y = (self.scanline - 1) as u8;

        for i in 0..64 {
            let oam = self.read_oam(i * 4);
            if oam.contains(x, y) {
                if let Some(color) = self.sprite_color(x, y, oam) {
                    return Some(color);
                }
            }
        }

        None
    }

    fn bytes_to_pixel(high: u8, low: u8, x: u8) -> u8 {
        let x_offset = 7 - x;
        ((low >> x_offset) & 0x01) + 2 * ((high >> x_offset) & 0x01)
    }

    fn attribute_byte(&mut self, x: u16, y: u16) -> u8 {
        self.read_memory((y / 32) * 8 + (x / 32) + 0x23C0)
    }

    fn bg_palette_addr_base(&mut self, x: u16, y: u16) -> u16 {
        let attr_byte = self.attribute_byte(x, y);

        let ix = x % 32;
        let iy = y % 32;

        let attr = if ix >= 16 && iy >= 16 { // Lower right
            (attr_byte >> 6) & 0x3
        } else if iy >= 16 { // Upper right
            (attr_byte >> 4) & 0x3
        } else if ix >= 16 { // Lower left
            (attr_byte >> 2) & 0x3
        } else { // Upper left
            attr_byte & 0x3
        };

        (attr as u16 * 4) + 0x3F00
    }

    fn bg_pixel(&mut self) -> Option<Color> {
        let x = (self.current_cycle - 1) as u16;
        let y = (self.scanline - 1) as u16;

        let index_addr = 0x2000 + 0x400 * self.base_namtable_addr as u16 + (y / 8) * 32 + (x / 8);

        let index = self.read_memory(index_addr);
        let mut tile_addr = (index as u16) << 4;
        if self.bg_pattern_offset {
            tile_addr += 0x1000
        }

        self.tile_high = self.read_memory(tile_addr + (y % 8) + 8);
        self.tile_low = self.read_memory(tile_addr + (y % 8));

        let color = Ppu::bytes_to_pixel(self.tile_high, self.tile_low, (x % 8) as u8);

        if color == 0 {
            return None;
        }

        assert!(color < 4);

        let palette_addr = self.bg_palette_addr_base(x, y) + color as u16;
        Some(palette(self.read_memory(palette_addr)))
    }

    fn universal_bg(&mut self) -> Color {
        palette(self.read_memory(0x3F00))
    }

    fn render_pixel(&mut self) {
        let x = self.current_cycle - 1;
        let y = self.scanline - 1;

        self.canvas[(y * 256 + x) as usize] =
            if let Some(sprite_pixel) = self.sprite_pixel() {
                sprite_pixel
        } else if let Some(bg_pixel) = self.bg_pixel() {
                bg_pixel
        } else {
                self.universal_bg()
        }

        //self.canvas[(y * 256 + x) as usize] = self.chr_pixel();
    }


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
        self.generate_nmi = (0x80 & byte) != 0;
        self.bg_pattern_offset = (0x10 & byte) != 0;
        self.sprite_offset = (0x08 & byte) != 0;
        self.base_namtable_addr = 0x03 & byte;

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
        if self.low_address {
            self.address |= byte as u16;
            self.low_address = false;
        } else {
            self.address = (byte as u16) << 8;
            self.low_address = true;
        }
    }

    fn write_scroll(&mut self, byte: u8) {
        println!("Scroll unimplemented");
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
            0x0000...0x1FFF => match &mut self.rom {
                Some(game) => game.ppu_read(address),
                None => panic!("No game loaded"),
            },
            0x2000...0x3FFF => self.vram[address as usize],
            0x4000...0xFFFF => self.read_memory(address % 0x4000),
        }
    }

    fn write_memory(&mut self, address: u16, byte: u8) {
        match address {
            0x0000...0x1FFF => match &mut self.rom {
                Some(game) => game.ppu_write(address, byte),
                None => panic!("No game loaded"),
            },
            0x2000...0x2FFF => self.vram[address as usize] = byte,
            0x3000...0x3EFF => self.vram[address as usize - 0x1000] = byte,
            0x3F00...0x3F1F => self.vram[address as usize] = byte,
            0x3F20...0x3FFF => self.vram[address as usize] = byte, // FIXME
            0x4000...0xFFFF => self.write_memory(address % 0x4000, byte),
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

    pub fn vblank(&mut self) {
        self.vblank.set(true);
        if self.generate_nmi {
            self.cpu_nmi = true;
        }
    }

    fn prerender_scanline(&mut self) {
        self.vblank.set(true);
        self.updated = true;
    }

}

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

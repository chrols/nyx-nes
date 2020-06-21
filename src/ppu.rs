//! http://wiki.nesdev.com/w/index.php/PPU_rendering

use crate::ines;
use crate::mapper;
use crate::mapper::Cartridge;

use ines::Mirroring;
use std::panic;

use serde::{Deserialize, Serialize};

/// The PPU renders 262 scanlines per frame.
const SCANLINES_PER_FRAME: usize = 262;

/// Each scanline lasts for 341 PPU clock cycles
const CYCLES_PER_SCANLINE: usize = 341;

type ColorIndex = u8;

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
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

    pub fn to_grayscale(&self) -> Color {
        let v: u8 =
            ((self.r as f32 * 0.21) + (self.r as f32 * 0.72) + (self.r as f32 * 0.07)) as u8;
        Color { r: v, g: v, b: v }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct OamData {
    top: u8,
    index: u8,
    attr: u8,
    left: u8,
    sprite_zero: bool,
}

impl OamData {
    fn new() -> OamData {
        OamData {
            top: 0xFF,
            index: 0xFF,
            attr: 0xFF,
            left: 0xFF,
            sprite_zero: false,
        }
    }
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
            Color::rgb(0, 0, 0)
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Ppu {
    // Variables corresponding to physical registers

    // Background registers

    // yyy NN YYYYY XXXXX
    // ||| || ||||| +++++-- coarse X scroll
    // ||| || +++++-------- coarse Y scroll
    // ||| ++-------------- nametable select
    // +++----------------- fine Y scroll
    address: u16,       // Current VRAM address AKA v
    temp_address: u16,  // Temporary VRAM address AKA t
    write_toggle: bool, // AKA w
    bg_tile_low: u16,
    bg_tile_high: u16,
    attribute_low: u16,
    attribute_high: u16,
    fine_x: u8,

    // PPUCTRL flags
    vram_increment: bool,
    sprite_offset: bool,
    bg_pattern_offset: bool,
    large_sprites: bool,
    generate_nmi: bool,

    // Sprites
    oam: Vec<u8>,
    secondary_oam: [OamData; 8],
    oam_addr: u8,
    nmi_occurred: bool,
    sprite_zero: bool,
    sprite_overflow: bool,
    vram: Vec<u8>,

    show_left_bg: bool,
    show_left_sprites: bool,
    show_bg: bool,
    show_sprites: bool,
    grayscale: bool,
    data_buffer: u8,
    pub canvas: Vec<Color>,
    pub prev_canvas: Vec<Color>,
    pub updated: bool,
    pub current_cycle: usize,
    pub scanline: usize,
    pub frame: usize,
    pub cpu_nmi: bool,
    #[serde(skip)]
    pub rom: Option<Box<Cartridge>>,

    pub use_ntsc: bool,
    prev_pixel: u8,
    cycle_counter: u8,
    ntsc_palette: Vec<u32>,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            address: 0,
            vram_increment: false,
            large_sprites: false,
            temp_address: 0,
            write_toggle: false,
            bg_tile_low: 0,
            bg_tile_high: 0,
            fine_x: 0,
            attribute_low: 0,
            attribute_high: 0,
            oam: vec![0; 0x100],
            secondary_oam: [OamData::new(); 8],
            oam_addr: 0,
            nmi_occurred: false,
            sprite_zero: false,
            sprite_overflow: false,
            vram: vec![0; 0x4000],
            show_left_bg: true,
            show_left_sprites: true,
            show_bg: true,
            show_sprites: true,
            grayscale: false,
            data_buffer: 0,
            canvas: vec![Color::new(); 256 * 240],
            prev_canvas: vec![Color::rgb(255, 255, 255); 256 * 240],
            updated: false,
            generate_nmi: false,
            scanline: 0,
            frame: 0,
            current_cycle: 0,
            bg_pattern_offset: false,
            sprite_offset: false,
            cpu_nmi: false,
            rom: None,
            use_ntsc: false,
            prev_pixel: 0,
            cycle_counter: 0,
            ntsc_palette: vec![0; 3 * 64 * 512],
        }
    }

    pub fn load_game(&mut self, file: ines::File) {
        self.rom = Some(mapper::new_mapper(file));
    }

    pub fn invalidate(&mut self) {
        for i in 0..(256 * 240) {
            self.canvas[i] = Color::rgb(0, 0, 0);
            self.prev_canvas[i] = Color::rgb(255, 255, 255);
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match address % 8 {
            1 => 0,
            2 => self.read_status(),
            4 => self.read_oamdata(),
            7 => self.read_data(),
            _ => panic!(
                "PPU Read: Unimplemented register: {:X} => {}",
                address,
                address % 8
            ),
        }
    }

    pub fn write(&mut self, address: u16, byte: u8) {
        match address % 8 {
            0 => self.write_ppuctrl(byte),
            1 => self.write_ppumask(byte),
            3 => self.oam_addr = byte,
            4 => self.write_oamdata(byte),
            5 => self.write_scroll(byte),
            6 => self.write_address(byte),
            7 => self.write_data(byte),
            _ => panic!(
                "PPU Write: Unimplemented register: {:X} => {}",
                address,
                address % 8
            ),
        }
    }

    // PPUDATA
    fn read_data(&mut self) -> u8 {
        let m = self.read_memory(self.address);

        let increment = if self.vram_increment { 32 } else { 1 };
        self.address = self.address.wrapping_add(increment);

        if self.address < 0x3F00 {
            let t = self.data_buffer;
            self.data_buffer = m;
            t
        } else {
            // Reading palette data loads a memory location "underneath" it to the read buffer
            self.data_buffer = self.read_memory(self.address - increment - 0x1000);
            m
        }
    }

    fn write_data(&mut self, byte: u8) {
        self.write_memory(self.address, byte);
        let increment = if self.vram_increment { 32 } else { 1 };
        self.address = self.address.wrapping_add(increment);
    }

    /// The PPU renders 262 scanlines per frame. Each scanline lasts for
    /// 341 PPU clock cycles (113.667 CPU clock cycles; 1 CPU cycle = 3
    /// PPU cycles), with each clock cycle producing one pixel. The line
    /// numbers given here correspond to how the internal PPU frame
    /// counters count lines.
    pub fn cycle(&mut self) {
        match self.scanline {
            0...239 => self.visible_scanline_cycle(),
            240 => (),
            241 => {
                if self.current_cycle == 1 {
                    self.vblank()
                }
            }
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

        if self.current_cycle == 0 && self.scanline == 261 {
            self.frame += 1;
        }

        if self.odd_frame_cycle_skip() {
            self.current_cycle = 0;
            self.scanline = 0;
        }

        self.cycle_counter += 1;
        if self.cycle_counter == 3 {
            self.cycle_counter = 0;
        }
    }

    fn odd_frame_cycle_skip(&mut self) -> bool {
        self.rendering_enabled()
            && self.frame % 2 == 1
            && self.scanline == 261
            && self.current_cycle == 340
    }

    fn prerender_scanline(&mut self) {
        match self.current_cycle {
            0 => self.updated = true,
            1 => {
                self.sprite_zero = false;
                self.nmi_occurred = false;
            }
            2...255 => {
                if self.current_cycle % 8 == 0 {
                    self.coarse_x_increment()
                }
            }
            256 => self.y_increment(),
            257 => self.horizontal_t2v(),
            258 | 259 => (),
            260 => {
                if self.rendering_enabled() {
                    if let Some(game) = &mut self.rom {
                        game.on_scanline();
                    }
                }
            }
            261...279 => (),
            280...304 => self.vertical_t2v(),
            305...320 => (),
            321...336 => self.prefetch(),
            _ => (),
        }
    }

    /// Render a visible scanline
    fn visible_scanline_cycle(&mut self) {
        match self.current_cycle {
            0 => (),
            1...255 => {
                self.render_pixel();
                if self.current_cycle % 8 == 0 {
                    self.update_tile();
                }
            }
            256 => {
                self.render_pixel();
                self.update_tile();
                self.y_increment();
            }
            257 => {
                self.horizontal_t2v();
                self.clear_secondary_oam();
                self.sprite_evaluation();
            }
            258 | 259 => (),
            260 => {
                if self.rendering_enabled() {
                    if let Some(game) = &mut self.rom {
                        game.on_scanline();
                    }
                }
            }
            261...320 => (),
            321...336 => self.prefetch(),
            337...339 => (),
            _ => (),
        }
    }

    fn mirror_byte(byte: u8) -> u8 {
        let mut mirror = 0;
        if byte & 0x01 != 0 {
            mirror |= 0x80;
        }
        if byte & 0x02 != 0 {
            mirror |= 0x40;
        }
        if byte & 0x04 != 0 {
            mirror |= 0x20;
        }
        if byte & 0x08 != 0 {
            mirror |= 0x10;
        }
        if byte & 0x10 != 0 {
            mirror |= 0x08;
        }
        if byte & 0x20 != 0 {
            mirror |= 0x04;
        }
        if byte & 0x40 != 0 {
            mirror |= 0x02;
        }
        if byte & 0x80 != 0 {
            mirror |= 0x01;
        }
        mirror
    }

    fn tile_address_for_index(&self, index: u8) -> u16 {
        if self.large_sprites {
            Ppu::address_large_tile(index)
        } else {
            Ppu::address_regular_tile(index, self.sprite_offset)
        }
    }

    fn address_large_tile(index: u8) -> u16 {
        (((index & 0xFE) as u16) << 4) + if (index & 1) != 0 { 0x1000 } else { 0 }
    }

    fn address_regular_tile(index: u8, offset: bool) -> u16 {
        ((index as u16) << 4) + if offset { 0x1000 } else { 0 }
    }

    // Return the color for the sprite described by oam at the current
    // beam position (if applicable).
    fn sprite_color(&mut self, x: u8, y: u8, oam: OamData) -> Option<ColorIndex> {
        let tile_addr = self.tile_address_for_index(oam.index);
        let horizontal_mirror = oam.attr & 0x80 != 0;

        // Should not be necessary AFAIK, but it will crash the debug
        // build with when they are 1 > 0
        if oam.top > y {
            return None;
        }

        let y_naive_offset = if horizontal_mirror {
            let bottom = if self.large_sprites { 15 } else { 7 };
            bottom - (y - oam.top)
        } else {
            y - oam.top
        };
        let y_offset = if self.large_sprites && y_naive_offset >= 8 {
            y_naive_offset + 8
        } else {
            y_naive_offset
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
        let color = self.read_memory(palette_addr + pixel as u16);
        Some(color)
    }

    pub fn dump_memory(&mut self, addr: u16, blocks: usize) {
        let mut addr = addr;
        for _i in 0..blocks {
            print!("{:04X}:", addr);
            for _j in 0..32 {
                print!(" {:02X}", self.vram[addr as usize]);
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
        self.dump_memory(0x2000, 129);
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

    // Return the color for the sprite at the current beam position
    // (if any).
    fn sprite_pixel(&mut self) -> Option<ColorIndex> {
        let x = (self.current_cycle) as u8;
        let y = (self.scanline) as u8;

        if !self.show_sprites || (x < 8 && !self.show_left_sprites) {
            return None;
        }

        // Everything in secondary OAM is already is on the current line
        for i in 0..8 {
            let oam = self.secondary_oam[i];
            if let Some(column) = x.checked_sub(oam.left) {
                if column < 8 {
                    if let Some(color) = self.sprite_color(x, y, oam) {
                        // FIXME Ordering important due to bg_pixel
                        // mutation

                        let bg_pixel = self.bg_pixel();

                        // Check for sprite zero hit
                        //
                        // Apparently sprite zero hit cannot occur at
                        // x=255 due to "an obscure reason related to
                        // the pixel pipeline."
                        if oam.sprite_zero && bg_pixel != None && x != 255 && self.scanline < 239 {
                            self.sprite_zero = true;
                        }

                        return if bg_pixel != None && (0x20 & oam.attr) != 0 {
                            bg_pixel
                        } else {
                            Some(color)
                        };
                    }
                }
            }
        }

        None
    }

    fn bytes_to_pixel(high: u8, low: u8, x: u8) -> u8 {
        assert!(x < 8);
        let x_offset = 7 - x;
        ((low >> x_offset) & 0x01) + 2 * ((high >> x_offset) & 0x01)
    }

    fn attribute_byte(&mut self) -> u8 {
        let attribute_address = 0x23C0
            | (self.address & 0x0C00)
            | ((self.address >> 4) & 0x38)
            | ((self.address >> 2) & 0x07);
        let attribute_byte = self.read_memory(attribute_address);
        let shift = ((self.address >> 4) & 4) | (self.address & 2);

        ((attribute_byte >> shift) & 3)
    }

    // FIXME Mutates state. Make explicit?
    fn bg_pixel(&mut self) -> Option<ColorIndex> {
        let color = if (self.bg_tile_low << self.fine_x & 0x8000) != 0 {
            1
        } else {
            0
        } + if (self.bg_tile_high << self.fine_x & 0x8000) != 0 {
            2
        } else {
            0
        };

        self.bg_tile_low <<= 1;
        self.bg_tile_high <<= 1;

        let attribute = if (self.attribute_low << self.fine_x & 0x8000) != 0 {
            1
        } else {
            0
        } + if (self.attribute_high << self.fine_x & 0x8000) != 0 {
            2
        } else {
            0
        };

        self.attribute_low <<= 1;
        self.attribute_high <<= 1;

        if color == 0 {
            return None;
        }

        if !self.show_bg || (self.current_cycle - 1 < 8 && !self.show_left_bg) {
            return None;
        }

        assert!(color < 4);
        let palette_addr = (attribute as u16 * 4) + 0x3F00 + color as u16;
        Some(self.read_memory(palette_addr))
    }

    fn universal_bg(&mut self) -> ColorIndex {
        self.read_memory(0x3F00)
    }

    // Render pixel for the current "beam" position
    fn render_pixel(&mut self) {
        // Rendering occurs on cycle 1..256. Underlying canvas is
        // 0-indexed.
        let x = self.current_cycle - 1;
        let y = self.scanline;

        let ci = if let Some(sprite_pixel) = self.sprite_pixel() {
            sprite_pixel
        } else if let Some(bg_pixel) = self.bg_pixel() {
            bg_pixel
        } else {
            self.universal_bg()
        };

        if self.use_ntsc {
            self.canvas[(y * 256 + x) as usize] = self.ntsc_color(ci);
        } else {
            self.canvas[(y * 256 + x) as usize] = palette(ci);
        }

        if self.grayscale {
            let p = self.canvas[(y * 256 + x) as usize];
            self.canvas[(y * 256 + x) as usize] = p.to_grayscale();
        }
    }

    fn clear_secondary_oam(&mut self) {
        for i in 0..8 {
            self.secondary_oam[i] = OamData::new();
        }
    }

    /// FIXME: Not 100% accurate, does not consider sprite overflow
    fn sprite_evaluation(&mut self) {
        // If both bg and sprites are disabled no sprite evaluation should take place
        if !self.rendering_enabled() {
            return;
        }

        let sprite_size = if self.large_sprites { 16 } else { 8 };
        let y = self.scanline as u8;

        let mut n = 0;
        for i in 0..64 {
            let oi = i * 4;

            let y_pos = self.oam[oi];

            match y.checked_sub(y_pos) {
                Some(line) if line < sprite_size => {
                    self.secondary_oam[n] = OamData {
                        top: self.oam[oi].saturating_add(1),
                        index: self.oam[oi + 1],
                        attr: self.oam[oi + 2],
                        left: self.oam[oi + 3].saturating_add(1),
                        sprite_zero: i == 0,
                    };

                    n += 1;
                }
                _ => (),
            }

            if n == 8 {
                break;
            }
        }
    }

    /// $2000 PPUCTRL

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
        // Update base nametable
        // t: ... BA.. .... .... = d: .... ..BA
        self.temp_address = (self.temp_address & 0xF3FF) | (byte as u16 & 0x3) << 10;

        self.vram_increment = (0x04 & byte) != 0;
        self.sprite_offset = (0x08 & byte) != 0;

        self.bg_pattern_offset = (0x10 & byte) != 0;
        self.large_sprites = (0x20 & byte) != 0;
        self.generate_nmi = (0x80 & byte) != 0;
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
        self.show_left_bg = (0x02 & byte) != 0;
        self.show_left_sprites = (0x04 & byte) != 0;

        self.show_bg = (0x08 & byte) != 0;
        self.show_sprites = (0x10 & byte) != 0;

        self.grayscale = (0x01 & byte) != 0;

        if (0x20 & byte) != 0 {
            println!("Do not know how to emphasize red");
        }

        if (0x40 & byte) != 0 {
            println!("Do not know how to emphasize green");
        }

        if (0x80 & byte) != 0 {
            println!("Do not know how to emphasize blue");
        }
    }

    fn read_oamdata(&mut self) -> u8 {
        self.oam[self.oam_addr as usize]
    }

    fn write_oamdata(&mut self, byte: u8) {
        self.oam[self.oam_addr as usize] = byte;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    // $2006  PPUADDR
    fn write_address(&mut self, byte: u8) {
        if self.write_toggle {
            // t: ....... HGFEDCBA = d: HGFEDCBA
            // v                   = t
            // w:                  = 0
            self.temp_address = (self.temp_address & 0xFF00) | byte as u16;
            self.address = self.temp_address;
            self.write_toggle = false;
        } else {
            // t: .FEDCBA ........ = d: ..FEDCBA
            // t: X...... ........ = 0
            // w:                  = 1
            self.temp_address = (self.temp_address & 0x00FF) | ((byte as u16) << 8);
            self.write_toggle = true;
        }
    }

    fn write_scroll(&mut self, byte: u8) {
        if self.write_toggle {
            // t: .CBA ..HG FED. .... = d: HGFEDCBA
            // w:                  = 0
            self.temp_address = (self.temp_address & 0x8C1F)
                | (byte as u16 & 0x07) << 12
                | (byte as u16 & 0xF8) << 2;
            self.write_toggle = false;
        } else {
            // t: ....... ...HGFED = d: HGFED...
            // x:              CBA = d: .....CBA
            // w:                  = 1
            self.temp_address = (self.temp_address & 0xFFE0) | (byte >> 3) as u16;
            self.fine_x = byte & 0x7;
            self.write_toggle = true;
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
            0x0000...0x1FFF => match &mut self.rom {
                Some(game) => game.ppu_read(address),
                None => panic!("No game loaded"),
            },
            0x2000...0x3FFF => self.vram[self.actual_vram_address(address) as usize],
            0x4000...0xFFFF => self.read_memory(address % 0x4000),
        }
    }

    fn write_memory(&mut self, address: u16, byte: u8) {
        match address {
            0x0000...0x1FFF => match &mut self.rom {
                Some(game) => game.ppu_write(address, byte),
                None => panic!("No game loaded"),
            },
            0x2000...0x3FFF => {
                let address = self.actual_vram_address(address) as usize;
                self.vram[address] = byte;
            }
            0x4000...0xFFFF => self.write_memory(address % 0x4000, byte),
        }
    }

    fn actual_vram_address(&self, address: u16) -> u16 {
        let mirroring = if let Some(game) = &self.rom {
            game.mirroring()
        } else {
            Mirroring::Vertical
        };

        match address {
            0x2000...0x23FF => match mirroring {
                Mirroring::Vertical => address,
                Mirroring::Horizontal => address,
                Mirroring::SingleScreenLower => address,
                Mirroring::SingleScreenUpper => address + 0x400,
            },
            0x2400...0x27FF => match mirroring {
                Mirroring::Vertical => address,
                Mirroring::Horizontal => address - 0x400,
                Mirroring::SingleScreenLower => address - 0x400,
                Mirroring::SingleScreenUpper => address,
            },
            0x2800...0x2BFF => match mirroring {
                Mirroring::Vertical => address - 0x800,
                Mirroring::Horizontal => address,
                Mirroring::SingleScreenLower => address - 0x800,
                Mirroring::SingleScreenUpper => address - 0x400,
            },
            0x2C00...0x2FFF => match mirroring {
                Mirroring::Vertical => address - 0x800,
                Mirroring::Horizontal => address - 0x400,
                Mirroring::SingleScreenLower => address - 0xC00,
                Mirroring::SingleScreenUpper => address - 0x800,
            },
            0x3000...0x3EFF => self.actual_vram_address(address - 0x1000),
            0x3F00...0x3F0F => address,
            0x3F10 => 0x3F00,
            0x3F11...0x3F13 => address,
            0x3F14 => 0x3F04,
            0x3F15...0x3F17 => address,
            0x3F18 => 0x3F08,
            0x3F19...0x3F1B => address,
            0x3F1C => 0x3F0C,
            0x3F1D...0x3F1F => address,
            0x3F20...0x3FFF => 0x3F00 + (address - 0x3F00) % 0x20,
            _ => panic!("Invalid VRAM address provided: {:04X}", address),
        }
    }

    /// $2002 PPUSTATUS

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
    fn read_status(&mut self) -> u8 {
        // w:                  = 0
        self.write_toggle = false;

        let mut byte = if self.nmi_occurred { 0x80 } else { 0 };
        byte |= if self.sprite_zero { 0x40 } else { 0 };
        byte |= if self.sprite_overflow { 0x20 } else { 0 };
        self.nmi_occurred = false;
        byte
    }

    pub fn vblank(&mut self) {
        if self.generate_nmi {
            self.cpu_nmi = true;
        }
        self.nmi_occurred = true;
    }

    fn horizontal_t2v(&mut self) {
        if self.rendering_enabled() {
            // v: ....F.. ...EDCBA = t: ....F.. ...EDCBA
            self.address = (self.address & 0xFBE0) | (self.temp_address & 0x041F);
        }
    }

    fn vertical_t2v(&mut self) {
        // v: IHG F.ED CBA. .... = t: IHG F.ED CBA. ....
        if self.rendering_enabled() {
            self.address = (self.address & 0x041F) | (self.temp_address & !0x041F);
        }
    }

    fn rendering_enabled(&self) -> bool {
        self.show_bg || self.show_sprites
    }

    fn update_tile(&mut self) {
        let index_addr = 0x2000 | (self.address & 0xFFF);
        let index = self.read_memory(index_addr);
        let mut tile_addr = (index as u16) << 4;

        if self.bg_pattern_offset {
            tile_addr += 0x1000
        }

        self.bg_tile_high = (self.bg_tile_high & 0xFF00)
            | (self.read_memory(tile_addr + (self.address >> 12) + 8) as u16);
        self.bg_tile_low = (self.bg_tile_low & 0xFF00)
            | (self.read_memory(tile_addr + (self.address >> 12)) as u16);

        self.attribute_high &= 0xFF00;
        self.attribute_low &= 0xFF00;

        let ab = self.attribute_byte();
        self.attribute_low |= if (ab & 1) != 0 { 0x00FF } else { 0 };
        self.attribute_high |= if (ab & 2) != 0 { 0x00FF } else { 0 };

        self.coarse_x_increment();
    }

    fn prefetch(&mut self) {
        if self.current_cycle == 336 {
            self.update_tile();
            self.bg_tile_high <<= 8;
            self.bg_tile_low <<= 8;
            self.attribute_high <<= 8;
            self.attribute_low <<= 8;

            self.update_tile();
        }
    }

    fn coarse_x_increment(&mut self) {
        if !self.rendering_enabled() {
            return;
        }

        if (self.address & 0x001F) == 31 {
            // if coarse X == 31
            self.address &= !0x001F; // coarse X = 0
            self.address ^= 0x0400; // switch horizontal nametable
        } else {
            self.address += 1 // increment coarse X
        }
    }

    fn y_increment(&mut self) {
        if !self.rendering_enabled() {
            return;
        }

        if (self.address & 0x7000) != 0x7000 {
            // if fine Y < 7
            self.address += 0x1000 // increment fine Y
        } else {
            self.address &= !0x7000; // fine Y = 0
            let mut y = (self.address & 0x03E0) >> 5; // let y = coarse Y

            if y == 29 {
                y = 0; // coarse Y = 0
                self.address ^= 0x0800; // switch vertical nametable
            } else if y == 31 {
                y = 0; // coarse Y = 0, nametable not switched
            } else {
                y += 1; // increment coarse Y
            }
            self.address = (self.address & !0x03E0) | (y << 5); // put coarse Y back into v
        }
    }

    pub fn dump(&self) {
        println!("Cycle: {}, Scanline: {}", self.current_cycle, self.scanline);
    }

    fn ntsc_color(&mut self, color_index: u8) -> Color {
        let pi = self.cycle_counter as usize * 64 * 512
            + (self.prev_pixel % 64) as usize * 512
            + color_index as usize;
        let ntsc_color = self.ntsc_palette[pi];
        let r = ((ntsc_color >> 16) & 0xff) as u8;
        let g = ((ntsc_color >> 8) & 0xff) as u8;
        let b = ((ntsc_color >> 0) & 0xff) as u8;

        self.prev_pixel = color_index;
        Color::rgb(r, g, b)
    }

    // Adapted from bisqwit NTSC filter here:
    // https://bisqwit.iki.fi/jutut/kuvat/programming_examples/nesemu1/nesemu1.cc
    pub fn build_ntsc_palette(&mut self) {
        // The input value is a NES color index (with de-emphasis bits).
        // We need RGB values. To produce a RGB value, we emulate the NTSC circuitry.
        // For most part, this process is described at:
        //    http://wiki.nesdev.com/w/index.php/NTSC_video

        let s = [
            -6, -69, 26, -59, 29, -55, 73, -40, 68, -17, 125, 11, 68, 33, 125, 78,
        ];

        for o in 0..3 {
            for u in 0..3 {
                for p0 in 0..512 {
                    for p1 in 0..64 {
                        let mut y = 0;
                        let mut i = 0;
                        let mut q = 0;
                        for p in 0..12 {
                            // 12 samples of NTSC signal constitute a color.
                            // Sample either the previous or the current pixel.
                            let r = (p + o * 4) % 12;
                            // let pixel = p0;
                            let pixel = if r < 8 - u * 2 { p0 } else { p1 }; // Use pixel=p0 to disable artifacts.

                            // Decode the color index.
                            let c = pixel % 16;
                            let l = if c < 0xE { pixel / 4 & 12 } else { 4 };
                            let e = p0 / 64;

                            let cond1 = (c + 8 + p) % 12 < 6;
                            let cond2 = 152278 >> p / 2 * 3 & e == 0;
                            let cond3 = c > if cond1 { 12 } else { 0 };

                            let index = if cond3 { 1 } else { 0 } + if cond2 { 2 } else { 0 } + l;

                            // NES NTSC modulator (square wave between up to four voltage levels):
                            let b = 40 + s[index];

                            // Ideal TV NTSC demodulator:
                            let (sin, cos) = (std::f32::consts::PI * p as f32 / 6.0).sin_cos();

                            y += b;
                            i += b * (cos * 5909.0) as i32;
                            q += b * (sin * 5909.0) as i32;
                        }

                        // Convert the YIQ color into RGB
                        let gammafix = |f: f32| {
                            if f <= 0.0 {
                                0.0
                            } else {
                                f.powf(2.2 / 1.8)
                            }
                        };

                        let clamp = |v| {
                            if v > 255.0 {
                                255
                            } else {
                                v as u32
                            }
                        };

                        let pi = o * 64 * 512 + p1 * 512 + p0;

                        // Store color at subpixel precision
                        self.ntsc_palette[pi] += match u {
                            2 => {
                                let v = y as f32 / 1980.0
                                    + i as f32 * 0.947 / 9e6
                                    + q as f32 * 0.624 / 9e6;
                                0x10000 * clamp(255.0 * gammafix(v))
                            }

                            1 => {
                                let v = y as f32 / 1980.0
                                    + i as f32 * -0.275 / 9e6
                                    + q as f32 * -0.636 / 9e6;
                                0x00100 * clamp(255.0 * gammafix(v))
                            }

                            0 => {
                                let v = y as f32 / 1980.0
                                    + i as f32 * -1.109 / 9e6
                                    + q as f32 * 1.709 / 9e6;
                                0x00001 * clamp(255.0 * gammafix(v))
                            }
                            _ => panic!("NTSC Invalid value"),
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mapper::dummy::DummyROM;

    #[test]
    fn large_sprite_address() {
        assert_eq!(0x0000, Ppu::address_large_tile(0));
        assert_eq!(0x1000, Ppu::address_large_tile(1));
        assert_eq!(0x0020, Ppu::address_large_tile(2));
        assert_eq!(0x1020, Ppu::address_large_tile(3));
    }

    #[test]
    fn palette_mirroring() {
        let mut ppu = Ppu::new();
        ppu.write_memory(0x3F00, 100);
        assert_eq!(ppu.actual_vram_address(0x3F00), 0x3F00);
        assert_eq!(ppu.actual_vram_address(0x3F10), 0x3F00);
        assert_eq!(ppu.read_memory(0x3F00), 100);
        assert_eq!(ppu.read_memory(0x3F10), 100);
        ppu.write_memory(0x3F10, 200);
        assert_eq!(ppu.read_memory(0x3F00), 200);
        assert_eq!(ppu.read_memory(0x3F10), 200);
    }

    #[test]
    fn horizontal_mirroring() {
        let mut ppu = Ppu::new();

        let mut rom = DummyROM::new();
        rom.mirroring = Mirroring::Horizontal;

        ppu.rom = Some(rom);

        ppu.write_memory(0x2000, 100);
        assert_eq!(ppu.actual_vram_address(0x2000), 0x2000);
        assert_eq!(ppu.actual_vram_address(0x2400), 0x2000);
        assert_eq!(ppu.read_memory(0x2000), 100);
        assert_eq!(ppu.read_memory(0x2400), 100);
        ppu.write_memory(0x2400, 200);
        assert_eq!(ppu.read_memory(0x2000), 200);
        assert_eq!(ppu.read_memory(0x2400), 200);
    }

    #[test]
    fn vertical_mirroring() {
        let mut ppu = Ppu::new();

        let mut rom = DummyROM::new();
        rom.mirroring = Mirroring::Vertical;

        ppu.rom = Some(rom);

        ppu.write_memory(0x2000, 100);
        ppu.write_memory(0x2400, 128);

        assert_eq!(ppu.actual_vram_address(0x2000), 0x2000);
        assert_eq!(ppu.actual_vram_address(0x2800), 0x2000);
        assert_eq!(ppu.actual_vram_address(0x2400), 0x2400);
        assert_eq!(ppu.actual_vram_address(0x2C00), 0x2400);

        assert_eq!(ppu.read_memory(0x2000), 100);
        assert_eq!(ppu.read_memory(0x2800), 100);
        assert_eq!(ppu.read_memory(0x2400), 128);
        assert_eq!(ppu.read_memory(0x2C00), 128);

        ppu.write_memory(0x2800, 200);
        ppu.write_memory(0x2C00, 255);

        assert_eq!(ppu.read_memory(0x2000), 200);
        assert_eq!(ppu.read_memory(0x2800), 200);
        assert_eq!(ppu.read_memory(0x2400), 255);
        assert_eq!(ppu.read_memory(0x2C00), 255);
    }

    #[test]
    fn x_increment_wraparound() {
        let mut ppu = Ppu::new();
        assert_eq!(ppu.address, 0);
        ppu.coarse_x_increment();
        assert_eq!(ppu.address, 1);

        ppu.address = 31;
        ppu.coarse_x_increment();
        assert_eq!(ppu.address, 0x400);
    }

    #[test]
    fn scroll_offset() {
        let mut ppu = Ppu::new();
        assert_eq!(ppu.address, 0);

        ppu.write_scroll(8);
        ppu.write_scroll(0);
        ppu.horizontal_t2v();
        ppu.vertical_t2v();

        assert_eq!(ppu.address, 1);
        for _ in 0..31 {
            ppu.coarse_x_increment();
        }
        assert_eq!(ppu.address, 0x400);

        ppu.write_scroll(255);
        ppu.write_scroll(0);
        ppu.horizontal_t2v();
        ppu.vertical_t2v();

        assert_eq!(ppu.address, 0x1F);
        for _ in 0..31 {
            ppu.coarse_x_increment();
        }
        assert_eq!(ppu.address, 0x41E);
    }

    #[test]
    fn first_tiles() {
        let mut ppu = Ppu::new();
        ppu.write_memory(0x0010, 0xDE);
        ppu.write_memory(0x0020, 0xAD);
        ppu.write_memory(0x0018, 0xBE);
        ppu.write_memory(0x0028, 0xEF);
        ppu.write_memory(0x2000, 0x01);
        ppu.write_memory(0x2001, 0x02);

        assert_eq!(ppu.address, 0);
        assert_eq!(ppu.scanline, 261);
        assert_eq!(ppu.current_cycle, 0);

        for _ in 0..341 {
            ppu.cycle();
        }

        assert_eq!(ppu.scanline, 0);
        assert_eq!(ppu.current_cycle, 0);
        assert_eq!(ppu.address, 2);
        assert_eq!(ppu.bg_tile_low, 0xDEAD);
        assert_eq!(ppu.bg_tile_high, 0xBEEF);
    }

    #[test]
    fn full_nametable_change() {
        let mut ppu = Ppu::new();
        ppu.write_ppuctrl(0x01);
        ppu.write_scroll(0);
        ppu.write_scroll(0);
        ppu.horizontal_t2v();
        ppu.vertical_t2v();
        assert_eq!(ppu.address, 0x400);
    }

    #[test]
    fn scroll_clear_works() {
        let mut ppu = Ppu::new();
        ppu.temp_address = 0x041F;
        ppu.write_scroll(0);
        ppu.write_scroll(0);
        assert_eq!(0x0400, ppu.temp_address);
    }

    #[test]
    fn reading_ppustatus_clears_vblank() {
        let mut ppu = Ppu::new();
        ppu.vblank();
        assert_eq!(true, ppu.nmi_occurred);
        assert_eq!(0x80, ppu.read(0x2002));
        assert_eq!(0x00, ppu.read(0x2002));
    }

    #[test]
    fn frame_timing() {
        const EVEN_FRAME_TIME: usize = 262 * 341;
        const ODD_FRAME_TIME: usize = EVEN_FRAME_TIME - 1;

        let mut ppu = Ppu::new();
        // Clear VBLANK
        ppu.read(0x2002);
        assert_eq!(0, ppu.read(0x2002));

        // Wait for VBLANK
        while ppu.read(0x2002) & 0x80 == 0 {
            ppu.cycle();
        }

        let mut count = 0;

        // Wait for next VBLANK
        while ppu.read(0x2002) & 0x80 == 0 {
            ppu.cycle();
            count += 1;
        }
        assert_eq!(ODD_FRAME_TIME, count);

        count = 0;

        // Wait for next VBLANK
        while ppu.read(0x2002) & 0x80 == 0 {
            ppu.cycle();
            count += 1;
        }
        assert_eq!(EVEN_FRAME_TIME, count);
    }

    #[test]
    fn ntsc_palette() {
        let mut ppu = Ppu::new();
        ppu.build_ntsc_palette();
        assert_eq!(ppu.ntsc_palette[0], 0x5460819);
        assert_eq!(ppu.ntsc_palette[1], 0x1001527);
        assert_eq!(ppu.ntsc_palette[2], 0x2312526);
        assert_eq!(ppu.ntsc_palette[3], 0x3886155);
        assert_eq!(ppu.ntsc_palette[4], 0x5460782);
        assert_eq!(ppu.ntsc_palette[5], 0x5458191);
        assert_eq!(ppu.ntsc_palette[6], 0x5456896);
        assert_eq!(ppu.ntsc_palette[7], 0x4145664);
        assert_eq!(ppu.ntsc_palette[8], 0x2638080);
        assert_eq!(ppu.ntsc_palette[9], 0x1195520);
    }
}

use std::cell::Cell;

pub struct Ppu {
    oam: [u8; 0x100],
    oam_addr: u8,
    vblank: Cell<bool>,
    sprite_zero: bool,
    sprite_overflow: bool,
    vram: [u8; 0x4000],
    address: u16,
    low_address: bool,
    scroll_x: u8,
    scroll_y: u8,
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
        }
    }
    
    
    pub fn read(&self, address: u16) -> u8 {
        println!("PPUWREAD: {:X}", address);
        match address {
            0x2002 => self.read_status(),
            _ => panic!("Unimplemented: {:X}", address),
        }
    }
    
    pub fn write(&mut self, address: u16, byte: u8) {
        println!("PPUWRITE: {:X} {:X}", address, byte);
        match address {
            0x2000 => panic!("Unimplemented: {:X} = {:X}", address, byte),
            0x2003 => self.oam_addr = byte,
            0x2004 => self.write_oamdata(byte),
            0x2005 => self.write_scroll(byte),
            0x2006 => self.write_address(byte),
            0x2007 => {
                self.write_memory(self.address, byte);
                self.address.wrapping_add(1);
            },
            _ => panic!("Unimplemented: {:X}", address),
        }
    }

    fn write_oamdata(&mut self, byte: u8) {
        self.oam[self.oam_addr as usize] = byte;
        self.oam_addr.wrapping_add(1);
    }

    fn write_address(&mut self, byte: u8) {
        println!("PPUADDR: {:X}", byte);
        if self.low_address {
            self.address = byte as u16;
            self.low_address = false;
        } else {
            self.address |= ((byte as u16) << 8);
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
            _ => panic!("Unimplemented PPU address: {:X}", address),
        }
    }

    fn write_memory(&mut self, address: u16, byte: u8) {
        match address {
            _ => panic!("Unimplemented PPU address: {:X}", address),
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
}





// PPUCTRL 	$2000 	VPHB SINN 	NMI enable (V), PPU master/slave (P), sprite height (H), background tile select (B), sprite tile select (S), increment mode (I), nametable select (NN)
// PPUMASK 	$2001 	BGRs bMmG 	color emphasis (BGR), sprite enable (s), background enable (b), sprite left column enable (M), background left column enable (m), greyscale (G)
// PPUSTATUS 	$2002 	VSO- ---- 	vblank (V), sprite 0 hit (S), sprite overflow (O); read resets write pair for $2005/$2006
// OAMADDR 	$2003 	aaaa aaaa 	OAM read/write address
// OAMDATA 	$2004 	dddd dddd 	OAM data read/write
// PPUSCROLL 	$2005 	xxxx xxxx 	fine scroll position (two writes: X scroll, Y scroll)
// PPUADDR 	$2006 	aaaa aaaa 	PPU read/write address (two writes: most significant byte, least significant byte)
// PPUDATA 	$2007 	dddd dddd 	PPU data read/write
// OAMDMA 	$4014 	aaaa aaaa 	OAM DMA high address 

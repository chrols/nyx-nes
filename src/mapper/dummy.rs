use super::Cartridge;
use crate::ines::File;
use crate::ines::Mirroring;

pub struct DummyROM {
    vram: [u8; 0x2000],
    rom: [u8; 0x4000],
}

impl DummyROM {
    pub fn new() -> Box<DummyROM> {
        Box::new(DummyROM {
            vram: [0; 0x2000],
            rom: [0; 0x4000],
        })
    }
}

impl Cartridge for DummyROM {
    fn read(&mut self, address: u16) -> u8 {
        self.rom[address as usize % 0x4000]
    }

    fn write(&mut self, address: u16, byte: u8) {
        self.rom[address as usize % 0x4000] = byte
    }

    fn ppu_read(&mut self, address: u16) -> u8 {
        self.vram[address as usize % 0x2000]
    }

    fn ppu_write(&mut self, address: u16, byte: u8) {
        self.vram[address as usize % 0x2000] = byte
    }

    fn mirroring(&self) -> Mirroring {
        Mirroring::Horizontal
    }
}

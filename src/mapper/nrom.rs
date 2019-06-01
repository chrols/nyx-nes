use crate::ines::File;
use super::Cartridge;

pub struct NROM {
    pub file: File,
}

impl Cartridge for NROM {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4020...0xFFFF => self.file.prg_rom[(address as usize - if self.file.prg_rom_blocks == 1 { 0xC000} else { 0x8000 } as usize)],
            _ => panic!("Unimplemented mapper"),
        }
    }

    fn write(&mut self, address: u16, byte: u8) {
        panic!("Attempt to write to NROM!");
    }

    fn ppu_read(&mut self, address: u16) -> u8 {
        match address {
            0x0000...0x1FFF => self.file.chr_rom[address as usize],
            _ => panic!("PPU read outside of range"),
        }
    }

    fn ppu_write(&mut self, address: u16, byte: u8) {
        panic!("Attempt to write to NROM!");
    }

}

use crate::ines::File;
use super::Cartridge;

pub struct MMC1 {
    chr_ram: [u8; 0x2000],
    pub file: File,
}


impl MMC1 {
    pub fn new(file: File) -> MMC1 {
        MMC1 {
            file,
            chr_ram: [0; 0x2000],
        }
    }
}

impl Cartridge for MMC1 {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4020...0xFFFF => self.file.prg_rom[(address as usize - if self.file.prg_rom_blocks == 1 { 0xC000} else { 0x8000 } as usize)],
            _ => panic!("Unimplemented mapper"),
        }
    }

    fn write(&mut self, address: u16, byte: u8) {
        println!("{:X} = {:X}", address, byte);
    }

    fn ppu_read(&mut self, address: u16) -> u8 {
        match address {
            0x0000...0x1FFF => self.chr_ram[address as usize],
            _ => panic!("PPU read outside of range"),
        }
    }

    fn ppu_write(&mut self, address: u16, byte: u8) {
        match address {
            0x0000...0x1FFF => self.chr_ram[address as usize] = byte,
            _ => panic!("PPU read outside of range"),
        }

    }
}

use super::Cartridge;
use crate::ines::File;
use crate::ines::Mirroring;

pub struct NROM {
    pub file: File,
}

impl Cartridge for NROM {
    fn read(&mut self, address: u16) -> u8 {
        let mut ram_address = address - 0x8000;
        if self.file.prg_rom_blocks == 1 {
            ram_address %= 0x4000;
        }

        match address {
            0x4020...0xFFFF => self.file.prg_rom[ram_address as usize],
            _ => panic!("Read outside scope: {:04X}", address),
        }
    }

    fn write(&mut self, address: u16, byte: u8) {
        println!("Attempt to write to NROM: {:04X} = {:02X}", address, byte);
    }

    fn ppu_read(&mut self, address: u16) -> u8 {
        match address {
            0x0000...0x1FFF => self.file.chr_rom[address as usize],
            _ => panic!("PPU read outside of range"),
        }
    }

    fn ppu_write(&mut self, address: u16, byte: u8) {
        println!("Attempt to write to NROM: {:04X} = {:02X}", address, byte);
    }

    fn mirroring(&self) -> Mirroring {
        self.file.mirroring
    }
}

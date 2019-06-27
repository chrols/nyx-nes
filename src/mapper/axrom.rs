use super::Cartridge;
use crate::ines::File;
use crate::ines::Mirroring;

pub struct AxROM {
    pub file: File,
    chr_ram: [u8; 0x2000],
    bank: u8,
}

impl AxROM {
    pub fn new(file: File) -> AxROM {
        AxROM {
            file,
            chr_ram: [0; 0x2000],
            bank: 0,
        }
    }
}

impl Cartridge for AxROM {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4020...0x7FFF => 0,
            0x8000...0xFFFF => {
                self.file.prg_rom[address as usize - 0x8000 + self.bank as usize * 0x8000]
            }
            _ => panic!("Read outside scope: {:04X}", address),
        }
    }

    fn write(&mut self, address: u16, byte: u8) {
        match address {
            0x8000...0xFFFF => self.bank = 0x7 & byte,
            _ => panic!(
                "AxROM write outside of range: {:04X} = {:02X}",
                address, byte
            ),
        }
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
            _ => println!(
                "Attempt to PPU write to AxROM: {:04X} = {:02X}",
                address, byte
            ),
        }
    }

    fn mirroring(&self) -> Mirroring {
        self.file.mirroring
    }
}

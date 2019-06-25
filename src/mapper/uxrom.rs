use crate::ines::File;
use crate::ines::Mirroring;
use super::Cartridge;

pub struct UxROM {
    pub file: File,
    active_bank: u8,
    chr_ram: [u8; 0x2000],
    sram: [u8; 0x2000],
}


impl UxROM {
    pub fn new(file: File) -> UxROM {
        UxROM {
            file,
            active_bank: 0,
            chr_ram: [0; 0x2000],
            sram: [0; 0x2000],
        }
    }
}

// CPU $8000-$BFFF: 16 KB switchable PRG ROM bank
// CPU $C000-$FFFF: 16 KB PRG ROM bank, fixed to the last bank
impl Cartridge for UxROM {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x6000...0x7FFF => self.sram[address as usize - 0x6000],
            0x8000...0xBFFF => self.file.prg_rom[address as usize - 0x8000 + (self.active_bank as usize) * 0x4000],
            0xC000...0xFFFF => self.file.prg_rom[address as usize - 0xC000 + (self.file.prg_rom_blocks as usize - 1) * 0x4000],
            _ => panic!("Read outside scope: {:04X}", address),
        }
    }

    fn write(&mut self, address: u16, byte: u8) {
        match address {
            0x6000...0x7FFF => self.sram[address as usize - 0x6000] = byte,
            0x8000...0xFFFF => {
        let bank = byte & 0x0F;
        assert!(bank <= self.file.prg_rom_blocks);
        self.active_bank = bank;
            },
        _ => panic!("Write outside scope: {:04X}", address),
        }

    }

    fn ppu_read(&mut self, address: u16) -> u8 {
        match address {
            0x0000...0x1FFF => self.chr_ram[address as usize],
            _ => panic!("PPU read outside of range: {:04X}", address),
        }
    }

    fn ppu_write(&mut self, address: u16, byte: u8) {
        match address {
            0x0000...0x1FFF => self.chr_ram[address as usize] = byte,
            _ => panic!("PPU read outside of range: {:04X}", address),
        }

    }

    fn mirroring(&self) -> Mirroring {
        self.file.mirroring
    }
}

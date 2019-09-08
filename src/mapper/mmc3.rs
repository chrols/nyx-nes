use super::Cartridge;
use crate::ines::File;
use crate::ines::Mirroring;

pub struct MMC3 {
    pub file: File,
    mirroring: Mirroring,
    chr_inversion: bool,
    prg_bank_mode: bool,
    prg_offset: [usize; 4],
    chr_offset: [usize; 8],
    bank_register: [usize; 8],
    bank_select: usize,
}

impl MMC3 {
    pub fn new(file: File) -> MMC3 {
        let prg_last_block = (file.prg_rom_blocks as usize * 2 - 1) * 0x2000;
        let prg_next_to_last = prg_last_block - 0x2000;
        MMC3 {
            mirroring: file.mirroring,
            file,
            chr_inversion: false,
            prg_bank_mode: false,
            prg_offset: [0, 0, prg_next_to_last, prg_last_block],
            chr_offset: [0, 1, 2, 3, 4, 5, 6, 7],
            bank_register: [0; 8],
            bank_select: 0,
        }
    }

    // 7  bit  0
    // ---- ----
    // CPMx xRRR
    // |||   |||
    // |||   +++- Specify which bank register to update on next write to Bank Data register
    // |||        0: Select 2 KB CHR bank at PPU $0000-$07FF (or $1000-$17FF);
    // |||        1: Select 2 KB CHR bank at PPU $0800-$0FFF (or $1800-$1FFF);
    // |||        2: Select 1 KB CHR bank at PPU $1000-$13FF (or $0000-$03FF);
    // |||        3: Select 1 KB CHR bank at PPU $1400-$17FF (or $0400-$07FF);
    // |||        4: Select 1 KB CHR bank at PPU $1800-$1BFF (or $0800-$0BFF);
    // |||        5: Select 1 KB CHR bank at PPU $1C00-$1FFF (or $0C00-$0FFF);
    // |||        6: Select 8 KB PRG ROM bank at $8000-$9FFF (or $C000-$DFFF);
    // |||        7: Select 8 KB PRG ROM bank at $A000-$BFFF
    // ||+------- Nothing on the MMC3, see MMC6
    // |+-------- PRG ROM bank mode (0: $8000-$9FFF swappable,
    // |                                $C000-$DFFF fixed to second-last bank;
    // |                             1: $C000-$DFFF swappable,
    // |                                $8000-$9FFF fixed to second-last bank)
    // +--------- CHR A12 inversion (0: two 2 KB banks at $0000-$0FFF,
    //                                  four 1 KB banks at $1000-$1FFF;
    //                               1: two 2 KB banks at $1000-$1FFF,
    //                                  four 1 KB banks at $0000-$0FFF)
    fn bank_select(&mut self, byte: u8) {
        self.chr_inversion = (byte & 0x80) != 0;
        self.prg_bank_mode = (byte & 0x40) != 0;
        self.bank_select = (byte & 7) as usize;
        self.update_offsets();
    }

    fn bank_data(&mut self, byte: u8) {
        let new_bank = byte as usize;
        self.bank_register[self.bank_select] = if self.bank_select < 2 {
            new_bank & 0xFE
        } else if self.bank_select > 5 {
            new_bank & 0x3F
        } else {
            new_bank
        };

        self.update_offsets();
    }

    fn set_mirroring(&mut self, byte: u8) {
        self.mirroring = if (byte & 1) == 1 {
            Mirroring::Horizontal
        } else {
            Mirroring::Vertical
        };
    }

    fn prg_ram_protect(&mut self, byte: u8) {}

    fn irq_latch(&mut self, byte: u8) {}

    fn irq_reload(&mut self) {}

    fn irq_enable(&mut self) {}

    fn irq_disable(&mut self) {}

    fn update_offsets(&mut self) {
        let prg_last_block = self.file.prg_rom_blocks as usize * 2 - 1;
        let prg_next_to_last = prg_last_block - 1;

        if self.prg_bank_mode {
            self.prg_offset[0] = prg_next_to_last * 0x2000;
            self.prg_offset[1] = self.bank_register[7] * 0x2000;
            self.prg_offset[2] = self.bank_register[6] * 0x2000;
            self.prg_offset[3] = prg_last_block * 0x2000;
        } else {
            self.prg_offset[0] = self.bank_register[6] * 0x2000;
            self.prg_offset[1] = self.bank_register[7] * 0x2000;
            self.prg_offset[2] = prg_next_to_last * 0x2000;
            self.prg_offset[3] = prg_last_block * 0x2000;
        }

        if self.chr_inversion {
            self.chr_offset[0] = self.bank_register[2] * 0x400;
            self.chr_offset[1] = self.bank_register[3] * 0x400;
            self.chr_offset[2] = self.bank_register[4] * 0x400;
            self.chr_offset[3] = self.bank_register[5] * 0x400;

            self.chr_offset[4] = self.bank_register[0] * 0x400;
            self.chr_offset[5] = (self.bank_register[0] + 1) * 0x400;
            self.chr_offset[6] = self.bank_register[1] * 0x400;
            self.chr_offset[7] = (self.bank_register[1] + 1) * 0x400;
        } else {
            self.chr_offset[0] = self.bank_register[0] * 0x400;
            self.chr_offset[1] = (self.bank_register[0] + 1) * 0x400;
            self.chr_offset[2] = self.bank_register[1] * 0x400;
            self.chr_offset[3] = (self.bank_register[1] + 1) * 0x400;

            self.chr_offset[4] = self.bank_register[2] * 0x400;
            self.chr_offset[5] = self.bank_register[3] * 0x400;
            self.chr_offset[6] = self.bank_register[4] * 0x400;
            self.chr_offset[7] = self.bank_register[5] * 0x400;
        }
    }
}

impl Cartridge for MMC3 {
    fn read(&mut self, address: u16) -> u8 {
        let prg_addr = match address {
            0x8000...0x9FFF => self.prg_offset[0],
            0xA000...0xBFFF => self.prg_offset[1],
            0xC000...0xDFFF => self.prg_offset[2],
            0xE000...0xFFFF => self.prg_offset[3],
            _ => {
                println!("Invalid MMC3 read {:X}", address);
                0
            }
        };

        let offset = (address % 0x2000) as usize;
        self.file.prg_rom[prg_addr + offset]
    }

    fn write(&mut self, address: u16, byte: u8) {
        match address {
            0x8000...0x9FFF if address % 2 == 0 => self.bank_select(byte),
            0x8000...0x9FFF => self.bank_data(byte),
            0xA000...0xBFFF if address % 2 == 0 => self.set_mirroring(byte),
            0xA000...0xBFFF => self.prg_ram_protect(byte),
            0xC000...0xDFFF if address % 2 == 0 => self.irq_latch(byte),
            0xC000...0xDFFF => self.irq_reload(),
            0xE000...0xFFFF if address % 2 == 0 => self.irq_disable(),
            0xE000...0xFFFF => self.irq_enable(),
            _ => println!("Invalid MMC3 write {:X} = {:X}", address, byte),
        }
    }

    fn ppu_read(&mut self, address: u16) -> u8 {
        let chr_addr = match address {
            0x0000...0x03FF => self.chr_offset[0],
            0x0400...0x07FF => self.chr_offset[1],
            0x0800...0x0BFF => self.chr_offset[2],
            0x0C00...0x0FFF => self.chr_offset[3],
            0x1000...0x13FF => self.chr_offset[4],
            0x1400...0x17FF => self.chr_offset[5],
            0x1800...0x1BFF => self.chr_offset[6],
            0x1C00...0x1FFF => self.chr_offset[7],
            _ => panic!("Invalid MMC3 read {:X}", address),
        };

        let offset = (address % 0x400) as usize;

        self.file.chr_rom[chr_addr + offset]
    }

    fn ppu_write(&mut self, address: u16, byte: u8) {
        println!("PPU WRITE MMC3 {:X} = {:X}", address, byte);
    }

    fn mirroring(&self) -> Mirroring {
        self.mirroring
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_resets() {}
}

use super::Cartridge;
use crate::ines::File;
use crate::ines::Mirroring;

use serde::{Deserialize, Serialize};

use std::mem;

#[derive(Serialize, Deserialize)]
pub struct MMC3 {
    #[serde(skip)]
    pub file: File,
    mirroring: Mirroring,
    chr_inversion: bool,
    prg_ram: Vec<u8>,
    prg_bank_mode: bool,
    prg_offset: [usize; 4],
    chr_offset: [usize; 8],
    bank_register: [usize; 8],
    bank_select: usize,
    irq_counter: u8,
    irq_counter_reload: u8,
    irq_enabled: bool,
    irq_triggered: bool,
}

impl MMC3 {
    pub fn new(file: File) -> MMC3 {
        let prg_last_block = (file.prg_rom_blocks as usize * 2 - 1) * 0x2000;
        let prg_next_to_last = prg_last_block - 0x2000;
        let mut mmc3 = MMC3 {
            mirroring: file.mirroring,
            file,
            chr_inversion: false,
            prg_ram: vec![0; 0x2000],
            prg_bank_mode: false,
            prg_offset: [0, 1, prg_next_to_last, prg_last_block],
            chr_offset: [0, 0, 0, 0, 0, 0, 0, 0],
            bank_register: [0; 8],
            bank_select: 0,
            irq_counter: 0,
            irq_counter_reload: 0,
            irq_enabled: false,
            irq_triggered: false,
        };
        mmc3.update_offsets();
        mmc3
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

    fn prg_ram_protect(&mut self, byte: u8) {
        if byte != 0 {
            println!("Ram protection: {:X}", byte);
        }
    }

    fn irq_latch(&mut self, byte: u8) {
        self.irq_counter_reload = byte;
    }

    fn irq_reload(&mut self) {
        // FIXME Reload should occur at next "rising edge"
        self.irq_counter = self.irq_counter_reload;
    }

    fn irq_enable(&mut self) {
        self.irq_enabled = true;
    }

    fn irq_disable(&mut self) {
        self.irq_enabled = false;
        self.irq_triggered = false;
    }

    fn update_offsets(&mut self) {
        self.update_prg_offsets();
        self.update_chr_offsets();
    }

    fn update_prg_offsets(&mut self) {
        // File blocks are twice the size of MMC3 blocks (16 KiB vs 8 KiB)
        let blocks = self.file.prg_rom_blocks as usize * 2;
        let prg_last_block = blocks - 1;
        let prg_next_to_last = prg_last_block - 1;

        let li = prg_last_block * 0x2000;
        let nli = prg_next_to_last * 0x2000;

        let r6_i = self.bank_register[6] % blocks * 0x2000;
        let r7_i = self.bank_register[7] % blocks * 0x2000;

        if self.prg_bank_mode {
            self.prg_offset[0] = nli;
            self.prg_offset[1] = r7_i;
            self.prg_offset[2] = r6_i;
            self.prg_offset[3] = li;
        } else {
            self.prg_offset[0] = r6_i;
            self.prg_offset[1] = r7_i;
            self.prg_offset[2] = nli;
            self.prg_offset[3] = li;
        }
    }

    fn update_chr_offsets(&mut self) {
        // File blocks are eight times the size of the MMC3 blocks ( 8 KiB vs 1 KiB)
        let blocks = self.file.chr_rom_blocks as usize * 8;

        let r0_li = (self.bank_register[0] & 0xFE) % blocks * 0x400;
        let r0_hi = (self.bank_register[0] | 0x01) % blocks * 0x400;
        let r1_li = (self.bank_register[1] & 0xFE) % blocks * 0x400;
        let r1_hi = (self.bank_register[1] | 0x01) % blocks * 0x400;

        let r2_i = self.bank_register[2] % blocks * 0x400;
        let r3_i = self.bank_register[3] % blocks * 0x400;
        let r4_i = self.bank_register[4] % blocks * 0x400;
        let r5_i = self.bank_register[5] % blocks * 0x400;

        if self.chr_inversion {
            self.chr_offset[0] = r2_i;
            self.chr_offset[1] = r3_i;
            self.chr_offset[2] = r4_i;
            self.chr_offset[3] = r5_i;

            self.chr_offset[4] = r0_li;
            self.chr_offset[5] = r0_hi;
            self.chr_offset[6] = r1_li;
            self.chr_offset[7] = r1_hi;
        } else {
            self.chr_offset[0] = r0_li;
            self.chr_offset[1] = r0_hi;
            self.chr_offset[2] = r1_li;
            self.chr_offset[3] = r1_hi;

            self.chr_offset[4] = r2_i;
            self.chr_offset[5] = r3_i;
            self.chr_offset[6] = r4_i;
            self.chr_offset[7] = r5_i;
        }
    }
}

impl Cartridge for MMC3 {
    fn read(&mut self, address: u16) -> u8 {
        if address >= 0x6000 && address <= 0x7FFF {
            return self.prg_ram[address as usize - 0x6000];
        }

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
            0x6000...0x7FFF => self.prg_ram[address as usize - 0x6000] = byte,
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

    fn irq(&mut self) -> bool {
        if self.irq_enabled && self.irq_triggered {
            self.irq_triggered = false;
            true
        } else {
            false
        }
    }

    fn on_scanline(&mut self) {
        if self.irq_counter == 0 {
            self.irq_counter = self.irq_counter_reload;
        } else {
            self.irq_counter -= 1;

            if self.irq_counter == 0 && self.irq_enabled {
                self.irq_triggered = true;
            }
        }
    }

    fn to_json(&self) -> String {
        serde_json::to_string(self).unwrap()
    }

    fn from_json(&mut self, json: &str) {
        let mut rom: MMC3 = serde_json::from_str(json).unwrap();
        mem::swap(&mut self.mirroring, &mut rom.mirroring);
        mem::swap(&mut self.chr_inversion, &mut rom.chr_inversion);
        mem::swap(&mut self.prg_ram, &mut rom.prg_ram);
        mem::swap(&mut self.prg_bank_mode, &mut rom.prg_bank_mode);
        mem::swap(&mut self.prg_offset, &mut rom.prg_offset);
        mem::swap(&mut self.chr_offset, &mut rom.chr_offset);
        mem::swap(&mut self.bank_register, &mut rom.bank_register);
        mem::swap(&mut self.bank_select, &mut rom.bank_select);
        mem::swap(&mut self.irq_counter, &mut rom.irq_counter);
        mem::swap(&mut self.irq_counter_reload, &mut rom.irq_counter_reload);
        mem::swap(&mut self.irq_enabled, &mut rom.irq_enabled);
        mem::swap(&mut self.irq_triggered, &mut rom.irq_triggered);
    }
}

#[cfg(test)]
mod tests {
    // use super::*;

    #[test]
    fn register_resets() {
        assert!(false);
    }
}

use super::Cartridge;
use crate::ines::File;
use crate::ines::Mirroring;
use std::mem;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct NROM {
    #[serde(skip)]
    pub file: File,
    sram: Vec<u8>,
    chr_ram: Vec<u8>,
}

impl NROM {
    pub fn new(file: File) -> NROM {
        NROM {
            file,
            sram: vec![0; 0x2000],
            chr_ram: vec![0; 0x2000],
        }
    }
}

impl Cartridge for NROM {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4020...0x5FFF => panic!("Read in unused ROM region? {:X}", address),
            0x6000...0x7FFF => self.file.prg_rom[address as usize - 0x6000],
            0x8000...0xFFFF => {
                self.file.prg_rom[(address as usize - 0x8000)
                    % if self.file.prg_rom_blocks == 1 {
                        0x4000
                    } else {
                        0x8000
                    }]
            }
            _ => panic!("NROM Read outside ROM region: {:X}", address),
        }
    }

    fn write(&mut self, address: u16, byte: u8) {
        match address {
            0x6000...0x7FFF => self.sram[(address as usize - 0x6000) % 0x2000] = byte,
            _ => panic!("Attempt to write to NROM: {:04X} = {:02X}", address, byte),
        }
    }

    fn ppu_read(&mut self, address: u16) -> u8 {
        if self.file.chr_rom_blocks == 0 {
            self.chr_ram[address as usize]
        } else {
            match address {
                0x0000...0x1FFF => self.file.chr_rom[address as usize],
                _ => panic!("PPU read outside of range"),
            }
        }
    }

    fn ppu_write(&mut self, address: u16, byte: u8) {
        if self.file.chr_rom_blocks == 0 {
            self.chr_ram[address as usize] = byte;
        } else {
            println!(
                "PPU: Attempt to write to NROM: {:04X} = {:02X}",
                address, byte
            );
        }
    }

    fn mirroring(&self) -> Mirroring {
        self.file.mirroring
    }

    fn to_json(&self) -> String {
        serde_json::to_string(self).unwrap()
    }

    fn from_json(&mut self, json: &str) {
        let mut nrom: NROM = serde_json::from_str(json).unwrap();
        mem::swap(&mut self.sram, &mut nrom.sram);
        mem::swap(&mut self.chr_ram, &mut nrom.chr_ram);
    }
}

use super::Cartridge;
use crate::ines::File;
use crate::ines::Mirroring;

enum PrgRomMode {
    SingleBig,
    FixedFirst,
    FixedLast,
}

pub struct MMC1 {
    pub file: File,
    sram: [u8; 0x2000],
    shift_times: u8,
    shift_register: u8,
    prg_rom_mode: PrgRomMode,
    single_chr_rom: bool,
    chr_ram: [u8; 0x2000],
    chr_bank_0: u8,
    chr_bank_1: u8,
    prg_bank: u8,
    control: u8,
    mirroring: Mirroring,
}

impl MMC1 {
    pub fn new(file: File) -> MMC1 {
        println!("NEW");
        MMC1 {
            mirroring: file.mirroring,
            file,
            sram: [0; 0x2000],
            shift_times: 0,
            shift_register: 0x10,
            prg_rom_mode: PrgRomMode::FixedFirst,
            single_chr_rom: true,
            chr_ram: [0; 0x2000],
            chr_bank_0: 0,
            chr_bank_1: 0,
            prg_bank: 0,
            control: 0,
        }
    }

    // 4bit0
    // -----
    // CPPMM
    // |||||
    // |||++- Mirroring (0: one-screen, lower bank; 1: one-screen, upper bank;
    // |||               2: vertical; 3: horizontal)
    // |++--- PRG ROM bank mode (0, 1: switch 32 KB at $8000, ignoring low bit of bank number;
    // |                         2: fix first bank at $8000 and switch 16 KB bank at $C000;
    // |                         3: fix last bank at $C000 and switch 16 KB bank at $8000)
    // +----- CHR ROM bank mode (0: switch 8 KB at a time; 1: switch two separate 4 KB banks)
    fn update_control(&mut self, value: u8) {
        self.control = value;

        let mirroring = value & 3;
        self.mirroring = match mirroring {
            0 => Mirroring::SingleScreenLower,
            1 => Mirroring::SingleScreenUpper,
            2 => Mirroring::Vertical,
            3 => Mirroring::Horizontal,
            _ => panic!("MMC1 Impossible result"),
        };

        self.prg_rom_mode = match (value >> 2) & 3 {
            0 | 1 => PrgRomMode::SingleBig,
            2 => PrgRomMode::FixedFirst,
            3 => PrgRomMode::FixedLast,
            _ => panic!("MMC1 Invalid value: {}", value),
        };

        self.single_chr_rom = (0x10 & value) == 0;
        //println!("Control = {:02X}", value);
    }

    fn update_chr_bank_0(&mut self, value: u8) {
        if self.file.chr_rom_blocks == 0 {
            return;
        }
        //println!("CHR0: {:02X}", value);
        self.chr_bank_0 = if self.single_chr_rom {
            value & 0xFE
        } else {
            value
        } % (self.file.chr_rom_blocks * 2);
        //println!("CHR0: {:02X}", self.chr_bank_0);
    }

    fn update_chr_bank_1(&mut self, value: u8) {
        if self.file.chr_rom_blocks == 0 {
            return;
        }

        //println!("CHR1: {:02X}", value);
        self.chr_bank_1 = value % (self.file.chr_rom_blocks * 2);
        //println!("CHR1: {:02X}", self.chr_bank_1);
        //println!("BLOCKS: {}", self.file.chr_rom_blocks);
    }

    fn update_prg_bank(&mut self, value: u8) {
        self.prg_bank = value;
    }
}

// CPU $6000-$7FFF: 8 KB PRG RAM bank, (optional)
// CPU $8000-$BFFF: 16 KB PRG ROM bank, either switchable or fixed to the first bank
// CPU $C000-$FFFF: 16 KB PRG ROM bank, either fixed to the last bank or switchable
// PPU $0000-$0FFF: 4 KB switchable CHR bank
// PPU $1000-$1FFF: 4 KB switchable CHR bank
impl Cartridge for MMC1 {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0x6000...0x7FFF => self.sram[address as usize - 0x6000],
            _ => match self.prg_rom_mode {
                PrgRomMode::SingleBig => match address {
                    0x8000...0xFFFF => {
                        self.file.prg_rom
                            [address as usize - 0x8000 + self.prg_bank as usize * 0x4000]
                    }
                    _ => panic!("MMC1 Read outside of range: {:X}", address),
                },
                PrgRomMode::FixedFirst => match address {
                    0x8000...0xBFFF => self.file.prg_rom[address as usize - 0x8000],
                    0xC000...0xFFFF => {
                        self.file.prg_rom
                            [address as usize - 0xC000 + self.prg_bank as usize * 0x4000]
                    }
                    _ => panic!("MMC1 Read outside of range: {:X}", address),
                },
                PrgRomMode::FixedLast => match address {
                    0x8000...0xBFFF => {
                        self.file.prg_rom
                            [address as usize - 0x8000 + self.prg_bank as usize * 0x4000]
                    }
                    0xC000...0xFFFF => {
                        self.file.prg_rom[address as usize - 0xC000
                            + (self.file.prg_rom_blocks as usize - 1) * 0x4000]
                    }
                    _ => panic!("MMC1 Read outside of range: {:X}", address),
                },
            },
        }
    }

    fn write(&mut self, address: u16, byte: u8) {
        match address {
            0x6000...0x7FFF => self.sram[address as usize - 0x6000] = byte,
            0x8000...0xFFFF => {
                if byte & 0x80 != 0 {
                    self.shift_register = 0x10;
                    self.update_control(self.control | 0x0C);
                } else {
                    let last = self.shift_register & 1 != 0;
                    self.shift_register >>= 1;
                    self.shift_register |= (byte & 1) << 4;
                    if last {
                        match address {
                            0x8000...0x9FFF => self.update_control(self.shift_register),
                            0xA000...0xBFFF => self.update_chr_bank_0(self.shift_register),
                            0xC000...0xDFFF => self.update_chr_bank_1(self.shift_register),
                            0xE000...0xFFFF => self.update_prg_bank(self.shift_register),
                            _ => (),
                        }
                        self.shift_register = 0x10;
                    }
                }
            }
            _ => println!("Invalid MMC1 write {:X} = {:X}", address, byte),
        }
    }

    fn ppu_read(&mut self, address: u16) -> u8 {
        if address >= 0x2000 {
            return self.ppu_read(address % 0x2000);
        }
        if self.file.chr_rom_blocks == 0 {
            self.chr_ram[address as usize]
        } else if self.single_chr_rom {
            //println!("READING HEY!");
            //println!("CONTROL: {}", self.control);
            let addr = self.chr_bank_0 & 0xFE;
            self.file.chr_rom[address as usize + addr as usize * 0x2000]
        } else {
            //println!("CONTROL: {}", self.control);
            match address {
                0x0000...0x0FFF => {
                    self.file.chr_rom[address as usize + self.chr_bank_0 as usize * 0x1000]
                }
                0x1000...0x1FFF => {
                    self.file.chr_rom[address as usize + (self.chr_bank_1 as usize - 1) * 0x1000]
                }
                _ => panic!("PPU read outside of range"),
            }
        }
    }

    fn ppu_write(&mut self, address: u16, byte: u8) {
        self.chr_ram[address as usize] = byte;
    }

    fn mirroring(&self) -> Mirroring {
        self.mirroring
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_resets() {
        let mut mmc1 = MMC1::new(File::read("rom/nestest.nes"));
        assert_eq!(mmc1.shift_register, 0x10);
        mmc1.write(0xFFE1, 0x00);
        assert_eq!(mmc1.shift_register, 0x08);
        mmc1.write(0xFFE1, 0x00);
        assert_eq!(mmc1.shift_register, 0x04);
        mmc1.write(0xFFE1, 0x00);
        assert_eq!(mmc1.shift_register, 0x02);
        mmc1.write(0xFFE1, 0x00);
        assert_eq!(mmc1.shift_register, 0x01);
        mmc1.write(0xFFE1, 0x00);
        assert_eq!(mmc1.shift_register, 0x10);
    }
}

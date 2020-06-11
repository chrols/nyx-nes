use crate::ines::File;
use crate::ines::Mirroring;

mod axrom;
pub mod dummy;
mod mmc1;
mod mmc3;
mod nrom;
mod uxrom;

pub trait Cartridge {
    fn read(&mut self, address: u16) -> u8;
    fn write(&mut self, address: u16, byte: u8);
    fn ppu_read(&mut self, address: u16) -> u8;
    fn ppu_write(&mut self, address: u16, byte: u8);
    fn mirroring(&self) -> Mirroring;
}

// pub enum Mapper {
//     NROM, // 0
//     MMC1, // 1
//     UxROM, // 2
//     CNROM, // 3
//     MMC3, // 4
//     AxROM, // 7
// }

pub fn new_mapper(file: File) -> Box<Cartridge> {
    match file.mapper {
        0 => Box::new(nrom::NROM::new(file)),
        1 => Box::new(mmc1::MMC1::new(file)),
        2 => Box::new(uxrom::UxROM::new(file)),
        4 => Box::new(mmc3::MMC3::new(file)),
        7 => Box::new(axrom::AxROM::new(file)),
        _ => panic!("Unimplemented mapper!"),
    }
}

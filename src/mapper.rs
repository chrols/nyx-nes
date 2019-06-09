use super::ines::File;

mod nrom;
mod mmc1;

pub trait Cartridge {
    fn read(&mut self, address: u16) -> u8;
    fn write(&mut self, address: u16, byte: u8);
    fn ppu_read(&mut self, address: u16) -> u8;
    fn ppu_write(&mut self, address: u16, byte: u8);
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
    println!("{}", file.mapper);
    match file.mapper {
        0 => Box::new(nrom::NROM { file }),
        1 => Box::new(mmc1::MMC1::new(file)),
        _ => panic!("Unimplemented mapper!"),
    }
}
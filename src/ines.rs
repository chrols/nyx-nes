use std::fs;

extern crate zip;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Mirroring {
    Horizontal,
    Vertical,
}

#[derive(Clone, Debug)]
pub struct File {
    pub prg_rom_blocks: u8,
    pub chr_rom_blocks: u8,
    pub mirroring: Mirroring,
    pub trainer_present: bool,
    pub battery_ram: bool,
    pub four_screen_vram: bool,
    pub mapper: u8,
    pub trainer: Vec<u8>,
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
}


fn unzip(file: &Vec<u8>) -> Result<Vec<u8>, &'static str>
{
    use std::io::prelude::*;


    let mut reader = std::io::Cursor::new(file);

    let mut zip = match zip::ZipArchive::new(reader) {
        Ok(data) => data,
        Err(error) => return Err("ZipFile error"),
    };

    for i in 0..zip.len()
    {
        let mut file = zip.by_index(i).unwrap();
        println!("Filename: {}", file.name());

        let mut buf:Vec<u8> = file.bytes().map(|byte| byte.unwrap()).collect();

        if File::nes_header(&buf) {
            return Ok(buf);
        }
    }

    Err("No file")
}

impl File {

    fn nes_header(data: &Vec<u8>) -> bool {
        data[0..4] == [0x4E, 0x45, 0x53, 0x1A]
    }

    fn open_file(filename: &str) -> Result<Vec<u8>, &'static str> {
        let file = fs::read(filename).expect("Could not read provided file");

        if File::nes_header(&file) {
            Ok(file)
        } else {
            unzip(&file)
        }

    }

    pub fn read(filename: &str) -> File {
        let file = File::open_file(filename).expect("Unable to open file!");

        let prg_rom_blocks = file[4];
        let chr_rom_blocks = file[5];

        let flags6 = file[6];
        let mirroring = if flags6 & 0x01 == 0 { Mirroring::Horizontal } else { Mirroring::Vertical };

        let trainer_present = flags6 & 0x04 != 0;
        //let four_screen_vram = flags6 & 0x08 != 0;
        let mapper = (flags6 & 0xF0) >> 4;

        let trainer_size = if trainer_present { 0x200 } else { 0 };
        let prg_rom_size = prg_rom_blocks as usize * 0x4000;
        let chr_rom_size = chr_rom_blocks as usize * 0x2000;

        println!("Vertical: {}", mirroring == Mirroring::Vertical);
        println!("PRG ROM: {}", prg_rom_blocks);
        println!("CHR ROM: {}", chr_rom_blocks);
        println!("Mapper: {}", mapper);

        let trainer_offset: usize = 0x10;
        let prg_rom_offset: usize = trainer_offset + trainer_size;
        let chr_rom_offset: usize = prg_rom_offset + prg_rom_size;
        //let chr_rom_end: usize = chr_rom_offset + chr_rom_size;

        let trainer = &file[trainer_offset..trainer_offset+trainer_size];
        let prg_rom = &file[prg_rom_offset..prg_rom_offset+prg_rom_size];
        let chr_rom = &file[chr_rom_offset..chr_rom_offset+chr_rom_size];

        File {
            prg_rom_blocks,
            chr_rom_blocks,
            mirroring,
            trainer_present,
            battery_ram: false,
            four_screen_vram: false,
            mapper,
            trainer: trainer.to_vec(),
            prg_rom: prg_rom.to_vec(),
            chr_rom: chr_rom.to_vec(),
        }
    }
}

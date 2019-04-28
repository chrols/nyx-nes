mod cpu;
mod ines;
use std::env;

fn sprite(chr_data: &[u8]) -> Vec<u8> {
    let mut pixels : Vec<u8> = Vec::new();

    for i in 0..8 {
        let low_byte: u8 = chr_data[i];
        let high_byte: u8 = chr_data[i + 8];

        for j in (0..8).rev() {
            let pixel = ((low_byte >> (j as u8)) & 0x01) + 2 * ((high_byte >> (j as u8)) & 0x01);
            pixels.push(pixel);
        }
    }

    pixels
}


fn main() {
    let mut cpu = cpu::Cpu::new();
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let filename = &args[1];
        let rom_file = ines::File::read(filename);
        cpu.rom = Some(rom_file);
        cpu.reset();
        loop {
            cpu.cycle();
        }
    } else {
        cpu::Cpu::kevtris_nestest();
    }
}

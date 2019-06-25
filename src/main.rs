mod cpu;
mod apu;
mod ppu;
mod ines;
mod gui;
mod audio;
mod gamepad;
mod kevtris;
mod mapper;
use std::env;

fn main() {
    let mut cpu = cpu::Cpu::new();
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let filename = &args[1];
        let file = ines::File::read(filename);
        cpu.load_game(file);
        cpu.reset();
        gui::execute(&mut cpu);
    } else {
        cpu::Cpu::kevtris_nestest();
    }
}

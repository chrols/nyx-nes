mod apu;
mod cpu;
mod gamepad;
mod gui;
mod ines;
mod kevtris;
mod mapper;
mod png;
mod ppu;
mod record;
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

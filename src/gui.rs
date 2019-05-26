extern crate sdl2;

use super::cpu::Cpu;
use super::ppu::Ppu;
use super::ppu;

use std::time::Instant;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use sdl2::render::Canvas;
use sdl2::video::Window;

// 256 * 240

static DISP_WIDTH: u32 = 256;
static DISP_HEIGHT: u32 = 240;

static SCREEN_W: u32 = 1024 ;
static SCREEN_H: u32 = 960 ;

static BLOCK_W: usize = 4;
static BLOCK_H: usize = 4;

pub struct EmulatorWindow {
    window: u32,
    canvas: u32,
}


fn convert_color(color: ppu::Color) -> Color {
    Color::from((color.r, color.g, color.b))
}

fn render(canvas: &mut Canvas<Window>, ppu: &Ppu) {
    let bg_color = Color::RGB(0, 0, 0);

    //println!("Color frame");

    canvas.set_draw_color(bg_color);
    canvas.clear();

    for y in 0..(DISP_HEIGHT as usize) {
        for x in 0..(DISP_WIDTH as usize) {
            let fg_color = ppu.canvas[y * DISP_WIDTH as usize + x];
            canvas.set_draw_color(convert_color(fg_color));
            canvas.fill_rect(Rect::new(
                (x * BLOCK_W) as i32,
                (y * BLOCK_H) as i32,
                BLOCK_W as u32,
                BLOCK_H as u32,
            ));
        }
    }
    canvas.present();
}


pub fn execute(cpu: &mut Cpu) {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("RNES Emulator", SCREEN_W, SCREEN_H)
        .position_centered()
        .build()
        .unwrap();


    let mut canvas = window.into_canvas().build().unwrap();
    canvas.set_draw_color(Color::RGB(0,0,0));
    canvas.clear();
    canvas.present();


    //let tick = Instant::now();
    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                Event::KeyDown {
                    keycode: Some(kc), ..
                } => {
                    match kc {
                        Keycode::A => cpu.ppu.dump_pattern_tables(),
                        Keycode::O => cpu.ppu.dump_oam(),
                        Keycode::E => cpu.ppu.dump_nametables(),
                        Keycode::U => cpu.gamepad.dump_buttons(),
                        Keycode::Left => cpu.gamepad.left = true,
                        Keycode::Right => cpu.gamepad.right = true,
                        Keycode::Up => cpu.gamepad.up = true,
                        Keycode::Down => cpu.gamepad.down = true,
                        Keycode::Space => cpu.gamepad.select = true,
                        Keycode::Return => cpu.gamepad.start = true,
                        Keycode::Z => cpu.gamepad.a = true,
                        Keycode::V => cpu.gamepad.b = true,

                        _ => (),
                    }
                }
                Event::KeyUp {
                    keycode: Some(kc), ..
                } => {
                    match kc {
                        Keycode::Left => cpu.gamepad.left = false,
                        Keycode::Right => cpu.gamepad.right = false,
                        Keycode::Up => cpu.gamepad.up = false,
                        Keycode::Down => cpu.gamepad.down = false,
                        Keycode::Space => cpu.gamepad.select = false,
                        Keycode::Return => cpu.gamepad.start = false,
                        Keycode::Z => cpu.gamepad.a = false,
                        Keycode::V => cpu.gamepad.b = false,
                        _ => (),
                    }
                }
                _ => {}
            }
        }


        if cpu.ppu.updated {
            render(&mut canvas, &cpu.ppu);
            cpu.ppu.updated = false;
        }

        cpu.cycle();
        cpu.ppu.cycle();
        cpu.ppu.cycle();
        cpu.ppu.cycle();

        if cpu.cyc % 29829 == 0 {
            cpu.ppu.vblank();
            cpu.nmi();
        }

        // if tick.elapsed() >= Duration::new(0, 1_000_000_000u32 / 10) {
        //     tick = Instant::now();
        //     cpu.ppu.vblank();
        //     cpu.nmi();
        // }

        //::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 1_700_000u32));
    }
}

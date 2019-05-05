extern crate sdl2;

use super::cpu::Cpu;
use super::ppu::Ppu;
use super::ppu;

use std::time::Duration;
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

static SCREEN_W: u32 = 256 * 4;
static SCREEN_H: u32 = 240 * 4;

static BLOCK_W: usize = 2;
static BLOCK_H: usize = 2;

pub struct EmulatorWindow {
    window: u32,
    canvas: u32,
}


fn convert_color(color: ppu::Color) -> Color {
    Color::from((color.r, color.g, color.b))
}

fn render(canvas: &mut Canvas<Window>, ppu: &Ppu) {
    let bg_color = Color::RGB(0, 0, 0);

    println!("Color frame");
    
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

    
    let mut tick = Instant::now();

    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                // Event::KeyDown {
                //     scancode: Some(sc), ..
                // } => {
                //     if let Some(x) = translate_scancode(sc) {
                //         println!("Key down {}", x);
                //         //cpu.key_down(x);
                //     }
                // }
                // Event::KeyUp {
                //     scancode: Some(sc), ..
                // } => {
                //     if let Some(x) = translate_scancode(sc) {
                //         println!("Key up {}", x);
                //         //cpu.key_up(x);
                //     }
                // }
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
        
        if tick.elapsed() >= Duration::new(0, 1_000_000_000u32 / 10) {
            tick = Instant::now();
            cpu.ppu.vblank();
            cpu.nmi();
        }

        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 1_700_000u32));
    }
}

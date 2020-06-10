extern crate sdl2;

use crate::cpu::Cpu;
use crate::ppu;
use crate::ppu::Ppu;

use std::time::Instant;

use sdl2::audio::AudioSpecDesired;
use sdl2::controller::Button;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use sdl2::render::Canvas;
use sdl2::video::Window;

// 256 * 240

static DISP_WIDTH: u32 = 256;
static DISP_HEIGHT: u32 = 240;

static SCREEN_W: u32 = 1024;
static SCREEN_H: u32 = 960;

static BLOCK_W: usize = 4;
static BLOCK_H: usize = 4;

fn convert_color(color: ppu::Color) -> Color {
    Color::from((color.r, color.g, color.b))
}

fn render(canvas: &mut Canvas<Window>, ppu: &mut Ppu) {
    let bg_color = Color::RGB(0, 0, 0);

    //println!("Color frame");

    canvas.set_draw_color(bg_color);
    //canvas.clear();

    for y in 0..(DISP_HEIGHT as usize) {
        for x in 0..(DISP_WIDTH as usize) {
            let index = y * DISP_WIDTH as usize + x;
            if ppu.canvas[index] != ppu.prev_canvas[index] {
                let fg_color = ppu.canvas[index];
                canvas.set_draw_color(convert_color(fg_color));
                canvas
                    .fill_rect(Rect::new(
                        (x * BLOCK_W) as i32,
                        (y * BLOCK_H) as i32,
                        BLOCK_W as u32,
                        BLOCK_H as u32,
                    ))
                    .unwrap();
            }
        }
    }
    ppu.prev_canvas = ppu.canvas.clone();
    canvas.present();
}

pub fn execute(cpu: &mut Cpu) {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let audio_subsystem = sdl_context.audio().unwrap();
    let controller_subsystem = sdl_context.game_controller().unwrap();

    let desired_spec = AudioSpecDesired {
        freq: Some(44_100),
        channels: Some(1), // mono
        samples: Some(4),  // default sample size
    };

    let audio_queue = audio_subsystem
        .open_queue::<f32, _>(None, &desired_spec)
        .unwrap();
    audio_queue.resume();

    let _gamepad = controller_subsystem.open(0);

    println!(
        "Game controllers: {}",
        controller_subsystem.num_joysticks().unwrap()
    );

    let window = video_subsystem
        .window("RNES Emulator", SCREEN_W, SCREEN_H)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let mut frame_tick = Instant::now();
    let mut input_poll = Instant::now();
    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        let msec_since_poll = input_poll.elapsed().as_millis();
        // Approx 16 ms between frames
        if msec_since_poll > 16 {
            input_poll = Instant::now();

            for event in event_pump.poll_iter() {
                //println!("{:?}", event);
                match event {
                    Event::ControllerButtonDown { button, .. } => match button {
                        Button::DPadLeft => cpu.gamepad.left = true,
                        Button::DPadRight => cpu.gamepad.right = true,
                        Button::DPadUp => cpu.gamepad.up = true,
                        Button::DPadDown => cpu.gamepad.down = true,
                        Button::Back => cpu.gamepad.select = true,
                        Button::Start => cpu.gamepad.start = true,
                        Button::A => cpu.gamepad.a = true,
                        Button::X => cpu.gamepad.b = true,
                        _ => println!("{:?}", button),
                    },
                    Event::ControllerButtonUp { button, .. } => match button {
                        Button::DPadLeft => cpu.gamepad.left = false,
                        Button::DPadRight => cpu.gamepad.right = false,
                        Button::DPadUp => cpu.gamepad.up = false,
                        Button::DPadDown => cpu.gamepad.down = false,
                        Button::Back => cpu.gamepad.select = false,
                        Button::Start => cpu.gamepad.start = false,
                        Button::A => cpu.gamepad.a = false,
                        Button::X => cpu.gamepad.b = false,
                        _ => println!("{:?}", button),
                    },
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => break 'running,
                    Event::KeyDown {
                        keycode: Some(kc), ..
                    } => match kc {
                        Keycode::Num1 => cpu.tracing = !cpu.tracing,
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
                        Keycode::J => cpu.gamepad.a = true,
                        Keycode::Q => cpu.gamepad.b = true,

                        _ => (),
                    },
                    Event::KeyUp {
                        keycode: Some(kc), ..
                    } => match kc {
                        Keycode::Left => cpu.gamepad.left = false,
                        Keycode::Right => cpu.gamepad.right = false,
                        Keycode::Up => cpu.gamepad.up = false,
                        Keycode::Down => cpu.gamepad.down = false,
                        Keycode::Space => cpu.gamepad.select = false,
                        Keycode::Return => cpu.gamepad.start = false,
                        Keycode::J => cpu.gamepad.a = false,
                        Keycode::Q => cpu.gamepad.b = false,
                        _ => (),
                    },
                    _ => {}
                }
            }
        }

        if cpu.ppu.updated {
            render(&mut canvas, &mut cpu.ppu);

            let buffer = cpu.apu.drain();

            let delay = buffer.len() as u128 * 1_000_000 / 44_100;

            let elapsed = frame_tick.elapsed();
            if elapsed.as_micros() > delay {
                println!("Frame took: {} msec", elapsed.as_millis());
                cpu.ppu.dump();
            } else {
                while frame_tick.elapsed().as_micros() < delay {}
            }
            audio_queue.queue(&buffer);

            frame_tick = Instant::now();

            cpu.ppu.updated = false;
        }

        cpu.cycle();
    }
}

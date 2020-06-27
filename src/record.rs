use crate::ppu::Ppu;

use gif::{Encoder, Frame, Parameter, Repeat};
use std::fs::File;
use std::io::Write;

use std::time::{SystemTime, UNIX_EPOCH};

pub type GifEncoder = Encoder<std::fs::File>;

pub fn new_gif_encoder(width: u16, height: u16) -> GifEncoder {
    let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
    let name = format!("{:?}.gif", now.as_millis());
    let color_map = &[0xFF, 0xFF, 0xFF, 0, 0, 0];
    let image = File::create(name).unwrap();;
    let mut encoder = Encoder::new(image, width, height, color_map).unwrap();
    Repeat::Infinite.set_param(&mut encoder).unwrap();
    encoder
}

pub fn write_gif_frame<W: Write>(encoder: &mut gif::Encoder<W>, ppu: &Ppu) {
    // FIXME Enlarge GIF
    let mut pixels = Vec::with_capacity(ppu.canvas.len());
    for p in ppu.canvas.iter() {
        pixels.push(p.r);
        pixels.push(p.g);
        pixels.push(p.b);
    }
    let mut frame = Frame::from_rgb_speed(256, 240, &pixels, 30);
    frame.delay = 6;
    encoder.write_frame(&frame).unwrap();
}

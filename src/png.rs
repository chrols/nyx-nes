use crate::gui;
use crate::ppu::Ppu;

use std::fs::File;
use std::io::BufWriter;
use std::path::Path;

use std::time::{SystemTime, UNIX_EPOCH};

pub fn write_png_frame(ppu: &Ppu) {
    let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
    let name = format!("{:?}.png", now.as_millis());
    let path = Path::new(&name);
    let file = File::create(path).unwrap();
    let ref mut w = BufWriter::new(file);

    let mut encoder = png::Encoder::new(w, gui::DISP_WIDTH, gui::DISP_HEIGHT);
    encoder.set_color(png::ColorType::RGB);
    encoder.set_depth(png::BitDepth::Eight);
    let mut writer = encoder.write_header().unwrap();

    let mut pixels = Vec::with_capacity(ppu.canvas.len());
    for p in ppu.canvas.iter() {
        pixels.push(p.r);
        pixels.push(p.g);
        pixels.push(p.b);
    }

    writer.write_image_data(&pixels).unwrap();
}

pub fn read_png_frame(filename: &str) -> Vec<u8> {
    let decoder = png::Decoder::new(File::open(filename).unwrap());
    let (info, mut reader) = decoder.read_info().unwrap();
    let mut buf = vec![0; info.buffer_size()];
    reader.next_frame(&mut buf).unwrap();
    buf
}

pub fn compare_ppu_frame_with_reference(ppu: &Ppu, reference: &str) -> bool {
    let mut pixels = Vec::with_capacity(ppu.canvas.len());
    for p in ppu.canvas.iter() {
        pixels.push(p.r);
        pixels.push(p.g);
        pixels.push(p.b);
    }

    let buf = read_png_frame(reference);
    let mut buf_i = buf.iter();
    let mut pix_i = pixels.iter();

    let res = buf_i.all(|x| {
        if let Some(n) = pix_i.next() {
            x == n
        } else {
            false
        }
    });

    res
}

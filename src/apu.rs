mod envelope;
mod noise;
mod pulse;
mod triangle;

use noise::Noise;
use pulse::Pulse;
use triangle::Triangle;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct FrameCounter {
    mode_zero: bool,
    interrupt_enable: bool,
    cpu_cycles: u16,
}

impl FrameCounter {
    fn new() -> FrameCounter {
        FrameCounter {
            mode_zero: true,
            interrupt_enable: false,
            cpu_cycles: 0,
        }
    }

    // Returns: quarter frame clock, half frame clock, interrupt
    fn cycle(&mut self) -> (bool, bool, bool) {
        self.cpu_cycles += 1;
        if self.mode_zero {
            self.mode_zero_cycle()
        } else {
            self.mode_one_cycle()
        }
    }

    fn mode_zero_cycle(&mut self) -> (bool, bool, bool) {
        match self.cpu_cycles {
            7457 => (true, false, false),
            14913 => (true, true, false),
            22371 => (true, false, false),
            29828 => (false, false, true),
            29829 => (true, true, true),
            29830 => {
                self.cpu_cycles = 0;
                (false, false, true)
            }
            _ => (false, false, false),
        }
    }

    fn mode_one_cycle(&mut self) -> (bool, bool, bool) {
        match self.cpu_cycles {
            7457 => (true, false, false),
            14913 => (true, true, false),
            22371 => (true, false, false),
            29829 => (false, false, false),
            37281 => (true, true, false),
            37282 => {
                self.cpu_cycles = 0;
                (false, false, false)
            }
            _ => (false, false, false),
        }
    }
}

const NSEC_CPU_CYCLE: u64 = 1_000_000_000 / 1_789_773;
const NSEC_SAMPLE_TIME: u64 = 1_000_000_000 / 44_100;

#[derive(Serialize, Deserialize)]
pub struct Apu {
    pulse_table: Vec<f32>,
    tnd_table: Vec<f32>,
    pulse1: Pulse,
    pulse2: Pulse,
    triangle: Triangle,
    noise: Noise,
    frame_counter: FrameCounter,
    buffer: Vec<f32>,
    odd_cycle: bool,
    time_next_sample: u64,
    time_current: u64,
}

impl Apu {
    pub fn new() -> Apu {
        Apu {
            pulse_table: (0..32)
                .map(|x| 95.52 / (8128.0 / x as f32 + 100.0))
                .collect(),
            tnd_table: (0..204)
                .map(|x| 163.67 / (24329.0 / x as f32 + 100.0))
                .collect(),
            pulse1: Pulse::new(),
            pulse2: Pulse::new_channel_2(),
            triangle: Triangle::new(),
            noise: Noise::new(),
            frame_counter: FrameCounter::new(),
            buffer: Vec::new(),
            odd_cycle: false,
            time_next_sample: 0,
            time_current: 0,
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4015 => self.read_status(),
            _ => panic!("Attempt to read from APU: {:04X}", address),
        }
    }

    pub fn write(&mut self, address: u16, byte: u8) {
        match address {
            0x4000 => self.pulse1.write_control(byte),
            0x4001 => self.pulse1.write_sweep(byte),
            0x4002 => self.pulse1.write_timer_low(byte),
            0x4003 => self.pulse1.write_timer_high(byte),
            0x4004 => self.pulse2.write_control(byte),
            0x4005 => self.pulse2.write_sweep(byte),
            0x4006 => self.pulse2.write_timer_low(byte),
            0x4007 => self.pulse2.write_timer_high(byte),
            0x4008 => self.triangle.write_control(byte),
            0x4009 => (),
            0x400A => self.triangle.write_timer_low(byte),
            0x400B => self.triangle.write_timer_high(byte),
            0x400C => self.noise.write_control(byte),
            0x400D => (),
            0x400E => self.noise.write_mode(byte),
            0x400F => self.noise.write_length_counter(byte),
            0x4010...0x4013 => (), //println!("{:04X} = {:04X}", address, byte),
            0x4014 => (),
            0x4015 => self.write_status(byte),
            0x4017 => (),
            _ => panic!("Attempt to write to APU: {:04X} = {:02X}", address, byte),
        }

        //self.device1.resume();
        //self.device2.resume();
    }

    fn write_status(&mut self, byte: u8) {
        self.pulse1.set_enabled(byte & 1 != 0);
        self.pulse2.set_enabled(byte & 2 != 0);
        self.triangle.set_enabled(byte & 4 != 0);
        self.noise.set_enabled(byte & 8 != 0);
        // self.dmc.set_enabled(byte & 0x10 != 0);
    }

    fn read_status(&mut self) -> u8 {
        let b0 = if self.pulse1.enabled { 0x01 } else { 0 };
        let b1 = if self.pulse2.enabled { 0x02 } else { 0 };
        let b2 = if self.triangle.enabled { 0x04 } else { 0 };
        let b3 = if self.noise.enabled { 0x08 } else { 0 };
        //let b4 =  if self.dmc.enabled { 0x10 } else { 0 };
        b0 | b1 | b2 | b3
    }

    pub fn cpu_cycle(&mut self) {
        if self.odd_cycle {
            self.odd_cycle = false;
            self.apu_cycle();
        } else {
            self.odd_cycle = true;
        }

        self.triangle.cycle();

        let (quarter_frame, half_frame, interrupt) = self.frame_counter.cycle();

        if quarter_frame {
            self.pulse1.on_quarter_frame();
            self.pulse2.on_quarter_frame();
            self.noise.on_quarter_frame();
            self.triangle.on_quarter_frame();
        }

        if half_frame {
            self.pulse1.on_half_frame();
            self.pulse2.on_half_frame();
            self.noise.on_half_frame();
            self.triangle.on_half_frame();
        }

        if interrupt {}

        if self.time_current >= self.time_next_sample {
            self.buffer.push(self.mix());
            self.time_next_sample += NSEC_SAMPLE_TIME;
        }

        self.time_current += NSEC_CPU_CYCLE;
    }

    fn apu_cycle(&mut self) {
        self.pulse1.on_clock();
        self.pulse2.on_clock();
        self.noise.on_clock();
    }

    fn mix(&self) -> f32 {
        let t = self.triangle.output() as usize;
        let n = self.noise.output() as usize;
        assert!(t <= 15);
        let pulse1 = self.pulse1.output() as usize;
        let pulse2 = self.pulse2.output() as usize;
        let pulse_out = self.pulse_table[pulse1 + pulse2];
        let tnd_out: f32 = self.tnd_table[t * 3 + n * 2];
        let out = pulse_out + tnd_out;
        return out;
    }

    pub fn drain(&mut self) -> Vec<f32> {
        let copy = self.buffer.clone();
        self.buffer.clear();
        copy
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frame_counter_four_step() {
        let mut fc = FrameCounter::new();
        let mut quarter_frames = 0;
        let mut half_frames = 0;
        let mut interrupts = 0;

        for _ in 0..(14915 * 20) {
            let (quarter_frame, half_frame, interrupt) = fc.cycle();

            if quarter_frame {
                quarter_frames += 1;
            }

            if half_frame {
                half_frames += 1;
            }

            if interrupt {
                interrupts += 1;
            }
        }

        assert_eq!(quarter_frames, 40);
        assert_eq!(half_frames, 20);
        assert_eq!(interrupts, 30);
    }

    #[test]
    fn frame_counter_five_step() {
        let mut fc = FrameCounter::new();
        fc.mode_zero = false;

        let mut quarter_frames = 0;
        let mut half_frames = 0;
        let mut interrupts = 0;

        for _ in 0..(18641 * 20) {
            let (quarter_frame, half_frame, interrupt) = fc.cycle();

            if quarter_frame {
                quarter_frames += 1;
            }

            if half_frame {
                half_frames += 1;
            }

            if interrupt {
                interrupts += 1;
            }
        }

        assert_eq!(quarter_frames, 40);
        assert_eq!(half_frames, 20);
        assert_eq!(interrupts, 0);
    }

}

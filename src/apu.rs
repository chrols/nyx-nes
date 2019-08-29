struct ApuTriangle {
    pos: usize,
    timer_value: u16,
    timer_period: u16,
    lcl: u8,
    control: bool,

    length_counter: u8,
    length_table: Vec<u8>,

    // Linear counter
    lc_value: u8,
    lc_reload_flag: bool,
    lc_reload_value: u8,
}

impl ApuTriangle {
    fn new() -> ApuTriangle {
        ApuTriangle {
            pos: 0,
            timer_value: 0,
            timer_period: 0,
            lcl: 0,
            control: false,

            length_counter: 0,
            length_table: vec![
                10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48,
                20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30,
            ],

            lc_value: 0,
            lc_reload_flag: false,
            lc_reload_value: 0,
        }
    }

    fn write_control(&mut self, value: u8) {
        self.control = (value & 0x80) == 0;
        self.lc_reload_value = value & 0x7F;
    }

    fn write_timer_low(&mut self, value: u8) {
        self.timer_period = (0xFF00 & self.timer_period) | value as u16;
    }

    fn write_timer_high(&mut self, value: u8) {
        self.length_counter = self.length_table[(value >> 3) as usize];
        self.timer_period = (0x00FF & self.timer_period) | ((value as u16 & 0x3) << 8);
        self.timer_value = self.timer_period;
        self.lc_reload_flag = true;
    }

    fn on_linear_counter_clock(&mut self) {
        if self.lc_reload_flag {
            self.lc_value = self.lc_reload_value;
        } else if self.lc_value != 0 {
            self.lc_value -= 1;
        }

        if self.control {
            self.lc_reload_flag = false;
        }
    }

    fn on_length_counter_clock(&mut self) {
        if self.control && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    fn cycle(&mut self) {
        self.timer_value = if let Some(t) = self.timer_value.checked_sub(1) {
            t
        } else {
            if self.lc_value > 0 && self.length_counter > 0 {
                self.pos = (self.pos + 1) % 32;
            }

            self.timer_period
        }
    }

    fn output(&self) -> u8 {
        let triangle = [
            15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 15,
        ];

        if self.lc_value == 0 || self.length_counter == 0 {
            0
        } else {
            triangle[self.pos]
        }
    }
}

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

pub struct Apu {
    pulse_table: Vec<f32>,
    tnd_table: Vec<f32>,
    triangle: ApuTriangle,
    frame_counter: FrameCounter,
    buffer: Vec<i16>,
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
            triangle: ApuTriangle::new(),
            frame_counter: FrameCounter::new(),
            buffer: Vec::new(),
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match address {
            0x4015 => 0,
            _ => panic!("Attempt to read from APU: {:04X}", address),
        }
    }

    pub fn write(&mut self, address: u16, byte: u8) {
        match address {
            0x4000 => (),          // self.set_channel_1(byte),
            0x4001...0x4003 => (), //println!("{:04X} = {:04X}", address, byte),
            0x4004 => (),          //self.set_channel_2(byte),
            0x4005...0x4007 => (), //println!("{:04X} = {:04X}", address, byte),
            0x4008 => self.triangle.write_control(byte),
            0x4009 => (),
            0x400A => self.triangle.write_timer_low(byte),
            0x400B => self.triangle.write_timer_high(byte),
            0x400C...0x4017 => (), //println!("{:04X} = {:04X}", address, byte),
            _ => panic!("Attempt to write to APU: {:04X} = {:02X}", address, byte),
        }

        //self.device1.resume();
        //self.device2.resume();
    }

    pub fn cpu_cycle(&mut self) {
        self.triangle.cycle();

        let (quarter_frame, half_frame, interrupt) = self.frame_counter.cycle();

        if quarter_frame {
            // FIXME Envelopes
            // FIXME Triangle linear counter
            self.triangle.on_linear_counter_clock();
        }

        if half_frame {
            // FIXME Length counters
            self.triangle.on_length_counter_clock();
            // FIXME Sweep units
        }

        if interrupt {}
        self.mix();
        self.buffer.push(self.mix());
    }

    fn apu_cycle(&mut self) {}

    fn mix(&self) -> i16 {
        // FIXME MIX MORE
        let t = self.triangle.output() as usize;
        assert!(t <= 15);
        let tnd_out: f32 = self.tnd_table[t * 3];
        let conv = (tnd_out * std::i16::MAX as f32) as i16;
        return conv;
    }

    pub fn drain(&mut self) -> Vec<i16> {
        let copy = self.buffer.clone();
        self.buffer.clear();
        copy
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn triangle_cycle() {
        // Loop over given range and verify that it repeats
        let triangle = [
            15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 15,
        ];

        let mut tri = ApuTriangle::new();
        tri.lc_value = 10;
        tri.length_counter = 10;

        for t in triangle.iter() {
            assert_eq!(tri.output(), *t as u8);
            tri.cycle();
        }

        assert_eq!(tri.output(), 15);
    }

    #[test]
    fn triangle_period() {
        let mut tri = ApuTriangle::new();
        tri.lc_value = 10;
        tri.length_counter = 10;

        tri.timer_value = 2;
        tri.timer_period = 2;
        assert_eq!(tri.output(), 15);
        tri.cycle();
        assert_eq!(tri.output(), 15);
        tri.cycle();
        assert_eq!(tri.output(), 15);
        tri.cycle();
        assert_eq!(tri.output(), 14);
    }

    #[test]
    fn frame_counter_four_step() {
        let mut fc = FrameCounter::new();
        let mut quarter_frames = 0;
        let mut half_frames = 0;
        let mut interrupts = 0;

        for i in 0..(14915 * 20) {
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

        for i in 0..(18641 * 20) {
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

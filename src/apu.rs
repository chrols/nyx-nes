struct ApuTriangle {
    pos: usize,
    timer_value: u16,
    timer_period: u16,
    lcl: u8,
    counter_period: u8, // Counter reload value
    control: bool,
    linear_counter: u8,
    length_value: u8,
    length_counter: u8,
    length_table: Vec<u8>,
    counter_reload: bool,
}

impl ApuTriangle {
    fn new() -> ApuTriangle {
        ApuTriangle {
            pos: 0,
            timer_value: 0,
            timer_period: 0,
            lcl: 0,
            counter_period: 0,
            control: false,
            linear_counter: 0,

            length_value: 0,
            length_counter: 0,
            length_table: vec![
                10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48,
                20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30,
            ],
            counter_reload: false,
        }
    }

    fn write_control(&mut self, value: u8) {
        // 0x6F 0x0110 1111
        // println!("Control: {:02X}", value);
        self.control = (value & 0x80) != 0;
        self.counter_period = value & 0x7F;
    }

    fn write_timer_low(&mut self, value: u8) {
        println!("Timer low: {:02X}", value);
        self.timer_period = (0xFF00 & self.timer_value) | value as u16;
    }

    fn write_timer_high(&mut self, value: u8) {
        println!("Timer higher: {:02X}", value);
        self.length_value = self.length_table[(value >> 3) as usize];
        self.timer_period = (0x00FF & self.timer_value) | ((value as u16) << 8);
        self.timer_value = self.timer_period;
        self.counter_reload = true;
    }

    // stepTimer
    fn cycle(&mut self) {
        // if self.counter_reload {
        //     //self.length_counter = self.counter_reload;
        // } else if self.length_counter > 0 {
        //     self.length_counter -= 1;
        // }

        // // if self.length_enabled {
        // //     self.counter_reload = false;
        // // }

        self.timer_value = if let Some(t) = self.timer_value.checked_sub(1) {
            t
        } else {
            if self.length_value > 0 && self.length_counter > 0 {
                self.pos += 1;
                self.pos %= 32;
            }

            self.timer_period
        }
    }

    fn output(&self) -> u8 {
        let triangle = [
            15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 15,
        ];

        if self.linear_counter == 0 || self.length_counter == 0 {
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
}

impl Apu {
    pub fn new() -> Apu {
        Apu {
            pulse_table: (0..32)
                .map(|x| 95.52 / (8128.0 / x as f32 + 100.0))
                .collect(),
            tnd_table: (0..32)
                .map(|x| 163.67 / (24329.0 / x as f32 + 100.0))
                .collect(),
            triangle: ApuTriangle::new(),
            frame_counter: FrameCounter::new(),
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
        let (quarter_frame, half_frame, interrupt) = self.frame_counter.cycle();

        if quarter_frame {
            // FIXME Envelopes
            // FIXME Triangle linear counter
        }

        if half_frame {
            // FIXME Length counters
            // FIXME Sweep units
        }

        if interrupt {}
    }

    fn apu_cycle(&mut self) {}

    fn drain(&mut self) {
        // let target_bytes = (44_100 * 4) / 1000 * 16;
        // let wave = self.gen_output(target_bytes);
        // //let wave = gen_wave(target_bytes);
        // self.queue.queue(&wave);
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

        for t in triangle.iter() {
            assert_eq!(tri.output(), *t as u8);
            tri.cycle();
        }

        assert_eq!(tri.output(), 15);
    }

    #[test]
    fn triangle_period() {
        let mut tri = ApuTriangle::new();
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

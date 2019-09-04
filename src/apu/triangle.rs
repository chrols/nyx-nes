pub struct Triangle {
    pub enabled: bool,
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

impl Triangle {
    pub fn new() -> Triangle {
        Triangle {
            enabled: true,
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

    pub fn write_control(&mut self, value: u8) {
        self.control = (value & 0x80) == 0;
        self.lc_reload_value = value & 0x7F;
    }

    pub fn write_timer_low(&mut self, value: u8) {
        self.timer_period = (0xFF00 & self.timer_period) | value as u16;
    }

    pub fn write_timer_high(&mut self, value: u8) {
        self.length_counter = self.length_table[(value >> 3) as usize];
        self.timer_period = (0x00FF & self.timer_period) | ((value as u16 & 0x7) << 8);
        self.timer_value = self.timer_period;
        self.lc_reload_flag = true;
    }

    pub fn on_quarter_frame(&mut self) {
        if self.lc_reload_flag {
            self.lc_value = self.lc_reload_value;
        } else if self.lc_value != 0 {
            self.lc_value -= 1;
        }

        if self.control {
            self.lc_reload_flag = false;
        }
    }

    pub fn on_half_frame(&mut self) {
        if self.control && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    pub fn cycle(&mut self) {
        if self.timer_value == 0 {
            if self.lc_value > 0 && self.length_counter > 0 {
                self.pos += 1;
                self.pos %= 32;
            }

            self.timer_value = self.timer_period;
        } else {
            self.timer_value -= 1;
        }
    }

    pub fn output(&self) -> u8 {
        let triangle = [
            15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 15,
        ];

        if self.lc_value == 0 || self.length_counter == 0 || !self.enabled {
            0
        } else {
            triangle[self.pos]
        }
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !self.enabled {
            self.length_counter = 0;
        }
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

        let mut tri = Triangle::new();
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
        let mut tri = Triangle::new();
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
}

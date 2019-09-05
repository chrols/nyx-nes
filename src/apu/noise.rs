use super::envelope::Envelope;

const TIMER_PERIOD: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

pub struct Noise {
    pub enabled: bool,
    envelope: Envelope,
    length_counter_halt: bool,
    length_table: Vec<u8>,
    length_counter: u8,
    shift_register: u16,
    mode_flag: bool,

    timer_value: u16,
    timer_period: u16,
}

impl Noise {
    pub fn new() -> Noise {
        Noise {
            enabled: true,
            envelope: Envelope::new(),
            length_counter_halt: false,
            length_table: vec![
                10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48,
                20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30,
            ],
            length_counter: 0,
            shift_register: 1,
            mode_flag: false,
            timer_value: 0,
            timer_period: 0,
        }
    }

    pub fn write_control(&mut self, byte: u8) {
        self.length_counter_halt = (byte & 0x20) != 0;
        self.envelope.set_constant_flag((byte & 0x10) != 0);
        self.envelope.set_v(byte & 0x0F);
    }

    pub fn write_mode(&mut self, byte: u8) {
        self.mode_flag = (byte & 0x80) != 0;
        self.timer_period = TIMER_PERIOD[(byte & 0x0F) as usize];
    }

    pub fn write_length_counter(&mut self, byte: u8) {
        self.length_counter = self.length_table[(byte >> 3) as usize];
        self.envelope.set_start();
    }

    pub fn on_clock(&mut self) {
        if self.timer_value == 0 {
            self.timer_value = self.timer_period;
            self.shift_register = Noise::new_shift_value(self.shift_register, self.mode_flag);
        } else {
            self.timer_value -= 1;
        }
    }

    fn new_shift_value(shift_value: u16, mode_flag: bool) -> u16 {
        let bit_two = (shift_value & if mode_flag { 0x0020 } else { 0x0002 }) != 0;
        let feedback = (shift_value & 0x0001 != 0) ^ bit_two;
        let mut new_shift_value = shift_value;
        new_shift_value = new_shift_value >> 1;
        new_shift_value = (new_shift_value & 0xBFFF) | if feedback { 0x4000 } else { 0 };
        new_shift_value
    }

    pub fn on_quarter_frame(&mut self) {
        self.envelope.on_clock();
    }

    pub fn on_half_frame(&mut self) {
        if !self.length_counter_halt && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    pub fn output(&self) -> u8 {
        let shifter_says_no = 0x0001 & self.shift_register == 1;
        if shifter_says_no || self.length_counter == 0 || !self.enabled {
            0
        } else {
            self.envelope.output()
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
    fn shift_feedback_no_mode() {
        assert_eq!(Noise::new_shift_value(0x0001, false), 0x4000);
        assert_eq!(Noise::new_shift_value(0x0003, false), 0x0001);

        assert_eq!(Noise::new_shift_value(0x0001, true), 0x4000);
        assert_eq!(Noise::new_shift_value(0x0021, true), 0x0010);
    }

    #[test]
    fn correct_length_no_mode() {
        let mut v = 1;
        for i in 0..32767 {
            v = Noise::new_shift_value(v, false);
            if i != 32766 && v == 1 {
                assert_ne!(v, 1);
            }
        }
        assert_eq!(v, 1);

    }

    #[test]
    fn correct_length_mode() {
        let mut v = 1;
        for i in 0..31 {
            v = Noise::new_shift_value(v, true);
            if i != 30 && v == 1 {
                assert_ne!(v, 1);
            }
        }
        assert_eq!(v, 1);

    }
}

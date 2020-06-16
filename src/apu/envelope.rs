use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Envelope {
    start_flag: bool,
    loop_flag: bool,
    constant_flag: bool,
    divider: u8,
    decay_level: u8,
    value: u8,
}

impl Envelope {
    pub fn new() -> Envelope {
        Envelope {
            start_flag: false,
            loop_flag: false,
            constant_flag: false,
            divider: 0,
            decay_level: 0,
            value: 0,
        }
    }

    pub fn on_clock(&mut self) {
        if self.start_flag {
            self.start_flag = false;
            self.divider = self.value;
            self.decay_level = 15;
        } else if self.divider == 0 {
            self.divider = self.value;

            if self.decay_level != 0 {
                self.decay_level -= 1;
            } else if self.loop_flag {
                self.decay_level = 15;
            }
        } else {

        }
    }

    pub fn output(&self) -> u8 {
        if self.constant_flag {
            self.value
        } else {
            self.decay_level
        }
    }

    pub fn set_v(&mut self, v: u8) {
        self.value = v;
    }

    pub fn set_start(&mut self) {
        self.start_flag = true;
    }

    pub fn set_constant_flag(&mut self, flag: bool) {
        self.constant_flag = flag;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clocked_on_start_flag() {
        let mut e = Envelope::new();
        e.set_v(100);
        e.set_start();
        e.on_clock();
        assert_eq!(e.start_flag, false);
        assert_eq!(e.divider, 100);
        assert_eq!(e.decay_level, 15);
    }

    #[test]
    fn divider_zero_decay_nz() {
        let mut e = Envelope::new();
        e.divider = 0;
        e.decay_level = 15;
        e.set_v(100);
        e.on_clock();
        assert_eq!(e.divider, 100);
        assert_eq!(e.decay_level, 14);
    }

    #[test]
    fn divider_zero_decay_zero_w_loop() {
        let mut e = Envelope::new();
        e.divider = 0;
        e.decay_level = 0;
        e.loop_flag = true;
        e.set_v(100);
        e.on_clock();
        assert_eq!(e.divider, 100);
        assert_eq!(e.decay_level, 15);
    }

    #[test]
    fn divider_zero_decay_zero_no_loop() {
        let mut e = Envelope::new();
        e.divider = 0;
        e.decay_level = 0;
        e.loop_flag = false;
        e.set_v(100);
        e.on_clock();
        assert_eq!(e.divider, 100);
        assert_eq!(e.decay_level, 0);
    }

    #[test]
    fn constant_set() {
        let mut e = Envelope::new();
        e.constant_flag = true;
        e.set_v(100);
        e.divider = 1;
        e.decay_level = 15;
        e.on_clock();
        assert_eq!(e.output(), 100);
    }

    #[test]
    fn constant_not_set() {
        let mut e = Envelope::new();
        e.constant_flag = false;
        e.set_v(100);
        e.divider = 1;
        e.decay_level = 15;
        e.on_clock();
        assert_eq!(e.output(), 15);
    }
}

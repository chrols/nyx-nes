use super::envelope::Envelope;

const DUTY_CYCLE: [[u8; 8]; 4] = [
    [0u8, 1u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8],
    [0u8, 1u8, 1u8, 0u8, 0u8, 0u8, 0u8, 0u8],
    [0u8, 1u8, 1u8, 1u8, 1u8, 0u8, 0u8, 0u8],
    [1u8, 0u8, 0u8, 1u8, 1u8, 1u8, 1u8, 1u8],
];

pub struct Pulse {
    envelope: Envelope,
    timer_value: u16,
    timer_period: u16,
    current_duty: usize,

    length_table: Vec<u8>,

    length_counter_halt: bool,
    length_counter: u8,

    seq_pos: usize,

    channel_1: bool,

    sweep_enabled: bool,
    sweep_reload: bool,
    sweep_negate: bool,

    sweep_divider_counter: u8,
    sweep_divider_period: u8,
    sweep_shift: u8,
}

impl Pulse {
    pub fn new() -> Pulse {
        Pulse {
            envelope: Envelope::new(),
            timer_value: 0,
            timer_period: 0,
            current_duty: 0,
            length_table: vec![
                10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48,
                20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30,
            ],
            length_counter_halt: false,
            length_counter: 0,
            seq_pos: 0,
            channel_1: true,
            sweep_enabled: false,
            sweep_reload: false,
            sweep_negate: false,
            sweep_divider_counter: 0,
            sweep_divider_period: 0,
            sweep_shift: 0,
        }
    }

    pub fn new_channel_2() -> Pulse {
        Pulse {
            envelope: Envelope::new(),
            timer_value: 0,
            timer_period: 0,
            current_duty: 0,
            length_table: vec![
                10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48,
                20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30,
            ],
            length_counter_halt: false,
            length_counter: 0,
            seq_pos: 0,
            channel_1: false,
            sweep_enabled: false,
            sweep_reload: false,
            sweep_negate: false,
            sweep_divider_counter: 0,
            sweep_divider_period: 0,
            sweep_shift: 0,
        }
    }

    pub fn write_control(&mut self, byte: u8) {
        self.current_duty = ((byte >> 6) & 3) as usize;
        self.length_counter_halt = (byte & 0x20) != 0;
        self.envelope.set_constant_flag((byte & 0x10) != 0);
        self.envelope.set_v(byte & 0x0F);
    }

    pub fn write_sweep(&mut self, byte: u8) {
        self.sweep_enabled = (byte & 0x80) != 0;
        self.sweep_divider_period = (byte >> 4) & 0x3;
        self.sweep_negate = (byte & 0x08) != 0;
        self.sweep_shift = byte & 0x3;
        self.sweep_reload = true;
    }

    pub fn write_timer_low(&mut self, byte: u8) {
        self.timer_period = (0xFF00 & self.timer_period) | byte as u16;
    }

    pub fn write_timer_high(&mut self, byte: u8) {
        self.length_counter = self.length_table[(byte >> 3) as usize];
        self.timer_period = (0x00FF & self.timer_period) | ((byte as u16 & 0x07) << 8);
        self.timer_value = self.timer_period;
        self.envelope.set_start();
    }

    pub fn on_clock(&mut self) {
        if self.timer_value == 0 {
            self.timer_value = self.timer_period;
            self.seq_pos += 1;
            self.seq_pos %= 8;
        } else {
            self.timer_value -= 1;
        }
    }

    pub fn on_quarter_frame(&mut self) {
        self.envelope.on_clock();
    }

    pub fn on_half_frame(&mut self) {
        if !self.length_counter_halt && self.length_counter > 0 {
            self.length_counter -= 1;
        }

        self.update_sweep();
    }

    fn update_sweep(&mut self) {
        if !self.sweep_enabled {
            return;
        }

        if self.sweep_divider_counter == 0 {
            self.timer_period = self.sweep_new_period();
        }

        if self.sweep_divider_counter == 0 || self.sweep_reload {
            self.sweep_divider_counter = self.sweep_divider_period;
            self.sweep_reload = false;
        } else {
            self.sweep_divider_counter -= 1;
        }
    }

    fn sweep_new_period(&self) -> u16 {
        let shift_period = self.timer_period >> self.sweep_shift;

        if self.sweep_negate {
            self.timer_period - shift_period - if self.channel_1 { 1 } else { 0 }
        } else {
            self.timer_period + shift_period
        }
    }

    fn sweep_muted(&self) -> bool {
        self.timer_period > 0x7FF || self.timer_period < 8
    }

    fn sequencer_output(&self) -> bool {
        DUTY_CYCLE[self.current_duty][self.seq_pos] == 1
    }

    pub fn output(&self) -> u8 {
        if self.sweep_muted() || self.length_counter == 0 {
            0
        } else if self.sequencer_output() {
            self.envelope.output()
        } else {
            0
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn regular_update() {
        let mut p = Pulse::new();
        let mut v: Vec<u8> = Vec::new();
        p.length_counter = 255;
        p.envelope.set_constant_flag(true);
        p.envelope.set_v(1);
        p.timer_period = 8;

        for i in 0..8 * 8 {
            if i % 8 == 0 {
                v.push(p.output());
            }
            p.on_clock();
        }

        assert_eq!(v, vec![0u8, 1u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8]);

        v.clear();
        p.timer_value = 0;
        p.current_duty = 1;
        for i in 0..8 * 8 {
            if i % 8 == 0 {
                v.push(p.output());
            }
            p.on_clock();
        }

        assert_eq!(v, vec![0u8, 1u8, 1u8, 0u8, 0u8, 0u8, 0u8, 0u8]);

        v.clear();
        p.timer_value = 0;
        p.current_duty = 2;
        for i in 0..8 * 8 {
            if i % 8 == 0 {
                v.push(p.output());
            }
            p.on_clock();
        }

        assert_eq!(v, vec![0u8, 1u8, 1u8, 1u8, 1u8, 0u8, 0u8, 0u8]);

        v.clear();
        p.timer_value = 0;
        p.current_duty = 3;
        for i in 0..8 * 8 {
            if i % 8 == 0 {
                v.push(p.output());
            }
            p.on_clock();
        }

        assert_eq!(v, vec![1u8, 0u8, 0u8, 1u8, 1u8, 1u8, 1u8, 1u8]);
    }

    #[test]
    fn sweep_period_is_doubled_on_zero() {
        let mut p = Pulse::new();
        p.sweep_enabled = true;
        p.sweep_negate = false;
        p.sweep_shift = 0;
        p.timer_period = 100;
        p.on_half_frame();
        assert_eq!(p.timer_period, 200);
    }

    #[test]
    fn output_is_envelope() {
        let mut p = Pulse::new();
        p.length_counter = 1;
        p.timer_period = 8;
        p.envelope.set_constant_flag(true);
        p.envelope.set_v(100);
        p.current_duty = 3;
        assert_eq!(p.output(), 100);
    }

    #[test]
    fn sequencer_zero_no_output() {
        let mut p = Pulse::new();
        p.envelope.set_constant_flag(true);
        p.envelope.set_v(100);
        assert_eq!(p.output(), 0);
    }

    #[test]
    fn sweep_overflow_no_output() {
        let mut p = Pulse::new();
        p.timer_period = 0x800;
        p.envelope.set_constant_flag(true);
        p.envelope.set_v(100);
        p.current_duty = 3;
        assert_eq!(p.output(), 0);
    }

    #[test]
    fn length_counter_zero_no_output() {
        let mut p = Pulse::new();
        p.length_counter = 0;
        p.envelope.set_constant_flag(true);
        p.envelope.set_v(100);
        p.current_duty = 3;
        assert_eq!(p.output(), 0);
    }

    #[test]
    fn timer_below_eight_no_output() {
        let mut p = Pulse::new();
        p.length_counter = 1;
        p.timer_value = 7;
        p.envelope.set_constant_flag(true);
        p.envelope.set_v(100);
        p.current_duty = 3;
        assert_eq!(p.output(), 0);
    }

    #[test]
    fn sweep_regular_update() {
        let mut p = Pulse::new();
        p.sweep_enabled = true;
        p.sweep_divider_counter = 0;
        assert_eq!(true, false);
    }

    #[test]
    fn sweep_divider_decrement() {
        let mut p = Pulse::new();
        p.sweep_enabled = true;
        p.sweep_divider_counter = 8;
        p.sweep_divider_period = 10;
        p.on_half_frame();
        assert_eq!(p.sweep_divider_counter, 7);
    }

    #[test]
    fn sweep_divider_zero() {
        let mut p = Pulse::new();
        p.sweep_enabled = true;
        p.sweep_divider_counter = 0;
        p.sweep_divider_period = 10;
        p.on_half_frame();
        assert_eq!(p.sweep_divider_counter, 10);
        assert_eq!(p.sweep_reload, false);
    }

    #[test]
    fn sweep_divider_reload() {
        let mut p = Pulse::new();
        p.sweep_enabled = true;
        p.sweep_divider_counter = 8;
        p.sweep_divider_period = 10;
        p.sweep_reload = true;
        p.on_half_frame();
        assert_eq!(p.sweep_divider_counter, 10);
        assert_eq!(p.sweep_reload, false);
    }
}

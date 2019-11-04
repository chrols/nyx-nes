use std::cell::Cell;

pub struct Gamepad {
    pub a: bool,
    pub b: bool,
    pub select: bool,
    pub start: bool,
    pub up: bool,
    pub down: bool,
    pub left: bool,
    pub right: bool,
    byte: Cell<u8>,
    polling: bool,
}

impl Gamepad {
    pub fn new() -> Gamepad {
        Gamepad {
            a: false,
            b: false,
            select: false,
            start: false,
            up: false,
            down: false,
            left: false,
            right: false,
            byte: Cell::new(0),
            polling: false,
        }
    }

    pub fn dump_buttons(&self) {
        println!(
            "{} {} {}",
            if self.a { "A" } else { "a" },
            if self.b { "B" } else { "b" },
            if self.start { "START" } else { "start" }
        );
    }

    pub fn start_poll(&mut self) {
        self.polling = true;
    }

    pub fn stop_poll(&mut self) {
        self.polling = false;
        let mut b = 0;
        if self.a {
            b |= 0x01;
        }
        if self.b {
            b |= 0x02;
        }
        if self.select {
            b |= 0x04;
        }
        if self.start {
            b |= 0x08;
        }
        if self.up {
            b |= 0x10;
        }
        if self.down {
            b |= 0x20;
        }
        if self.left {
            b |= 0x40;
        }
        if self.right {
            b |= 0x80;
        }
        self.byte.set(b);
    }

    pub fn read(&self) -> u8 {
        if self.polling {
            1
        } else {
            let res = 0x01 & self.byte.get();
            self.byte.set(self.byte.get() >> 1);
            res
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn one_is_default() {
        let gamepad = Gamepad::new();
        assert_eq!(1, gamepad.read());
    }

    #[test]
    fn one_is_returned_during_polling() {
        let mut gamepad = Gamepad::new();
        gamepad.start_poll();
        assert_eq!(1, gamepad.read());
    }

    #[test]
    fn unpressed_is_zero() {
        let mut gamepad = Gamepad::new();
        gamepad.start_poll();
        gamepad.stop_poll();
        assert_eq!(0, gamepad.read());
    }

    #[test]
    fn order_is_correct() {
        let mut gamepad = Gamepad::new();
        gamepad.a = true;
        gamepad.select = true;
        gamepad.up = true;
        gamepad.left = true;
        gamepad.start_poll();
        gamepad.stop_poll();

        assert_eq!(1, gamepad.read());
        assert_eq!(0, gamepad.read());
        assert_eq!(1, gamepad.read());
        assert_eq!(0, gamepad.read());
        assert_eq!(1, gamepad.read());
        assert_eq!(0, gamepad.read());
        assert_eq!(1, gamepad.read());
        assert_eq!(0, gamepad.read());

        gamepad = Gamepad::new();

        gamepad.b = true;
        gamepad.start = true;
        gamepad.down = true;
        gamepad.right = true;
        gamepad.start_poll();
        gamepad.stop_poll();

        assert_eq!(0, gamepad.read());
        assert_eq!(1, gamepad.read());
        assert_eq!(0, gamepad.read());
        assert_eq!(1, gamepad.read());
        assert_eq!(0, gamepad.read());
        assert_eq!(1, gamepad.read());
        assert_eq!(0, gamepad.read());
        assert_eq!(1, gamepad.read());
    }

    #[test]
    fn reading_beyond_input_return_one() {
        let mut gamepad = Gamepad::new();
        gamepad.start_poll();
        gamepad.stop_poll();

        for _i in 0..8 {
            gamepad.read();
        }

        assert_eq!(0, gamepad.read());
    }
}

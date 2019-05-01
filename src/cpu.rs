use std::fmt;
use super::ines;
use super::ppu::Ppu;

pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    ram: [u8; 0x800],
    c: bool, // Carry flag
    z: bool, // Zero flag
    i: bool, // Interrupt disable
    d: bool, // Decimal mode flag
    b: bool, // Break command
    v: bool, // Overflow flag
    n: bool, // Negative flag
    pub rom: Option<ines::File>,
    ppu: Ppu,
}

impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "A: {:X} X: {:X} Y: {:X} PC: {:X}, SP: {:X}",
            self.a, self.x, self.y, self.pc, self.sp
        )
    }
}

#[derive(PartialEq, Debug)]
enum Instruction {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    // Unofficial ones
    KIL,
    SLO,
    ANC,
    RLA,
    SRE,
    ALR,
    RRA,
    ARR,
    SAX,
    XAA,
    AHX,
    TAS,
    SHY,
    SHX,
    LAX,
    LAS,
    DCP,
    AXS,
    ISC,
}

#[derive(PartialEq, Debug)]
enum AddressingMode {
    Implied,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndexedIndirect,
    IndirectIndexed,
}

struct Operation {
    function: CpuFunction,
    instruction: Instruction,
    mode: AddressingMode,
    bytes: u8,
    cycles: u8,
}

struct Step {
    address: u16,
    //pc: u16,
    mode: AddressingMode,
}

impl Step {
    pub fn new() -> Step {
        Step {
            address: 0,
            mode: AddressingMode::Implied,
        }
    }
}

type CpuFunction = fn(&mut Cpu, &Step);

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0xFD,
            ram: [0; 0x800],
            c: false,
            z: false,
            i: false,
            d: false,
            b: false,
            v: false,
            n: false,
            rom: None,
            ppu: Ppu::new(),
        }
    }

    pub fn kevtris_nestest() {
        let nestest = ines::File::read("nestest.nes");
        let mut cpu = Cpu::new();
        cpu.rom = Some(nestest);
        cpu.reset();
        cpu.pc = 0xC000;
        loop {
            cpu.cycle();
        }
    }

    pub fn reset(&mut self) {
        self.pc = self.read_word(0xfffc);
        // self.pc = 0xC000;
    }

    fn adc(&mut self, step: &Step) {
        let t = self.a;
        let operand = self.memory_read(step.address);
        let result: u16 = self.a as u16 + operand as u16 + self.c as u16;
        self.a = (result & 0xFF) as u8;

        self.c = result > 0xFF;
        self.v = !((t ^ operand) & 0x80) != 0 && ((t ^ self.a) & 0x80) != 0;

        self.set_zn(self.a);
    }

    fn and(&mut self, step: &Step) {
        self.a &= self.memory_read(step.address);
        self.set_zn(self.a);
    }

    fn asl(&mut self, step: &Step) {
        if step.mode == AddressingMode::Accumulator {
            self.c = (self.a & 80_u8) != 0;
            self.set_zn(self.a);
        } else {
            let mut value = self.memory_read(step.address);
            self.c = (value & 80_u8) != 0;
            value <<= 1;
            self.memory_write(step.address, value);
            self.set_zn(value);
        }
    }

    // Branch if Carry Clear
    fn bcc(&mut self, step: &Step) {
        if self.c == false {
            self.pc = step.address;
            // FIXME Branch cycles
        }
    }

    // Branch if Carry Set
    fn bcs(&mut self, step: &Step) {
        if self.c == false {
            self.pc = step.address;
            // FIXME Branch cycles
        }
    }

    // Branch if Equal
    fn beq(&mut self, step: &Step) {
        if self.z {
            self.pc = step.address;
            // FIXME Branch cycles
        }
    }

    // Bit test
    fn bit(&mut self, step: &Step) {
        let m = self.memory_read(step.address);
        let r = self.a & m;
        self.v = (0x40 & r) != 0;
        self.set_zn(r);
    }

    // Branch if minus
    fn bmi(&mut self, step: &Step) {
        if self.n {
            self.pc = step.address;
            // FIXME Branch cycles
        }
    }

    // Branch if Not Equal
    fn bne(&mut self, step: &Step) {
        if self.z == false {
            self.pc = step.address;
            // FIXME Branch cycles
        }
    }

    // Branch if Positive
    fn bpl(&mut self, step: &Step) {
        if self.n == false {
            self.pc = step.address;
            // FIXME Branch cycles
        }
    }

    // Branch if Overflow Clear
    fn brk(&mut self, _step: &Step) {
        self.push_word(self.pc);
        self.php(&Step::new());
        self.pc = self.read_word(0xFFFE);
    }

    // Branch if Overflow Clear
    fn bvc(&mut self, step: &Step) {
        if self.v == false {
            self.pc = step.address;
            // FIXME Branch cycles
        }
    }

    // Branch if Overflow Set
    fn bvs(&mut self, step: &Step) {
        if self.v {
            self.pc = step.address;
            // FIXME Branch cycles
        }
    }

    fn clc(&mut self, _step: &Step) {
        self.c = false;
    }

    fn cld(&mut self, _step: &Step) {
        self.d = false;
    }

    fn cli(&mut self, _step: &Step) {
        self.i = false;
    }

    fn clv(&mut self, _step: &Step) {
        self.v = false;
    }

    // No operation
    fn nop(&mut self, _step: &Step) {}

    fn ora(&mut self, step: &Step) {
        self.a |= self.memory_read(step.address);
        self.set_zn(self.a);
    }

    // Compare
    fn cmp(&mut self, step: &Step) {
        let m = self.memory_read(step.address);
        self.c = if self.a >= m { true } else { false };
        self.set_zn(self.a.wrapping_sub(m));
    }

    // Compare X register
    fn cpx(&mut self, step: &Step) {
        let m = self.memory_read(step.address);
        self.c = if self.x >= m { true } else { false };
        self.set_zn(self.x.wrapping_sub(m));
    }

    // Compare Y register
    fn cpy(&mut self, step: &Step) {
        let m = self.memory_read(step.address);
        self.c = if self.y >= m { true } else { false };
        self.set_zn(self.y.wrapping_sub(m));
    }

    // Rotate left

    // Move each of the bits in either A or M one place to the left.
    // Bit 0 is filled with the current value of the carry flag whilst
    // the old bit 7 becomes the new carry flag value.

    fn rol(&mut self, step: &Step) {
        if step.mode == AddressingMode::Accumulator {
            let nc = (self.a & 0x80) != 0;
            self.a <<= 1;
            if self.c {
                self.a |= 0x01;
            }
            self.c = nc;
        } else {
            let mut m = self.memory_read(step.address);
            let nc = (m & 0x80) != 0;
            m <<= 1;
            if self.c {
                m |= 0x01;
            }
            self.memory_write(step.address, m);
            self.c = nc;
        }
    }


    // Rotate right

    // Move each of the bits in either A or M one place to the right.
    // Bit 7 is filled with the current value of the carry flag whilst
    // the old bit 0 becomes the new carry flag value.

    fn ror(&mut self, step: &Step) {
        if step.mode == AddressingMode::Accumulator {
            let nc = (self.a & 0x01) != 0;
            self.a >>= 1;
            if self.c {
                self.a |= 0x80;
            }
            self.c = nc;
        } else {
            let mut m = self.memory_read(step.address);
            let nc = (m & 0x01) != 0;
            m >>= 1;
            if self.c {
                m |= 0x80;
            }
            self.memory_write(step.address, m);
            self.c = nc;
        }
    }

    // Return from interrupt
    fn rti(&mut self, step: &Step) {
        let flags = self.pop();
        self.set_flags(flags);
        self.pc = self.pop_word();
    }

    // Exclusive OR
    fn eor(&mut self, step: &Step) {
        self.a ^= self.memory_read(step.address);
        self.set_zn(self.a);
    }

    // Increment memory
    fn inc(&mut self, step: &Step) {
        let m = self.memory_read(step.address);
        self.memory_write(step.address, m.wrapping_add(1));
        self.set_zn(self.memory_read(step.address));
    }

    // Increment X register
    fn inx(&mut self, _step: &Step) {
        self.x = self.x.wrapping_add(1);
        self.set_zn(self.x);
    }

    // Increment Y register
    fn iny(&mut self, _step: &Step) {
        self.y = self.y.wrapping_add(1);
        self.set_zn(self.y);
    }

    // Decrement memory
    fn dec(&mut self, step: &Step) {
        let m = self.memory_read(step.address);
        self.memory_write(step.address, m.wrapping_sub(1));
        self.set_zn(m.wrapping_sub(1));
    }

    // Decrement X register
    fn dex(&mut self, _step: &Step) {
        self.x = self.x.wrapping_sub(1);
        self.set_zn(self.x);
    }

    // Decrement Y register
    fn dey(&mut self, _step: &Step) {
        self.y = self.y.wrapping_sub(1);
        self.set_zn(self.y);
    }

    // Subtract with carry
    fn sbc(&mut self, step: &Step) {
        let a = self.a;
        let b = self.memory_read(step.address);
        let c = if self.c { 1 } else { 0 };
        self.a = a.wrapping_sub(b.wrapping_sub(1 - c));
        self.set_zn(self.a);

        self.c = a as i16 - b as i16 - (1 - c) as i16 >= 0;
        self.v = (a ^ b) & 0x80 != 0 && (a ^ self.a) & 0x80 != 0;
    }

    // Set carry flag
    fn sec(&mut self, _step: &Step) {
        self.c = true;
    }

    // Set decimal flag
    fn sed(&mut self, _step: &Step) {
        self.d = true;
    }

    // Set interrupt disable
    fn sei(&mut self, _step: &Step) {
        self.i = true;
    }

    // Load byte to both accumulator and X (ILLEGAL)
    fn lax(&mut self, step: &Step) {
        let m = self.memory_read(step.address);
        self.x = m;
        self.a = m;
        self.set_zn(m);
    }


    // Load X Register
    fn ldx(&mut self, step: &Step) {
        self.x = self.memory_read(step.address);
        self.set_zn(self.x);
    }

    // Load Y Register
    fn ldy(&mut self, step: &Step) {
        self.y = self.memory_read(step.address);
        self.set_zn(self.y);
    }

    // Logical shift right
    fn lsr(&mut self, step: &Step) {
        if step.mode == AddressingMode::Accumulator {
            self.c = (self.a & 1) != 0;
            self.a >>= 1;
            self.set_zn(self.a);
        } else {
            let mut m = self.memory_read(step.address);
            self.c = (m & 1) != 0;
            m >>= 1;
            self.memory_write(step.address, m);
            self.set_zn(m);
        }
    }

    // Jump
    fn jmp(&mut self, step: &Step) {
        self.pc = step.address;
    }

    // Jump to Subroutine
    fn jsr(&mut self, step: &Step) {
        self.push_word(self.pc - 1);
        self.pc = step.address;
    }

    // Return from subroutine
    fn rts(&mut self, _step: &Step) {
        let w = self.pop_word() + 1;
        self.pc = w;
    }

    // Transfer accumulator to X
    fn tax(&mut self, _step: &Step) {
        self.x = self.a;
        self.set_zn(self.x);
    }

    // Transfer accumulator to Y
    fn tay(&mut self, _step: &Step) {
        self.y = self.a;
        self.set_zn(self.y);
    }

    // Transfer stack pointer to X
    fn tsx(&mut self, _step: &Step) {
        self.x = self.sp;
        self.set_zn(self.x);
    }

    fn txa(&mut self, _step: &Step) {
        self.a = self.x;
        self.set_zn(self.a);
    }

    // Transfer X to stack pointer
    fn txs(&mut self, _step: &Step) {
        self.sp = self.x;
    }

    // Transfer Y to accumulator
    fn tya(&mut self, _step: &Step) {
        self.a = self.y;
        self.set_zn(self.a);
    }


    fn lda(&mut self, step: &Step) {
        self.a = self.memory_read(step.address);
        self.set_zn(self.a);
    }

    // Store Accumulator
    fn sta(&mut self, step: &Step) {
        self.memory_write(step.address, self.a);
    }

    // Store X Register
    fn stx(&mut self, step: &Step) {
        self.memory_write(step.address, self.x);
    }

    // Store Y Register
    fn sty(&mut self, step: &Step) {
        self.memory_write(step.address, self.y);
    }

    // Push Accumulator
    fn pha(&mut self, _step: &Step) {
        self.push(self.a);
    }

    // Push processor status
    fn php(&mut self, _step: &Step) {
        let mut byte = 0_u8;
        if self.c {byte = byte | 0x40}
        if self.z {byte = byte | 0x20}
        if self.i {byte = byte | 0x10}
        if self.d {byte = byte | 0x08}
        if self.b {byte = byte | 0x04}
        if self.v {byte = byte | 0x02}
        if self.n {byte = byte | 0x01}
        self.push(byte);
    }

    // Pull accumulator
    fn pla(&mut self, _step: &Step) {
        self.a = self.pop();
        self.set_zn(self.a);
    }

    // Pull processor status
    fn plp(&mut self, _step: &Step) {
        let byte = self.pop();
        self.set_flags(byte);
    }

    fn set_flags(&mut self, byte: u8) {
        self.c = (byte & 0x40) != 0;
        self.z = (byte & 0x20) != 0;
        self.i = (byte & 0x10) != 0;
        self.d = (byte & 0x08) != 0;
        self.b = (byte & 0x04) != 0;
        self.v = (byte & 0x02) != 0;
        self.n = (byte & 0x01) != 0;
    }


    // Halt and catch fire
    fn hcf(&mut self, _step: &Step) {
        panic!("Halt and catch fire!");
    }

    pub fn nmi(&mut self) {
        println!("NMI!");
        if self.i {
            return;
        }
        self.push_word(self.pc);
        self.php(&Step::new());
        self.pc = self.read_word(0xFFFA);
    }

    fn irq(&mut self) {
        self.push_word(self.pc - 1);
        self.pc = self.read_word(0xFFFE);
    }

    fn memory_read(&self, address: u16) -> u8 {
        match address {
            0x0000...0x1FFF => self.ram[(address % 0x800) as usize],
            0x2000...0x3FFF => self.ppu.read(address),
            0x4000...0x4017 => { println!("APU Unimplemented"); 0 }
            0x4018...0x401F => panic!("Read from disabled registers"),
            0x4020...0xFFFF => match &self.rom {
                Some(game) => match game.mapper {
                    0 => game.prg_rom[(address as usize - if game.prg_rom_blocks == 1 { 0xC000} else { 0x8000 } as usize)],
                    _ => panic!("Unimplemented mapper"),
                },
                None => panic!("No game loaded"),
            },
        }
    }

    fn read_word(&self, address: u16) -> u16 {
        let low_byte = self.memory_read(address);
        let high_byte = self.memory_read(address + 1);
        low_byte as u16 | (high_byte as u16) << 8
    }

    // FIXME Explain bug
    fn read_word_bug(&self, address: u16) -> u16 {
        let b = (address & 0xFF00) | ((address as u8).wrapping_add(1)) as u16;
        let low_byte = self.memory_read(address);
        let high_byte = self.memory_read(b);
        low_byte as u16 | (high_byte as u16) << 8
    }


    fn memory_write(&mut self, address: u16, byte: u8) {
        match address {
            0x0000...0x1FFF => self.ram[(address % 0x800) as usize] = byte,
            0x2000...0x3FFF => self.ppu.write(address, byte),
            0x4000...0x4013 => println!("APU Unimplemented"),
            0x4014 => self.oam_dma(byte),
            0x4016...0x4017 => println!("APU Unimplemented"),
            0x4016...0x4017 => println!("Gamepad unimplemented"),
            0x4018...0x401F => println!("Write to disabled memory area"),
            _ => (),
        }
    }

    fn oam_dma(&mut self, byte: u8) {
        let offset = (byte as u16) << 8;
        for i in 0..256 {
            let address = i + offset;
            let oam_byte = self.memory_read(address);
            self.ppu.write(0x2004, oam_byte);
        }
    }

    fn push(&mut self, byte: u8) {
        // Stack hard-coded to 0x100 - 0x1FF
        self.memory_write(0x100 + self.sp as u16, byte);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pop(&mut self) -> u8{
        self.sp += 1;
        self.memory_read(0x100 + self.sp as u16 - 1)
    }

    fn push_word(&mut self, word: u16) {
        let low_byte = (word & 0xff) as u8;
        let high_byte = ((word >> 8) & 0xff) as u8;

        self.push(high_byte);
        self.push(low_byte);
    }

    fn pop_byte(&mut self) -> u8 {
        self.sp += 1;
        self.memory_read(self.sp as u16 | 0x100)
    }

    fn pop_word(&mut self) -> u16 {
        let low_byte = self.pop_byte();
        let high_byte = self.pop_byte();

        ((high_byte as u16) << 8) | low_byte as u16
    }


    pub fn cycle(&mut self) {
        let byte = self.memory_read(self.pc);
        let fluff = Cpu::decode(byte);

        let decoded_address = self.decode_address(&fluff.mode);

        self.print_state(&fluff);

        let step = Step {
            address: decoded_address,
            mode: fluff.mode,
        };




        self.pc += fluff.bytes as u16;
        let exec = fluff.function;

        exec(self, &step);
    }

    fn print_state(&self, op: &Operation) {
        let mut op_bytes = String::new();

        for i in 0..op.bytes {
            op_bytes = op_bytes + &format!("{:02X} ", self.memory_read(self.pc + i as u16));
        }

        println!("{:04X}  {:<9} {:<31} A:{:02X} X:{:02X} Y:{:02X} P:?? SP:{:02X} PPU:???,??? CYC:???", self.pc, op_bytes, format!("{:?}", op.instruction), self.a, self.x, self.y, self.sp);
    }


    fn set_zn(&mut self, value: u8) {
        self.z = value == 0;
        self.n = (value as i8) < 0;
    }

    fn decode_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Accumulator => 0,
            AddressingMode::Implied => 0,
            AddressingMode::Immediate => self.pc + 1,
            AddressingMode::Absolute => self.read_word(self.pc + 1),
            AddressingMode::AbsoluteX => self.x as u16 + self.read_word(self.pc + 1),
            AddressingMode::AbsoluteY => (self.y as u16).wrapping_add(self.read_word(self.pc + 1)),
            AddressingMode::ZeroPage => self.memory_read(self.pc + 1) as u16,
            AddressingMode::ZeroPageX => self.memory_read(self.pc + 1 ).wrapping_add(self.x) as u16,
            AddressingMode::ZeroPageY => self.memory_read(self.pc + 1).wrapping_add(self.y) as u16,
            AddressingMode::Relative => {
                let offset = self.memory_read(self.pc + 1) as u16;
                if offset < 0x80 {
                    self.pc + 2 + offset
                } else {
                    self.pc + 2 + offset - 0x100
                }
            },
            AddressingMode::Indirect =>  self.read_word_bug(self.read_word(self.pc +1)),
            AddressingMode::IndirectIndexed =>  self.read_word_bug(self.memory_read(self.pc.wrapping_add(1)) as u16).wrapping_add(self.y as u16),
            AddressingMode::IndexedIndirect =>  self.read_word_bug(self.memory_read(self.pc.wrapping_add(1)) as u16).wrapping_add(self.x as u16),
            _ => panic!("Unimplemented addressing! {:?}", mode),
        }
    }

    fn decode(opcode: u8) -> Operation {
        match opcode {
            0 => Operation {
                function: Cpu::brk,
                instruction: Instruction::BRK,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 7,
            },
            1 => Operation {
                function: Cpu::ora,
                instruction: Instruction::ORA,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            2 => Operation {
                function: Cpu::kil,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            3 => Operation {
                function: Cpu::slo,
                instruction: Instruction::SLO,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 8,
            },
            4 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            5 => Operation {
                function: Cpu::ora,
                instruction: Instruction::ORA,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            6 => Operation {
                function: Cpu::asl,
                instruction: Instruction::ASL,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            7 => Operation {
                function: Cpu::slo,
                instruction: Instruction::SLO,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            8 => Operation {
                function: Cpu::php,
                instruction: Instruction::PHP,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 3,
            },
            9 => Operation {
                function: Cpu::ora,
                instruction: Instruction::ORA,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            10 => Operation {
                function: Cpu::asl,
                instruction: Instruction::ASL,
                mode: AddressingMode::Accumulator,
                bytes: 1,
                cycles: 2,
            },
            11 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ANC,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            12 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            13 => Operation {
                function: Cpu::ora,
                instruction: Instruction::ORA,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            14 => Operation {
                function: Cpu::asl,
                instruction: Instruction::ASL,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            15 => Operation {
                function: Cpu::slo,
                instruction: Instruction::SLO,
                mode: AddressingMode::Absolute,
                bytes: 2,
                cycles: 6,
            },
            16 => Operation {
                function: Cpu::bpl,
                instruction: Instruction::BPL,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            17 => Operation {
                function: Cpu::ora,
                instruction: Instruction::ORA,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 5,
            },
            18 => Operation {
                function: Cpu::kil,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            19 => Operation {
                function: Cpu::slo,
                instruction: Instruction::SLO,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 8,
            },
            20 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            21 => Operation {
                function: Cpu::ora,
                instruction: Instruction::ORA,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            22 => Operation {
                function: Cpu::asl,
                instruction: Instruction::ASL,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            23 => Operation {
                function: Cpu::slo,
                instruction: Instruction::SLO,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            24 => Operation {
                function: Cpu::clc,
                instruction: Instruction::CLC,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            25 => Operation {
                function: Cpu::ora,
                instruction: Instruction::ORA,
                mode: AddressingMode::AbsoluteY,
                bytes: 3,
                cycles: 4,
            },
            26 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            27 => Operation {
                function: Cpu::slo,
                instruction: Instruction::SLO,
                mode: AddressingMode::AbsoluteY,
                bytes: 2,
                cycles: 7,
            },
            28 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            29 => Operation {
                function: Cpu::ora,
                instruction: Instruction::ORA,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            30 => Operation {
                function: Cpu::asl,
                instruction: Instruction::ASL,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 7,
            },
            31 => Operation {
                function: Cpu::slo,
                instruction: Instruction::SLO,
                mode: AddressingMode::AbsoluteX,
                bytes: 2,
                cycles: 7,
            },
            32 => Operation {
                function: Cpu::jsr,
                instruction: Instruction::JSR,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            33 => Operation {
                function: Cpu::and,
                instruction: Instruction::AND,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            34 => Operation {
                function: Cpu::kil,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            35 => Operation {
                function: Cpu::rla,
                instruction: Instruction::RLA,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 8,
            },
            36 => Operation {
                function: Cpu::bit,
                instruction: Instruction::BIT,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            37 => Operation {
                function: Cpu::and,
                instruction: Instruction::AND,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            38 => Operation {
                function: Cpu::rol,
                instruction: Instruction::ROL,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            39 => Operation {
                function: Cpu::rla,
                instruction: Instruction::RLA,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            40 => Operation {
                function: Cpu::plp,
                instruction: Instruction::PLP,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 4,
            },
            41 => Operation {
                function: Cpu::and,
                instruction: Instruction::AND,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            42 => Operation {
                function: Cpu::rol,
                instruction: Instruction::ROL,
                mode: AddressingMode::Accumulator,
                bytes: 1,
                cycles: 2,
            },
            43 => Operation {
                function: Cpu::anc,
                instruction: Instruction::ANC,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            44 => Operation {
                function: Cpu::bit,
                instruction: Instruction::BIT,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            45 => Operation {
                function: Cpu::and,
                instruction: Instruction::AND,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            46 => Operation {
                function: Cpu::rol,
                instruction: Instruction::ROL,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            47 => Operation {
                function: Cpu::rla,
                instruction: Instruction::RLA,
                mode: AddressingMode::Absolute,
                bytes: 2,
                cycles: 6,
            },
            48 => Operation {
                function: Cpu::bmi,
                instruction: Instruction::BMI,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            49 => Operation {
                function: Cpu::and,
                instruction: Instruction::AND,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 5,
            },
            50 => Operation {
                function: Cpu::kil,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            51 => Operation {
                function: Cpu::rla,
                instruction: Instruction::RLA,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 8,
            },
            52 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            53 => Operation {
                function: Cpu::and,
                instruction: Instruction::AND,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            54 => Operation {
                function: Cpu::rol,
                instruction: Instruction::ROL,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            55 => Operation {
                function: Cpu::rla,
                instruction: Instruction::RLA,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            56 => Operation {
                function: Cpu::sec,
                instruction: Instruction::SEC,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            57 => Operation {
                function: Cpu::and,
                instruction: Instruction::AND,
                mode: AddressingMode::AbsoluteY,
                bytes: 3,
                cycles: 4,
            },
            58 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            59 => Operation {
                function: Cpu::rla,
                instruction: Instruction::RLA,
                mode: AddressingMode::AbsoluteY,
                bytes: 2,
                cycles: 7,
            },
            60 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            61 => Operation {
                function: Cpu::and,
                instruction: Instruction::AND,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            62 => Operation {
                function: Cpu::rol,
                instruction: Instruction::ROL,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 7,
            },
            63 => Operation {
                function: Cpu::rla,
                instruction: Instruction::RLA,
                mode: AddressingMode::AbsoluteX,
                bytes: 2,
                cycles: 7,
            },
            64 => Operation {
                function: Cpu::rti,
                instruction: Instruction::RTI,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 6,
            },
            65 => Operation {
                function: Cpu::eor,
                instruction: Instruction::EOR,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            66 => Operation {
                function: Cpu::kil,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            67 => Operation {
                function: Cpu::sre,
                instruction: Instruction::SRE,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 8,
            },
            68 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            69 => Operation {
                function: Cpu::eor,
                instruction: Instruction::EOR,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            70 => Operation {
                function: Cpu::lsr,
                instruction: Instruction::LSR,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            71 => Operation {
                function: Cpu::sre,
                instruction: Instruction::SRE,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            72 => Operation {
                function: Cpu::pha,
                instruction: Instruction::PHA,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 3,
            },
            73 => Operation {
                function: Cpu::eor,
                instruction: Instruction::EOR,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            74 => Operation {
                function: Cpu::lsr,
                instruction: Instruction::LSR,
                mode: AddressingMode::Accumulator,
                bytes: 1,
                cycles: 2,
            },
            75 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ALR,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            76 => Operation {
                function: Cpu::jmp,
                instruction: Instruction::JMP,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 3,
            },
            77 => Operation {
                function: Cpu::eor,
                instruction: Instruction::EOR,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            78 => Operation {
                function: Cpu::lsr,
                instruction: Instruction::LSR,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            79 => Operation {
                function: Cpu::sre,
                instruction: Instruction::SRE,
                mode: AddressingMode::Absolute,
                bytes: 2,
                cycles: 6,
            },
            80 => Operation {
                function: Cpu::bvc,
                instruction: Instruction::BVC,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            81 => Operation {
                function: Cpu::eor,
                instruction: Instruction::EOR,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 5,
            },
            82 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            83 => Operation {
                function: Cpu::sre,
                instruction: Instruction::SRE,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 8,
            },
            84 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            85 => Operation {
                function: Cpu::eor,
                instruction: Instruction::EOR,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            86 => Operation {
                function: Cpu::lsr,
                instruction: Instruction::LSR,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            87 => Operation {
                function: Cpu::sre,
                instruction: Instruction::SRE,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            88 => Operation {
                function: Cpu::cli,
                instruction: Instruction::CLI,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            89 => Operation {
                function: Cpu::eor,
                instruction: Instruction::EOR,
                mode: AddressingMode::AbsoluteY,
                bytes: 3,
                cycles: 4,
            },
            90 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            91 => Operation {
                function: Cpu::sre,
                instruction: Instruction::SRE,
                mode: AddressingMode::AbsoluteY,
                bytes: 2,
                cycles: 7,
            },
            92 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            93 => Operation {
                function: Cpu::eor,
                instruction: Instruction::EOR,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            94 => Operation {
                function: Cpu::lsr,
                instruction: Instruction::LSR,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 7,
            },
            95 => Operation {
                function: Cpu::sre,
                instruction: Instruction::SRE,
                mode: AddressingMode::AbsoluteX,
                bytes: 2,
                cycles: 7,
            },
            96 => Operation {
                function: Cpu::rts,
                instruction: Instruction::RTS,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 6,
            },
            97 => Operation {
                function: Cpu::adc,
                instruction: Instruction::ADC,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            98 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            99 => Operation {
                function: Cpu::rra,
                instruction: Instruction::RRA,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 8,
            },
            100 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            101 => Operation {
                function: Cpu::adc,
                instruction: Instruction::ADC,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            102 => Operation {
                function: Cpu::ror,
                instruction: Instruction::ROR,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            103 => Operation {
                function: Cpu::rra,
                instruction: Instruction::RRA,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            104 => Operation {
                function: Cpu::pla,
                instruction: Instruction::PLA,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 4,
            },
            105 => Operation {
                function: Cpu::adc,
                instruction: Instruction::ADC,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            106 => Operation {
                function: Cpu::ror,
                instruction: Instruction::ROR,
                mode: AddressingMode::Accumulator,
                bytes: 1,
                cycles: 2,
            },
            107 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ARR,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            108 => Operation {
                function: Cpu::jmp,
                instruction: Instruction::JMP,
                mode: AddressingMode::Indirect,
                bytes: 3,
                cycles: 5,
            },
            109 => Operation {
                function: Cpu::adc,
                instruction: Instruction::ADC,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            110 => Operation {
                function: Cpu::ror,
                instruction: Instruction::ROR,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            111 => Operation {
                function: Cpu::rra,
                instruction: Instruction::RRA,
                mode: AddressingMode::Absolute,
                bytes: 2,
                cycles: 6,
            },
            112 => Operation {
                function: Cpu::bvs,
                instruction: Instruction::BVS,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            113 => Operation {
                function: Cpu::adc,
                instruction: Instruction::ADC,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 5,
            },
            114 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            115 => Operation {
                function: Cpu::rra,
                instruction: Instruction::RRA,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 8,
            },
            116 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            117 => Operation {
                function: Cpu::adc,
                instruction: Instruction::ADC,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            118 => Operation {
                function: Cpu::ror,
                instruction: Instruction::ROR,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            119 => Operation {
                function: Cpu::rra,
                instruction: Instruction::RRA,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            120 => Operation {
                function: Cpu::sei,
                instruction: Instruction::SEI,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            121 => Operation {
                function: Cpu::adc,
                instruction: Instruction::ADC,
                mode: AddressingMode::AbsoluteY,
                bytes: 3,
                cycles: 4,
            },
            122 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            123 => Operation {
                function: Cpu::rra,
                instruction: Instruction::RRA,
                mode: AddressingMode::AbsoluteY,
                bytes: 2,
                cycles: 7,
            },
            124 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            125 => Operation {
                function: Cpu::adc,
                instruction: Instruction::ADC,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            126 => Operation {
                function: Cpu::ror,
                instruction: Instruction::ROR,
                mode: AddressingMode::ZeroPage,
                bytes: 3,
                cycles: 7,
            },
            127 => Operation {
                function: Cpu::rra,
                instruction: Instruction::RRA,
                mode: AddressingMode::AbsoluteX,
                bytes: 2,
                cycles: 7,
            },
            128 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            129 => Operation {
                function: Cpu::sta,
                instruction: Instruction::STA,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            130 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            131 => Operation {
                function: Cpu::sax,
                instruction: Instruction::SAX,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            132 => Operation {
                function: Cpu::sty,
                instruction: Instruction::STY,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            133 => Operation {
                function: Cpu::sta,
                instruction: Instruction::STA,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            134 => Operation {
                function: Cpu::stx,
                instruction: Instruction::STX,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            135 => Operation {
                function: Cpu::sax,
                instruction: Instruction::SAX,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            136 => Operation {
                function: Cpu::dey,
                instruction: Instruction::DEY,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            137 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            138 => Operation {
                function: Cpu::txa,
                instruction: Instruction::TXA,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            139 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::XAA,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            140 => Operation {
                function: Cpu::sty,
                instruction: Instruction::STY,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            141 => Operation {
                function: Cpu::sta,
                instruction: Instruction::STA,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            142 => Operation {
                function: Cpu::stx,
                instruction: Instruction::STX,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            143 => Operation {
                function: Cpu::sax,
                instruction: Instruction::SAX,
                mode: AddressingMode::Absolute,
                bytes: 2,
                cycles: 4,
            },
            144 => Operation {
                function: Cpu::bcc,
                instruction: Instruction::BCC,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            145 => Operation {
                function: Cpu::sta,
                instruction: Instruction::STA,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 6,
            },
            146 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            147 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::AHX,
                mode: AddressingMode::IndirectIndexed,
                bytes: 0,
                cycles: 6,
            },
            148 => Operation {
                function: Cpu::sty,
                instruction: Instruction::STY,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            149 => Operation {
                function: Cpu::sta,
                instruction: Instruction::STA,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            150 => Operation {
                function: Cpu::stx,
                instruction: Instruction::STX,
                mode: AddressingMode::ZeroPageY,
                bytes: 2,
                cycles: 4,
            },
            151 => Operation {
                function: Cpu::sax,
                instruction: Instruction::SAX,
                mode: AddressingMode::ZeroPageY,
                bytes: 2,
                cycles: 4,
            },
            152 => Operation {
                function: Cpu::tya,
                instruction: Instruction::TYA,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            153 => Operation {
                function: Cpu::sta,
                instruction: Instruction::STA,
                mode: AddressingMode::AbsoluteY,
                bytes: 3,
                cycles: 5,
            },
            154 => Operation {
                function: Cpu::txs,
                instruction: Instruction::TXS,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            155 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::TAS,
                mode: AddressingMode::AbsoluteY,
                bytes: 0,
                cycles: 5,
            },
            156 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SHY,
                mode: AddressingMode::AbsoluteX,
                bytes: 0,
                cycles: 5,
            },
            157 => Operation {
                function: Cpu::sta,
                instruction: Instruction::STA,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 5,
            },
            158 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SHX,
                mode: AddressingMode::AbsoluteY,
                bytes: 0,
                cycles: 5,
            },
            159 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::AHX,
                mode: AddressingMode::AbsoluteY,
                bytes: 0,
                cycles: 5,
            },
            160 => Operation {
                function: Cpu::ldy,
                instruction: Instruction::LDY,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            161 => Operation {
                function: Cpu::lda,
                instruction: Instruction::LDA,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            162 => Operation {
                function: Cpu::ldx,
                instruction: Instruction::LDX,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            163 => Operation {
                function: Cpu::lax,
                instruction: Instruction::LAX,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            164 => Operation {
                function: Cpu::ldy,
                instruction: Instruction::LDY,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            165 => Operation {
                function: Cpu::lda,
                instruction: Instruction::LDA,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            166 => Operation {
                function: Cpu::ldx,
                instruction: Instruction::LDX,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            167 => Operation {
                function: Cpu::lax,
                instruction: Instruction::LAX,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            168 => Operation {
                function: Cpu::tay,
                instruction: Instruction::TAY,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            169 => Operation {
                function: Cpu::lda,
                instruction: Instruction::LDA,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            170 => Operation {
                function: Cpu::tax,
                instruction: Instruction::TAX,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            171 => Operation {
                function: Cpu::lax,
                instruction: Instruction::LAX,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            172 => Operation {
                function: Cpu::ldy,
                instruction: Instruction::LDY,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            173 => Operation {
                function: Cpu::lda,
                instruction: Instruction::LDA,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            174 => Operation {
                function: Cpu::ldx,
                instruction: Instruction::LDX,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            175 => Operation {
                function: Cpu::lax,
                instruction: Instruction::LAX,
                mode: AddressingMode::Absolute,
                bytes: 2,
                cycles: 4,
            },
            176 => Operation {
                function: Cpu::bcs,
                instruction: Instruction::BCS,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            177 => Operation {
                function: Cpu::lda,
                instruction: Instruction::LDA,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 5,
            },
            178 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            179 => Operation {
                function: Cpu::lax,
                instruction: Instruction::LAX,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 5,
            },
            180 => Operation {
                function: Cpu::ldy,
                instruction: Instruction::LDY,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            181 => Operation {
                function: Cpu::lda,
                instruction: Instruction::LDA,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            182 => Operation {
                function: Cpu::ldx,
                instruction: Instruction::LDX,
                mode: AddressingMode::ZeroPageY,
                bytes: 2,
                cycles: 4,
            },
            183 => Operation {
                function: Cpu::lax,
                instruction: Instruction::LAX,
                mode: AddressingMode::ZeroPageY,
                bytes: 2,
                cycles: 4,
            },
            184 => Operation {
                function: Cpu::clv,
                instruction: Instruction::CLV,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            185 => Operation {
                function: Cpu::lda,
                instruction: Instruction::LDA,
                mode: AddressingMode::AbsoluteY,
                bytes: 3,
                cycles: 4,
            },
            186 => Operation {
                function: Cpu::tsx,
                instruction: Instruction::TSX,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            187 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::LAS,
                mode: AddressingMode::AbsoluteY,
                bytes: 0,
                cycles: 4,
            },
            188 => Operation {
                function: Cpu::ldy,
                instruction: Instruction::LDY,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            189 => Operation {
                function: Cpu::lda,
                instruction: Instruction::LDA,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            190 => Operation {
                function: Cpu::ldx,
                instruction: Instruction::LDX,
                mode: AddressingMode::AbsoluteY,
                bytes: 3,
                cycles: 4,
            },
            191 => Operation {
                function: Cpu::lax,
                instruction: Instruction::LAX,
                mode: AddressingMode::AbsoluteY,
                bytes: 2,
                cycles: 4,
            },
            192 => Operation {
                function: Cpu::cpy,
                instruction: Instruction::CPY,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            193 => Operation {
                function: Cpu::cmp,
                instruction: Instruction::CMP,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            194 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            195 => Operation {
                function: Cpu::dcp,
                instruction: Instruction::DCP,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 8,
            },
            196 => Operation {
                function: Cpu::cpy,
                instruction: Instruction::CPY,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            197 => Operation {
                function: Cpu::cmp,
                instruction: Instruction::CMP,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            198 => Operation {
                function: Cpu::dec,
                instruction: Instruction::DEC,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            199 => Operation {
                function: Cpu::dcp,
                instruction: Instruction::DCP,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            200 => Operation {
                function: Cpu::iny,
                instruction: Instruction::INY,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            201 => Operation {
                function: Cpu::cmp,
                instruction: Instruction::CMP,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            202 => Operation {
                function: Cpu::dex,
                instruction: Instruction::DEX,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            203 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::AXS,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            204 => Operation {
                function: Cpu::cpy,
                instruction: Instruction::CPY,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            205 => Operation {
                function: Cpu::cmp,
                instruction: Instruction::CMP,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            206 => Operation {
                function: Cpu::dec,
                instruction: Instruction::DEC,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            207 => Operation {
                function: Cpu::dcp,
                instruction: Instruction::DCP,
                mode: AddressingMode::Absolute,
                bytes: 2,
                cycles: 6,
            },
            208 => Operation {
                function: Cpu::bne,
                instruction: Instruction::BNE,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            209 => Operation {
                function: Cpu::cmp,
                instruction: Instruction::CMP,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 5,
            },
            210 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            211 => Operation {
                function: Cpu::dcp,
                instruction: Instruction::DCP,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 8,
            },
            212 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            213 => Operation {
                function: Cpu::cmp,
                instruction: Instruction::CMP,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            214 => Operation {
                function: Cpu::dec,
                instruction: Instruction::DEC,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            215 => Operation {
                function: Cpu::dcp,
                instruction: Instruction::DCP,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            216 => Operation {
                function: Cpu::cld,
                instruction: Instruction::CLD,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            217 => Operation {
                function: Cpu::cmp,
                instruction: Instruction::CMP,
                mode: AddressingMode::AbsoluteY,
                bytes: 3,
                cycles: 4,
            },
            218 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            219 => Operation {
                function: Cpu::dcp,
                instruction: Instruction::DCP,
                mode: AddressingMode::AbsoluteY,
                bytes: 2,
                cycles: 7,
            },
            220 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            221 => Operation {
                function: Cpu::cmp,
                instruction: Instruction::CMP,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            222 => Operation {
                function: Cpu::dec,
                instruction: Instruction::DEC,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 7,
            },
            223 => Operation {
                function: Cpu::dcp,
                instruction: Instruction::DCP,
                mode: AddressingMode::AbsoluteX,
                bytes: 2,
                cycles: 7,
            },
            224 => Operation {
                function: Cpu::cpx,
                instruction: Instruction::CPX,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            225 => Operation {
                function: Cpu::sbc,
                instruction: Instruction::SBC,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            226 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            227 => Operation {
                function: Cpu::isc,
                instruction: Instruction::ISC,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 8,
            },
            228 => Operation {
                function: Cpu::cpx,
                instruction: Instruction::CPX,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            229 => Operation {
                function: Cpu::sbc,
                instruction: Instruction::SBC,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            230 => Operation {
                function: Cpu::inc,
                instruction: Instruction::INC,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            231 => Operation {
                function: Cpu::isc,
                instruction: Instruction::ISC,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            232 => Operation {
                function: Cpu::inx,
                instruction: Instruction::INX,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            233 => Operation {
                function: Cpu::sbc,
                instruction: Instruction::SBC,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            234 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            235 => Operation {
                function: Cpu::sbc,
                instruction: Instruction::SBC,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            236 => Operation {
                function: Cpu::cpx,
                instruction: Instruction::CPX,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            237 => Operation {
                function: Cpu::sbc,
                instruction: Instruction::SBC,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            238 => Operation {
                function: Cpu::inc,
                instruction: Instruction::INC,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            239 => Operation {
                function: Cpu::isc,
                instruction: Instruction::ISC,
                mode: AddressingMode::Absolute,
                bytes: 2,
                cycles: 6,
            },
            240 => Operation {
                function: Cpu::beq,
                instruction: Instruction::BEQ,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            241 => Operation {
                function: Cpu::sbc,
                instruction: Instruction::SBC,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 5,
            },
            242 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            243 => Operation {
                function: Cpu::isc,
                instruction: Instruction::ISC,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 8,
            },
            244 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            245 => Operation {
                function: Cpu::sbc,
                instruction: Instruction::SBC,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            246 => Operation {
                function: Cpu::inc,
                instruction: Instruction::INC,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            247 => Operation {
                function: Cpu::isc,
                instruction: Instruction::ISC,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            248 => Operation {
                function: Cpu::sed,
                instruction: Instruction::SED,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            249 => Operation {
                function: Cpu::sbc,
                instruction: Instruction::SBC,
                mode: AddressingMode::AbsoluteY,
                bytes: 3,
                cycles: 4,
            },
            250 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            251 => Operation {
                function: Cpu::isc,
                instruction: Instruction::ISC,
                mode: AddressingMode::AbsoluteY,
                bytes: 2,
                cycles: 7,
            },
            252 => Operation {
                function: Cpu::nop,
                instruction: Instruction::NOP,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            253 => Operation {
                function: Cpu::sbc,
                instruction: Instruction::SBC,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            254 => Operation {
                function: Cpu::inc,
                instruction: Instruction::INC,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 7,
            },
            255 => Operation {
                function: Cpu::isc,
                instruction: Instruction::ISC,
                mode: AddressingMode::AbsoluteX,
                bytes: 2,
                cycles: 7,
            },
        }
    }

    // ILLEGAL OpCode
    fn ahx(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn alr(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn anc(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn arr(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn axs(&mut self, _step: &Step) {
    }

    // dcp OpCode
    fn dcp(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn isc(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn kil(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn las(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn rla(&mut self, _step: &Step) {
    }

    // rra OpCode
    fn rra(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn sax(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn shx(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn shy(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn slo(&mut self, _step: &Step) {
    }

    // sre OpCode
    fn sre(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn tas(&mut self, _step: &Step) {
    }

    // ILLEGAL OpCode
    fn xaa(&mut self, _step: &Step) {
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adc_regular() {
        let mut cpu = Cpu::new();
        cpu.a = 12;
        cpu.memory_write(100, 32);
        cpu.adc(&Step {
            address: 100,
            mode: AddressingMode::Implied,
        });
        assert_eq!(cpu.a, 44);
        assert_eq!(cpu.z, false);
        assert_eq!(cpu.c, false);
        assert_eq!(cpu.v, false);
    }

    #[test]
    fn adc_overflow() {
        let mut cpu = Cpu::new();
        cpu.a = 0xff;
        cpu.memory_write(100, 1);
        cpu.adc(&Step {
            address: 100,
            mode: AddressingMode::Implied,
        });
        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.z, true);
        assert_eq!(cpu.c, true);
        assert_eq!(cpu.v, true);
    }

    #[test]
    fn decode_adc() {
        let x = 0x69_u8;
        let op = Cpu::decode(x);
        assert_eq!(op.instruction, Instruction::ADC);
        assert_eq!(op.mode, AddressingMode::Immediate);
    }

    #[test]
    fn jsr() {
        let mut cpu = Cpu::new();
        cpu.sp = 0xFF;
        cpu.pc = 0x1237;
        cpu.jsr(&Step { address: 0xBEEF, mode: AddressingMode::Absolute });

        assert_eq!(cpu.pc, 0xBEEF);
        assert_eq!(cpu.sp, 0xFD);
        assert_eq!(cpu.read_word(0x01FE), 0x1236);
    }

    #[test]
    fn rts() {
        let mut cpu = Cpu::new();
        cpu.pc = 0x1234;
        cpu.sp = 0xFD;
        cpu.memory_write(0x01FE, 0xEE);
        cpu.memory_write(0x01FF, 0xBE);
        cpu.rts(&Step::new());

        assert_eq!(cpu.pc, 0xBEEF);
        assert_eq!(cpu.sp, 0xFF);
    }
}

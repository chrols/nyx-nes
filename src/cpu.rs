// use std::num::wrapping::Wrapping;

use std::fmt;
use super::ines;

pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    status: u8,
    ram: [u8; 0x800],
    c: bool, // Carry flag
    z: bool, // Zero flag
    i: bool, // Interrupt disable
    d: bool, // Decimal mode flag
    b: bool, // Break command
    v: bool, // Overflow flag
    n: bool, // Negative flag
    pub rom: Option<ines::File>,
}

impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "A: {} X: {} Y: {} PC: {}, SP: {}",
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
    pc: u16,
    mode: AddressingMode,
}

type CpuFunction = fn(&mut Cpu, &Step);

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0,
            status: 0,
            ram: [0; 0x800],
            c: false,
            z: false,
            i: false,
            d: false,
            b: false,
            v: false,
            n: false,
            rom: None,
        }
    }

    pub fn reset(&mut self) {
        self.pc = self.read_word(0xfffc);
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

    // BEQ - Branch if Equal
    fn bit(&mut self, step: &Step) {
        if self.z {
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
    fn brk(&mut self, step: &Step) {
        panic!("Unimplemented");
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

    // No operation
    fn nop(&mut self, step: &Step) {}

    // Clear decimal mode
    fn cld(&mut self, step: &Step) {
        self.d = false;
    }

    // Decrement X register
    fn dex(&mut self, step: &Step) {
        self.x = self.x.wrapping_sub(1);
        self.set_zn(self.x);
    }

    // Decrement Y register
    fn dey(&mut self, step: &Step) {
        self.y = self.y.wrapping_sub(1);
        self.set_zn(self.y);
    }

    // Set interrupt disable
    fn sei(&mut self, step: &Step) {
        self.i = true;
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

    // Jump to Subroutine
    fn jsr(&mut self, step: &Step) {}

    // Transfer X to Stack Pointer
    fn txs(&mut self, step: &Step) {
        self.sp = self.x;
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

    // Halt and catch fire
    fn hcf(&mut self, step: &Step) {
        panic!("Halt and catch fire!");
    }

    fn memory_read(&self, address: u16) -> u8 {
        println!("Addr: 0x{:X}", address);
        match address {
            0x0000...0x1FFF => self.ram[(address % 0x800) as usize],
            0x2000...0x3FFF => {
                println!("PPU Unimplemented");
                0x80
            }
            0x4000...0x401F => panic!("APU Unimplemented"),
            0x4020...0xFFFF => match &self.rom {
                Some(game) => match game.mapper {
                    0 => game.prg_rom[(address as usize - 0xC000)],
                    _ => panic!("Unimplemented mapper"),
                },
                None => panic!("No game loaded"),
            },
            _ => 0,
        }
    }

    fn read_word(&self, address: u16) -> u16 {
        let low_byte = self.memory_read(address);
        let high_byte = self.memory_read(address + 1);
        low_byte as u16 | (high_byte as u16) << 8
    }

    fn memory_write(&mut self, address: u16, byte: u8) {
        match address {
            0x0000...0x1FFF => self.ram[(address % 0x800) as usize] = byte,
            _ => (),
        }
    }

    pub fn cycle(&mut self) {
        let byte = self.memory_read(self.pc);
        println!("{:X}", byte);
        let fluff = Cpu::decode(byte);
        println!("{:?}", fluff.instruction);
        println!("{:?}", self);        

        let decoded_address = self.decode_address(&fluff.mode);

        let step = Step {
            address: decoded_address,
            pc: self.pc,
            mode: fluff.mode,
        };

        let exec = fluff.function;
        exec(self, &step);

        self.pc += fluff.bytes as u16;
    }

    fn set_zn(&mut self, value: u8) {
        self.z = value == 0;
        self.n = (value as i8) < 0;
    }

    fn decode_address(&self, mode: &AddressingMode) -> u16 {
        println!("{:?}", mode);
        match mode {
            AddressingMode::Implied => self.pc,
            AddressingMode::Immediate => self.pc + 1,
            AddressingMode::Absolute => self.read_word(self.pc + 1),
            AddressingMode::Relative => {
                let offset = self.memory_read(self.pc + 1) as u16;
                if offset > 0x80 {
                    self.pc + offset - 0x100
                } else {
                    self.pc + offset
                }
            }
            _ => panic!("Unimplemented addressing!"),
        }
    }

    fn decode(opcode: u8) -> Operation {
        match opcode {
            0 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::BRK,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 7,
            },
            1 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ORA,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            2 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            3 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SLO,
                mode: AddressingMode::IndexedIndirect,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::ORA,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            6 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ASL,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            7 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SLO,
                mode: AddressingMode::ZeroPage,
                bytes: 0,
                cycles: 5,
            },
            8 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::PHP,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 3,
            },
            9 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ORA,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            10 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::ORA,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            14 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ASL,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            15 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SLO,
                mode: AddressingMode::Absolute,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::ORA,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 5,
            },
            18 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            19 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SLO,
                mode: AddressingMode::IndirectIndexed,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::ORA,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            22 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ASL,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            23 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SLO,
                mode: AddressingMode::ZeroPageX,
                bytes: 0,
                cycles: 6,
            },
            24 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::CLC,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            25 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::SLO,
                mode: AddressingMode::AbsoluteY,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::ORA,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            30 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ASL,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 7,
            },
            31 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SLO,
                mode: AddressingMode::AbsoluteX,
                bytes: 0,
                cycles: 7,
            },
            32 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::JSR,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            33 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::AND,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            34 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            35 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RLA,
                mode: AddressingMode::IndexedIndirect,
                bytes: 0,
                cycles: 8,
            },
            36 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::BIT,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            37 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::AND,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            38 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ROL,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            39 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RLA,
                mode: AddressingMode::ZeroPage,
                bytes: 0,
                cycles: 5,
            },
            40 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::PLP,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 4,
            },
            41 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::AND,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            42 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ROL,
                mode: AddressingMode::Accumulator,
                bytes: 1,
                cycles: 2,
            },
            43 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ANC,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            44 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::BIT,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            45 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::AND,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            46 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ROL,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            47 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RLA,
                mode: AddressingMode::Absolute,
                bytes: 0,
                cycles: 6,
            },
            48 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::BMI,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            49 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::AND,
                mode: AddressingMode::IndirectIndexed,
                bytes: 2,
                cycles: 5,
            },
            50 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            51 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RLA,
                mode: AddressingMode::IndirectIndexed,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::AND,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            54 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ROL,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            55 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RLA,
                mode: AddressingMode::ZeroPageX,
                bytes: 0,
                cycles: 6,
            },
            56 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SEC,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            57 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::RLA,
                mode: AddressingMode::AbsoluteY,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::AND,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            62 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ROL,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 7,
            },
            63 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RLA,
                mode: AddressingMode::AbsoluteX,
                bytes: 0,
                cycles: 7,
            },
            64 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RTI,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 6,
            },
            65 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::EOR,
                mode: AddressingMode::IndexedIndirect,
                bytes: 2,
                cycles: 6,
            },
            66 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::KIL,
                mode: AddressingMode::Implied,
                bytes: 0,
                cycles: 2,
            },
            67 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SRE,
                mode: AddressingMode::IndexedIndirect,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::EOR,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            70 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::LSR,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            71 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SRE,
                mode: AddressingMode::ZeroPage,
                bytes: 0,
                cycles: 5,
            },
            72 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::PHA,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 3,
            },
            73 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::EOR,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            74 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::JMP,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 3,
            },
            77 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::EOR,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            78 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::LSR,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            79 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SRE,
                mode: AddressingMode::Absolute,
                bytes: 0,
                cycles: 6,
            },
            80 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::BVC,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            81 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::SRE,
                mode: AddressingMode::IndirectIndexed,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::EOR,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            86 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::LSR,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            87 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SRE,
                mode: AddressingMode::ZeroPageX,
                bytes: 0,
                cycles: 6,
            },
            88 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::CLI,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            89 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::SRE,
                mode: AddressingMode::AbsoluteY,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::EOR,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            94 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::LSR,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 7,
            },
            95 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SRE,
                mode: AddressingMode::AbsoluteX,
                bytes: 0,
                cycles: 7,
            },
            96 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RTS,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 6,
            },
            97 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::RRA,
                mode: AddressingMode::IndexedIndirect,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::ADC,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            102 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ROR,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            103 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RRA,
                mode: AddressingMode::ZeroPage,
                bytes: 0,
                cycles: 5,
            },
            104 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::PLA,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 4,
            },
            105 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ADC,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            106 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::JMP,
                mode: AddressingMode::Indirect,
                bytes: 3,
                cycles: 5,
            },
            109 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ADC,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            110 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ROR,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            111 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RRA,
                mode: AddressingMode::Absolute,
                bytes: 0,
                cycles: 6,
            },
            112 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::BVS,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            113 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::RRA,
                mode: AddressingMode::IndirectIndexed,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::ADC,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            118 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ROR,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            119 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RRA,
                mode: AddressingMode::ZeroPageX,
                bytes: 0,
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
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::RRA,
                mode: AddressingMode::AbsoluteY,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::ADC,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            126 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ROR,
                mode: AddressingMode::ZeroPage,
                bytes: 3,
                cycles: 7,
            },
            127 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::RRA,
                mode: AddressingMode::AbsoluteX,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::SAX,
                mode: AddressingMode::IndexedIndirect,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::SAX,
                mode: AddressingMode::ZeroPage,
                bytes: 0,
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
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::SAX,
                mode: AddressingMode::Absolute,
                bytes: 0,
                cycles: 4,
            },
            144 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::SAX,
                mode: AddressingMode::ZeroPageY,
                bytes: 0,
                cycles: 4,
            },
            152 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::LAX,
                mode: AddressingMode::IndexedIndirect,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::LAX,
                mode: AddressingMode::ZeroPage,
                bytes: 0,
                cycles: 3,
            },
            168 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::TAX,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            171 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::LAX,
                mode: AddressingMode::Immediate,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::LAX,
                mode: AddressingMode::Absolute,
                bytes: 0,
                cycles: 4,
            },
            176 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::LAX,
                mode: AddressingMode::IndirectIndexed,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::LAX,
                mode: AddressingMode::ZeroPageY,
                bytes: 0,
                cycles: 4,
            },
            184 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::LAX,
                mode: AddressingMode::AbsoluteY,
                bytes: 0,
                cycles: 4,
            },
            192 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::CPY,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            193 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::DCP,
                mode: AddressingMode::IndexedIndirect,
                bytes: 0,
                cycles: 8,
            },
            196 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::CPY,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            197 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::CMP,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            198 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::DEC,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            199 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::DCP,
                mode: AddressingMode::ZeroPage,
                bytes: 0,
                cycles: 5,
            },
            200 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::INY,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            201 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::CPY,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            205 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::CMP,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            206 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::DEC,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            207 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::DCP,
                mode: AddressingMode::Absolute,
                bytes: 0,
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
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::DCP,
                mode: AddressingMode::IndirectIndexed,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::CMP,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            214 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::DEC,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            215 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::DCP,
                mode: AddressingMode::ZeroPageX,
                bytes: 0,
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
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::DCP,
                mode: AddressingMode::AbsoluteY,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::CMP,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            222 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::DEC,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 7,
            },
            223 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::DCP,
                mode: AddressingMode::AbsoluteX,
                bytes: 0,
                cycles: 7,
            },
            224 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::CPX,
                mode: AddressingMode::Immediate,
                bytes: 2,
                cycles: 2,
            },
            225 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::ISC,
                mode: AddressingMode::IndexedIndirect,
                bytes: 0,
                cycles: 8,
            },
            228 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::CPX,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            229 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SBC,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 3,
            },
            230 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::INC,
                mode: AddressingMode::ZeroPage,
                bytes: 2,
                cycles: 5,
            },
            231 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ISC,
                mode: AddressingMode::ZeroPage,
                bytes: 0,
                cycles: 5,
            },
            232 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::INX,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            233 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::SBC,
                mode: AddressingMode::Immediate,
                bytes: 0,
                cycles: 2,
            },
            236 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::CPX,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            237 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SBC,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 4,
            },
            238 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::INC,
                mode: AddressingMode::Absolute,
                bytes: 3,
                cycles: 6,
            },
            239 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ISC,
                mode: AddressingMode::Absolute,
                bytes: 0,
                cycles: 6,
            },
            240 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::BEQ,
                mode: AddressingMode::Relative,
                bytes: 2,
                cycles: 2,
            },
            241 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::ISC,
                mode: AddressingMode::IndirectIndexed,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::SBC,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 4,
            },
            246 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::INC,
                mode: AddressingMode::ZeroPageX,
                bytes: 2,
                cycles: 6,
            },
            247 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ISC,
                mode: AddressingMode::ZeroPageX,
                bytes: 0,
                cycles: 6,
            },
            248 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::SED,
                mode: AddressingMode::Implied,
                bytes: 1,
                cycles: 2,
            },
            249 => Operation {
                function: Cpu::hcf,
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
                function: Cpu::hcf,
                instruction: Instruction::ISC,
                mode: AddressingMode::AbsoluteY,
                bytes: 0,
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
                function: Cpu::hcf,
                instruction: Instruction::SBC,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 4,
            },
            254 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::INC,
                mode: AddressingMode::AbsoluteX,
                bytes: 3,
                cycles: 7,
            },
            255 => Operation {
                function: Cpu::hcf,
                instruction: Instruction::ISC,
                mode: AddressingMode::AbsoluteX,
                bytes: 0,
                cycles: 7,
            },
        }
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
            pc: 0,
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
            pc: 0,
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

}

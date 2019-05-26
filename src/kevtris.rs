pub fn translate_02(byte: u8) -> &'static str {
    match byte {
        0x00 => return "tests completed successfully",
        0x01 => return "BCS failed to branch",
        0x02 => return "BCS branched when it shouldn't have",
        0x03 => return "BCC branched when it shouldn't have",
        0x04 => return "BCC failed to branch",
        0x05 => return "BEQ failed to branch",
        0x06 => return "BEQ branched when it shouldn't have",
        0x07 => return "BNE failed to branch",
        0x08 => return "BNE branched when it shouldn't have",
        0x09 => return "BVS failed to branch",
        0x0A => return "BVC branched when it shouldn't have",
        0x0B => return "BVC failed to branch",
        0x0C => return "BVS branched when it shouldn't have",
        0x0D => return "BPL failed to branch",
        0x0E => return "BPL branched when it shouldn't have",
        0x0F => return "BMI failed to branch",
        0x10 => return "BMI branched when it shouldn't have",
        0x11 => return "PHP/flags failure (bits set)",
        0x12 => return "PHP/flags failure (bits clear)",
        0x13 => return "PHP/flags failure (misc bit states)",
        0x14 => return "PLP/flags failure (misc bit states)",
        0x15 => return "PLP/flags failure (misc bit states)",
        0x16 => return "PHA/PLA failure (PLA didn't affect Z and N properly)",
        0x17 => return "PHA/PLA failure (PLA didn't affect Z and N properly)",
        0x18 => return "ORA # failure",
        0x19 => return "ORA # failure",
        0x1A => return "AND # failure",
        0x1B => return "AND # failure",
        0x1C => return "EOR # failure",
        0x1D => return "EOR # failure",
        0x1E => return "ADC # failure (overflow/carry problems)",
        0x1F => return "ADC # failure (decimal mode was turned on)",
        0x20 => return "ADC # failure",
        0x21 => return "ADC # failure",
        0x22 => return "ADC # failure",
        0x23 => return "LDA # failure (didn't set N and Z correctly)",
        0x24 => return "LDA # failure (didn't set N and Z correctly)",
        0x25 => return "CMP # failure (messed up flags)",
        0x26 => return "CMP # failure (messed up flags)",
        0x27 => return "CMP # failure (messed up flags)",
        0x28 => return "CMP # failure (messed up flags)",
        0x29 => return "CMP # failure (messed up flags)",
        0x2A => return "CMP # failure (messed up flags)",
        0x2B => return "CPY # failure (messed up flags)",
        0x2C => return "CPY # failure (messed up flags)",
        0x2D => return "CPY # failure (messed up flags)",
        0x2E => return "CPY # failure (messed up flags)",
        0x2F => return "CPY # failure (messed up flags)",
        0x30 => return "CPY # failure (messed up flags)",
        0x31 => return "CPY # failure (messed up flags)",
        0x32 => return "CPX # failure (messed up flags)",
        0x33 => return "CPX # failure (messed up flags)",
        0x34 => return "CPX # failure (messed up flags)",
        0x35 => return "CPX # failure (messed up flags)",
        0x36 => return "CPX # failure (messed up flags)",
        0x37 => return "CPX # failure (messed up flags)",
        0x38 => return "CPX # failure (messed up flags)",
        0x39 => return "LDX # failure (didn't set N and Z correctly)",
        0x3A => return "LDX # failure (didn't set N and Z correctly)",
        0x3B => return "LDY # failure (didn't set N and Z correctly)",
        0x3C => return "LDY # failure (didn't set N and Z correctly)",
        0x3D => return "compare(s) stored the result in a register (whoops!)",
        0x3E => return "INX/DEX/INY/DEY did something bad",
        0x3F => return "INY/DEY messed up overflow or carry",
        0x40 => return "INX/DEX messed up overflow or carry",
        0x41 => return "TAY did something bad (changed wrong regs, messed up flags)",
        0x42 => return "TAX did something bad (changed wrong regs, messed up flags)",
        0x43 => return "TYA did something bad (changed wrong regs, messed up flags)",
        0x44 => return "TXA did something bad (changed wrong regs, messed up flags)",
        0x45 => return "TXS didn't set flags right, or TSX touched flags and it shouldn't have",
        0x46 => return "wrong data popped, or data not in right location on stack",
        0x47 => return "JSR didn't work as expected",
        0x48 => return "RTS/JSR shouldn't have affected flags",
        0x49 => return "RTI/RTS didn't work right when return addys/data were manually pushed",
        0x4A => return "LSR A  failed",
        0x4B => return "ASL A  failed",
        0x4C => return "ROR A  failed",
        0x4D => return "ROL A  failed",
        0x4E => return "Absolute,X NOPs less than 3 bytes long",
        0x4F =>return  "IMPLIED NOPs affects regs/flags",
        0x50 =>return  "ZP,X NOPs less than 2 bytes long",
        0x51 => return "Absolute NOP less than 3 bytes long",
        0x52 => return "ZP NOPs less than 2 bytes long",
        0x53 => return "Absolute,X NOPs less than 3 bytes long",
        0x54 => return "Implied NOPs affects regs/flags",
        0x55 => return "ZP,X NOPs less than 2 bytes long",
        0x56 => return "Absolute NOP less than 3 bytes long",
        0x57 => return "ZP NOPs less than 2 bytes long",
        0x58 => return "LDA didn't load the data it expected to load",
        0x59 => return "STA didn't store the data where it was supposed to",
        0x5A => return "ORA failure",
        0x5B => return "ORA failure",
        0x5C => return "AND failure",
        0x5D => return "AND failure",
        0x5E => return "EOR failure",
        0x5F => return "EOR failure",
        0x60 => return "ADC failure",
        0x61 => return "ADC failure",
        0x62 => return "ADC failure",
        0x63 => return "ADC failure",
        0x64 => return "ADC failure",
        0x65 => return "CMP failure",
        0x66 => return "CMP failure",
        0x67 => return "CMP failure",
        0x68 => return "CMP failure",
        0x69 => return "CMP failure",
        0x6A => return "CMP failure",
        0x6B => return "CMP failure",
        0x6C => return "SBC failure",
        0x6D => return "SBC failure",
        0x6E => return "SBC failure",
        0x6F => return "SBC failure",
        0x70 => return "SBC failure",
        0x71 => return "SBC # failure",
        0x72 => return "SBC # failure",
        0x73 => return "SBC # failure",
        0x74 => return "SBC # failure",
        0x75 => return "SBC # failure",
        0x76 => return "LDA didn't set the flags properly",
        0x77 => return "STA affected flags it shouldn't",
        0x78 => return "LDY didn't set the flags properly",
        0x79 => return "STY affected flags it shouldn't",
        0x7A => return "LDX didn't set the flags properly",
        0x7B => return "STX affected flags it shouldn't",
        0x7C => return "BIT failure",
        0x7D => return "BIT failure",
        0x7E => return "ORA failure",
        0x7F => return "ORA failure",
        0x80 => return "AND failure",
        0x81 => return "AND failure",
        0x82 => return "EOR failure",
        0x83 => return "EOR failure",
        0x84 => return "ADC failure",
        0x85 => return "ADC failure",
        0x86 => return "ADC failure",
        0x87 => return "ADC failure",
        0x88 => return "ADC failure",
        0x89 => return "CMP failure",
        0x8A => return "CMP failure",
        0x8B => return "CMP failure",
        0x8C => return "CMP failure",
        0x8D => return "CMP failure",
        0x8E => return "CMP failure",
        0x8F => return "CMP failure",
        0x90 => return "SBC failure",
        0x91 => return "SBC failure",
        0x92 => return "SBC failure",
        0x93 => return "SBC failure",
        0x94 => return "SBC failure",
        0x95 => return "CPX failure",
        0x96 => return "CPX failure",
        0x97 => return "CPX failure",
        0x98 => return "CPX failure",
        0x99 => return "CPX failure",
        0x9A => return "CPX failure",
        0x9B => return "CPX failure",
        0x9C => return "CPY failure",
        0x9D => return "CPY failure",
        0x9E => return "CPY failure",
        0x9F => return "CPY failure",
        0xA0 => return "CPY failure",
        0xA1 => return "CPY failure",
        0xA2 => return "CPY failure",
        0xA3 => return "LSR failure",
        0xA4 => return "LSR failure",
        0xA5 => return "ASL failure",
        0xA6 => return "ASL failure",
        0xA7 => return "ROL failure",
        0xA8 => return "ROL failure",
        0xA9 => return "ROR failure",
        0xAA => return "ROR failure",
        0xAB => return "INC failure",
        0xAC => return "INC failure",
        0xAD => return "DEC failure",
        0xAE => return "DEC failure",
        0xAF => return "DEC failure",
        0xB0 => return "LDA didn't set the flags properly",
        0xB1 => return "STA affected flags it shouldn't",
        0xB2 => return "LDY didn't set the flags properly",
        0xB3 => return "STY affected flags it shouldn't",
        0xB4 => return "LDX didn't set the flags properly",
        0xB5 => return "STX affected flags it shouldn't",
        0xB6 => return "BIT failure",
        0xB7 => return "BIT failure",
        0xB8 => return "ORA failure",
        0xB9 => return "ORA failure",
        0xBA => return "AND failure",
        0xBB => return "AND failure",
        0xBC => return "EOR failure",
        0xBD => return "EOR failure",
        0xBE => return "ADC failure",
        0xBF => return "ADC failure",
        0xC0 => return "ADC failure",
        0xC1 => return "ADC failure",
        0xC2 => return "ADC failure",
        0xC3 => return "CMP failure",
        0xC4 => return "CMP failure",
        0xC5 => return "CMP failure",
        0xC6 => return "CMP failure",
        0xC7 => return "CMP failure",
        0xC8 => return "CMP failure",
        0xC9 => return "CMP failure",
        0xCA => return "SBC failure",
        0xCB => return "SBC failure",
        0xCC => return "SBC failure",
        0xCD => return "SBC failure",
        0xCE => return "SBC failure",
        0xCF => return "CPX failure",
        0xD0 => return "CPX failure",
        0xD1 => return "CPX failure",
        0xD2 => return "CPX failure",
        0xD3 => return "CPX failure",
        0xD4 => return "CPX failure",
        0xD5 => return "CPX failure",
        0xD6 => return "CPY failure",
        0xD7 => return "CPY failure",
        0xD8 => return "CPY failure",
        0xD9 => return "CPY failure",
        0xDA => return "CPY failure",
        0xDB => return "CPY failure",
        0xDC => return "CPY failure",
        0xDD => return "LSR failure",
        0xDE => return "LSR failure",
        0xDF => return "ASL failure",
        0xE0 => return "ASL failure",
        0xE1 => return "ROR failure",
        0xE2 => return "ROR failure",
        0xE3 => return "ROL failure",
        0xE4 => return "ROL failure",
        0xE5 => return "INC failure",
        0xE6 => return "INC failure",
        0xE7 => return "DEC failure",
        0xE8 => return "DEC failure",
        0xE9 => return "DEC failure",
        0xEA => return "LDA didn't load what it was supposed to",
        0xEB => return "read location should've wrapped around ffffh to 0000h",
        0xEC => return "should've wrapped zeropage address",
        0xED => return "ORA failure",
        0xEE => return "ORA failure",
        0xEF => return "AND failure",
        0xF0 => return "AND failure",
        0xF1 => return "EOR failure",
        0xF2 => return "EOR failure",
        0xF3 => return "ADC failure",
        0xF4 => return "ADC failure",
        0xF5 => return "ADC failure",
        0xF6 => return "ADC failure",
        0xF7 => return "ADC failure",
        0xF8 => return "CMP failure",
        0xF9 => return "CMP failure",
        0xFA => return "CMP failure",
        0xFB => return "CMP failure",
        0xFC => return "CMP failure",
        0xFD => return "CMP failure",
        0xFE => return "CMP failure",
        _ => panic!("Unknown code!"),
    }
}

pub fn translate_03(byte: u8) -> &'static str {
    match byte {
        0x00 => return "No error, all tests pass",
        0x01 => return "RRA (indr,x) failure",
        0x01 => return "SBC failure",
        0x02 => return "RRA (indr,x) failure",
        0x02 => return "SBC failure",
        0x03 => return "RRA (indr,x) failure",
        0x03 => return "SBC failure",
        0x04 => return "RRA zeropage failure",
        0x04 => return "SBC failure",
        0x05 => return "RRA zeropage failure",
        0x05 => return "SBC failure",
        0x06 => return "RRA zeropage failure",
        0x06 => return "STA failure",
        0x07 => return "JMP () data reading didn't wrap properly (this fails on a 65C02)",
        0x07 => return "RRA absolute failure",
        0x08 => return "LDY,X failure",
        0x08 => return "RRA absolute failure",
        0x09 => return "LDY,X failure",
        0x09 => return "RRA absolute failure",
        0x0A => return "RRA (indr),y failure",
        0x0A => return "STY,X failure",
        0x0B => return "ORA failure",
        0x0B => return "RRA (indr),y failure",
        0x0C => return "ORA failure",
        0x0C => return "RRA (indr),y failure",
        0x0D => return "AND failure",
        0x0D => return "RRA zp,x failure",
        0x0E => return "AND failure",
        0x0E => return "RRA zp,x failure",
        0x0F => return "EOR failure",
        0x0F => return "RRA zp,x failure",
        0x10 => return "EOR failure",
        0x10 => return "RRA abs,y failure",
        0x11 => return "ADC failure",
        0x11 => return "RRA abs,y failure",
        0x12 => return "ADC failure",
        0x12 => return "RRA abs,y failure",
        0x13 => return "ADC failure",
        0x13 => return "RRA abs,x failure",
        0x14 => return "ADC failure",
        0x14 => return "RRA abs,x failure",
        0x15 => return "ADC failure",
        0x15 => return "RRA abs,x failure",
        0x16 => return "CMP failure",
        0x17 => return "CMP failure",
        0x18 => return "CMP failure",
        0x19 => return "CMP failure",
        0x1A => return "CMP failure",
        0x1B => return "CMP failure",
        0x1C => return "CMP failure",
        0x1D => return "SBC failure",
        0x1E => return "SBC failure",
        0x1F => return "SBC failure",
        0x20 => return "SBC failure",
        0x21 => return "SBC failure",
        0x22 => return "LDA failure",
        0x23 => return "LDA failure",
        0x24 => return "STA failure",
        0x25 => return "LSR failure",
        0x26 => return "LSR failure",
        0x27 => return "ASL failure",
        0x28 => return "ASL failure",
        0x29 => return "ROR failure",
        0x2A => return "ROR failure",
        0x2B => return "ROL failure",
        0x2C => return "ROL failure",
        0x2D => return "INC failure",
        0x2E => return "INC failure",
        0x2F => return "DEC failure",
        0x30 => return "DEC failure",
        0x31 => return "DEC failure",
        0x32 => return "LDX,Y failure",
        0x33 => return "LDX,Y failure",
        0x34 => return "STX,Y failure",
        0x35 => return "STX,Y failure",
        0x36 => return "LDA failure",
        0x37 => return "LDA failure to wrap properly from ffffh to 0000h",
        0x38 => return "LDA failure, page cross",
        0x39 => return "ORA failure",
        0x3A => return "ORA failure",
        0x3B => return "AND failure",
        0x3C => return "AND failure",
        0x3D => return "EOR failure",
        0x3E => return "EOR failure",
        0x3F => return "ADC failure",
        0x40 => return "ADC failure",
        0x41 => return "ADC failure",
        0x42 => return "ADC failure",
        0x43 => return "ADC failure",
        0x44 => return "CMP failure",
        0x45 => return "CMP failure",
        0x46 => return "CMP failure",
        0x47 => return "CMP failure",
        0x48 => return "CMP failure",
        0x49 => return "CMP failure",
        0x4A => return "CMP failure",
        0x4B => return "SBC failure",
        0x4C => return "SBC failure",
        0x4D => return "SBC failure",
        0x4E => return "SBC failure",
        0x4F => return "SBC failure",
        0x50 => return "STA failure",
        0x51 => return "LDY,X failure",
        0x52 => return "LDY,X failure (didn't page cross)",
        0x53 => return "ORA failure",
        0x54 => return "ORA failure",
        0x55 => return "AND failure",
        0x56 => return "AND failure",
        0x57 => return "EOR failure",
        0x58 => return "EOR failure",
        0x59 => return "ADC failure",
        0x5A => return "ADC failure",
        0x5B => return "ADC failure",
        0x5C => return "ADC failure",
        0x5D => return "ADC failure",
        0x5E => return "CMP failure",
        0x5F => return "CMP failure",
        0x60 => return "CMP failure",
        0x61 => return "CMP failure",
        0x62 => return "CMP failure",
        0x63 => return "CMP failure",
        0x64 => return "CMP failure",
        0x65 => return "SBC failure",
        0x66 => return "SBC failure",
        0x67 => return "SBC failure",
        0x68 => return "SBC failure",
        0x69 => return "SBC failure",
        0x6A => return "LDA failure",
        0x6B => return "LDA failure (didn't page cross)",
        0x6C => return "STA failure",
        0x6D => return "LSR failure",
        0x6E => return "LSR failure",
        0x6F => return "ASL failure",
        0x70 => return "ASL failure",
        0x71 => return "ROR failure",
        0x72 => return "ROR failure",
        0x73 => return "ROL failure",
        0x74 => return "ROL failure",
        0x75 => return "INC failure",
        0x76 => return "INC failure",
        0x77 => return "DEC failure",
        0x78 => return "DEC failure",
        0x79 => return "DEC failure",
        0x7A => return "LDX,Y failure",
        0x7B => return "LDX,Y failure",
        0x7C => return "LAX (indr,x) failure",
        0x7D => return "LAX (indr,x) failure",
        0x7E => return "LAX zeropage failure",
        0x7F => return "LAX zeropage failure",
        0x80 => return "LAX absolute failure",
        0x81 => return "LAX absolute failure",
        0x82 => return "LAX (indr),y failure",
        0x83 => return "LAX (indr),y failure",
        0x84 => return "LAX zp,y failure",
        0x85 => return "LAX zp,y failure",
        0x86 => return "LAX abs,y failure",
        0x87 => return "LAX abs,y failure",
        0x88 => return "SAX (indr,x) failure",
        0x89 => return "SAX (indr,x) failure",
        0x8A => return "SAX zeropage failure",
        0x8B => return "SAX zeropage failure",
        0x8C => return "SAX absolute failure",
        0x8D => return "SAX absolute failure",
        0x8E => return "SAX zp,y failure",
        0x8F => return "SAX zp,y failure",
        0x90 => return "SBC failure",
        0x91 => return "SBC failure",
        0x92 => return "SBC failure",
        0x93 => return "SBC failure",
        0x94 => return "SBC failure",
        0x95 => return "DCP (indr,x) failure",
        0x96 => return "DCP (indr,x) failure",
        0x97 => return "DCP (indr,x) failure",
        0x98 => return "DCP zeropage failure",
        0x99 => return "DCP zeropage failure",
        0x9A => return "DCP zeropage failure",
        0x9B => return "DCP absolute failure",
        0x9C => return "DCP absolute failure",
        0x9D => return "DCP absolute failure",
        0x9E => return "DCP (indr),y failure",
        0x9F => return "DCP (indr),y failure",
        0xA0 => return "DCP (indr),y failure",
        0xA1 => return "DCP zp,x failure",
        0xA2 => return "DCP zp,x failure",
        0xA3 => return "DCP zp,x failure",
        0xA4 => return "DCP abs,y failure",
        0xA5 => return "DCP abs,y failure",
        0xA6 => return "DCP abs,y failure",
        0xA7 => return "DCP abs,x failure",
        0xA8 => return "DCP abs,x failure",
        0xA9 => return "DCP abs,x failure",
        0xAA => return "DCP (indr,x) failure",
        0xAB => return "DCP (indr,x) failure",
        0xAC => return "DCP (indr,x) failure",
        0xAD => return "DCP zeropage failure",
        0xAE => return "DCP zeropage failure",
        0xAF => return "DCP zeropage failure",
        0xB0 => return "DCP absolute failure",
        0xB1 => return "DCP absolute failure",
        0xB2 => return "DCP absolute failure",
        0xB3 => return "DCP (indr),y failure",
        0xB4 => return "DCP (indr),y failure",
        0xB5 => return "DCP (indr),y failure",
        0xB6 => return "DCP zp,x failure",
        0xB7 => return "DCP zp,x failure",
        0xB8 => return "DCP zp,x failure",
        0xB9 => return "DCP abs,y failure",
        0xBA => return "DCP abs,y failure",
        0xBB => return "DCP abs,y failure",
        0xBC => return "DCP abs,x failure",
        0xBD => return "DCP abs,x failure",
        0xBE => return "DCP abs,x failure",
        0xBF => return "SLO (indr,x) failure",
        0xC0 => return "SLO (indr,x) failure",
        0xC1 => return "SLO (indr,x) failure",
        0xC2 => return "SLO zeropage failure",
        0xC3 => return "SLO zeropage failure",
        0xC4 => return "SLO zeropage failure",
        0xC5 => return "SLO absolute failure",
        0xC6 => return "SLO absolute failure",
        0xC7 => return "SLO absolute failure",
        0xC8 => return "SLO (indr),y failure",
        0xC9 => return "SLO (indr),y failure",
        0xCA => return "SLO (indr),y failure",
        0xCB => return "SLO zp,x failure",
        0xCC => return "SLO zp,x failure",
        0xCD => return "SLO zp,x failure",
        0xCE => return "SLO abs,y failure",
        0xCF => return "SLO abs,y failure",
        0xD0 => return "SLO abs,y failure",
        0xD1 => return "SLO abs,x failure",
        0xD2 => return "SLO abs,x failure",
        0xD3 => return "SLO abs,x failure",
        0xD4 => return "RLA (indr,x) failure",
        0xD5 => return "RLA (indr,x) failure",
        0xD6 => return "RLA (indr,x) failure",
        0xD7 => return "RLA zeropage failure",
        0xD8 => return "RLA zeropage failure",
        0xD9 => return "RLA zeropage failure",
        0xDA => return "RLA absolute failure",
        0xDB => return "RLA absolute failure",
        0xDC => return "RLA absolute failure",
        0xDD => return "RLA (indr),y failure",
        0xDE => return "RLA (indr),y failure",
        0xDF => return "RLA (indr),y failure",
        0xE0 => return "RLA zp,x failure",
        0xE1 => return "RLA zp,x failure",
        0xE2 => return "RLA zp,x failure",
        0xE3 => return "RLA abs,y failure",
        0xE4 => return "RLA abs,y failure",
        0xE5 => return "RLA abs,y failure",
        0xE6 => return "RLA abs,x failure",
        0xE7 => return "RLA abs,x failure",
        0xE8 => return "RLA abs,x failure",
        0xE8 => return "SRE (indr,x) failure",
        0xEA => return "SRE (indr,x) failure",
        0xEB => return "SRE (indr,x) failure",
        0xEC => return "SRE zeropage failure",
        0xED => return "SRE zeropage failure",
        0xEE => return "SRE zeropage failure",
        0xEF => return "SRE absolute failure",
        0xF0 => return "SRE absolute failure",
        0xF1 => return "SRE absolute failure",
        0xF2 => return "SRE (indr),y failure",
        0xF3 => return "SRE (indr),y failure",
        0xF4 => return "SRE (indr),y failure",
        0xF5 => return "SRE zp,x failure",
        0xF6 => return "SRE zp,x failure",
        0xF7 => return "SRE zp,x failure",
        0xF8 => return "SRE abs,y failure",
        0xF9 => return "SRE abs,y failure",
        0xFA => return "SRE abs,y failure",
        0xFB => return "SRE abs,x failure",
        0xFC => return "SRE abs,x failure",
        0xFD => return "SRE abs,x failure",
        _ => panic!("Unknown code!"),
    }
}

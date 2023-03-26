import inspect

# from numba.experimental import jitclass

# @jitclass


class CPU:  # 8bit
    def __init__(self):
        self.registers = {
            "A": 0,
            "F": 0,
            "B": 0,
            "C": 0,
            "D": 0,
            "E": 0,
            "H": 0,
            "L": 0,
        }
        self.flags = {
            "Z": 0x80,
            "N": 0x40,
            "H": 0x20,
            "C": 0x10
        }

        self.PC: int = 0x100
        self.SP: int = 0xFFFE

        # clock
        self.clock_speed: int = 4194304  # 4.194304 MHz
        self.total_cycles: int = 0
        # cycles per instruction (M-cycles) (4.194304 MHz / 4 = 1.048576 MHz)

        # interrupts
        self.IME: bool = False  # flag
        self.IF: int = 0x00  # interrupt request
        self.IE: int = 0x00  # interrupt enable

        self.check_cb: bool = False

    """
    INFO:

    16 means 16bit register (mostly indirect (HL) | (BC) etc..) where() accesses memory
    no number assumes 8bit
    nn is the pc
    n is the pc + 1 (8bit)

    opcodes will return (instruction, cycles) as tuple
    e.g 0x00: lamdba: (instruction, cycle count)
    """
    opcodes = {
        0x00: (lambda cpu: cpu._nop(), 1),
        0x01: (lambda cpu, m: cpu._ld_rr_nn("B", "C", m), 3),  # LD BC, nn
        0x02: (lambda cpu, m: cpu._ld_16_r("B", "C", "A", m), 2),  # LD (BC), A
        0x03: (lambda cpu: cpu._inc_16("B", "C"), 2),  # INC BC
        0x04: (lambda cpu: cpu._inc(cpu.registers["B"]), 1),  # INC B
        0x05: (lambda cpu: cpu._dec(cpu.registers["B"]), 1),  # DEC B
        0x06: (lambda cpu, m: cpu._ld_r_n("B", m), 2),  # LD B, n
        0x07: (lambda cpu: cpu._rlca(), 1),  # RLCA # IMPLEMENT
        0x08: (lambda cpu, m: cpu._ld_nn_sp(m), 5),  # LD (nn), SP
        # ADD HL, BC
        0x09: (lambda cpu: cpu._add_16_16("H", "L", "B", "C"), 2),
        0x0A: (lambda cpu, m: cpu._ld_r_16("A", "B", "C", m), 2),  # LD A, (BC)
        0x0B: (lambda cpu: cpu._dec_16("B", "C"), 2),  # DEC BC
        0x0C: (lambda cpu: cpu._inc(cpu.registers["C"]), 1),  # INC C
        0x0D: (lambda cpu: cpu._dec(cpu.registers["C"]), 1),  # DEC C
        0x0E: (lambda cpu, m: cpu._ld_r_n("C", m), 2),  # LD C, n

        0x0F: (lambda cpu: cpu._rrca(), 1),  # RRCA # IMPLEMENT
        0x10: (lambda cpu, m: cpu._stop(m), 1),  # STOP # IMPLEMENT

        0x11: (lambda cpu, m: cpu._ld_rr_nn("D", "E", m), 3),  # LD DE, nn
        0x12: (lambda cpu, m: cpu._ld_16_r("D", "E", "A", m), 2),  # LD (DE), A
        0x13: (lambda cpu: cpu._inc_16("D", "E"), 2),  # INC DE
        0x14: (lambda cpu: cpu._inc(cpu.registers["D"]), 1),  # INC D
        0x15: (lambda cpu: cpu._dec(cpu.registers["D"]), 1),  # DEC D
        0x16: (lambda cpu, m: cpu._ld_r_n("D", m), 2),  # LD D, n

        0x17: (lambda cpu: cpu._rla(), 3),  # RLA # IMPLEMENT
        0x18: (lambda cpu, m: cpu._jr_n(m), 3),  # JR n

        # ADD HL, DE
        0x19: (lambda cpu: cpu._add_16_16("H", "L", "D", "E"), 2),
        0x1A: (lambda cpu, m: cpu._ld_r_16("A", "D", "E", m), 2),  # LD A, (DE)
        0x1B: (lambda cpu: cpu._dec_16("D", "E"), 2),  # DEC DE
        0x1C: (lambda cpu: cpu._inc(cpu.registers["E"]), 1),  # INC E
        0x1D: (lambda cpu: cpu._dec(cpu.registers["E"]), 1),  # DEC E
        0x1E: (lambda cpu, m: cpu._ld_r_n("E", m), 2),  # LD E, n

        0x1F: (lambda cpu: cpu._rra(), 1),  # RRA # IMPLEMENT
        0x20: (lambda cpu, m: cpu._jr_nz_n(m), 3),  # JR NZ, n

        0x21: (lambda cpu, m: cpu._ld_rr_nn("H", "L", m), 3),  # LD HL, nn
        # LDI (HL), A
        0x22: (lambda cpu, m: cpu._ldi_to_16("H", "L", "A", m), 2),
        0x23: (lambda cpu: cpu._inc_16("H", "L"), 2),  # INC HL
        0x24: (lambda cpu: cpu._inc(cpu.registers["H"]), 1),  # INC H
        0x25: (lambda cpu: cpu._dec(cpu.registers["H"]), 1),  # DEC H
        0x26: (lambda cpu, m: cpu._ld_r_n("H", m), 2),  # LD H, n
        0x27: (lambda cpu: cpu._daa(), 1),  # DAA # IMPLEMENT
        0x28: (lambda cpu, m: cpu._jr_z_n(m), 2),  # JR Z, n
        # ADD HL, HL
        0x29: (lambda cpu: cpu._add_16_16("H", "L", "H", "L"), 2),
        # LDI A, (HL)
        0x2A: (lambda cpu, m: cpu._ldi_from_16("A", "H", "L", m), 2),
        0x2B: (lambda cpu: cpu._dec_16("H", "L"), 2),  # DEC HL
        0x2C: (lambda cpu: cpu._inc(cpu.registers["L"]), 1),  # INC L
        0x2D: (lambda cpu: cpu._dec(cpu.registers["L"]), 1),  # DEC L
        0x2E: (lambda cpu, m: cpu._ld_r_n("L", m), 2),  # LD L, n

        0x2F: (lambda cpu: cpu._cpl(), 1),  # CPL # IMPLEMENT

        0x30: (lambda cpu, m: cpu._jr_nc_n(m), 2),  # JR NC, n
        0x31: (lambda cpu, m: cpu._ld_sp_nn(m), 3),  # LD SP, nn
        # LDD (HL), A
        0x32: (lambda cpu, m: cpu._ldd_to_16("H", "L", "A", m), 2),
        0x33: (lambda cpu: cpu._inc_pntrs("SP"), 2),  # INC SP
        0x34: (lambda cpu, m: cpu._inc(m.read_byte(
            cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 3),  # INC (HL)
        0x35: (lambda cpu, m: cpu._dec_16(m.read_byte(
            cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 3),  # DEC (HL)
        0x36: (lambda cpu, m: cpu._ld_n_to_16("H", "L", m), 3),  # LD (HL), n

        0x37: (lambda cpu: cpu._scf(), 1),  # SCF # IMPLEMENT

        0x38: (lambda cpu, m: cpu._jr_c_n(m), 2),  # JR C, n
        0x39: (lambda cpu: cpu._add_16_sp("H", "L"), 2),  # ADD HL, SP
        # LDD A, (HL)
        0x3A: (lambda cpu, m: cpu._ldd_from_16("A", "H", "L", m), 2),
        0x3B: (lambda cpu: cpu._dec_pntrs("SP"), 2),  # DEC SP
        0x3C: (lambda cpu: cpu._inc(cpu.registers["A"]), 1),  # INC A
        0x3D: (lambda cpu: cpu._dec(cpu.registers["A"], 1)),  # DEC A
        0x3E: (lambda cpu, m: cpu._ld_r_n("A", m), 2),  # LD A, n

        0x3F: (lambda cpu: cpu._ccf(), 1),  # CCF # IMPLEMENT

        0x40: (lambda cpu: cpu._ld_r_r("B", "B"), 1),  # LD B, B
        0x41: (lambda cpu: cpu._ld_r_r("B", "C"), 1),  # LD B, C
        0x42: (lambda cpu: cpu._ld_r_r("B", "D"), 1),  # LD B, D
        0x43: (lambda cpu: cpu._ld_r_r("B", "E"), 1),  # LD B, E
        0x44: (lambda cpu: cpu._ld_r_r("B", "H"), 1),  # LD B, H
        0x45: (lambda cpu: cpu._ld_r_r("B", "L"), 1),  # LD B, L
        0x46: (lambda cpu, m: cpu._ld_r_16("B", "H", "L", m), 2),  # LD B, (HL)
        0x47: (lambda cpu: cpu._ld_r_r("B", "A"), 1),  # LD B, A
        0x48: (lambda cpu: cpu._ld_r_r("C", "B"), 1),  # LD C, B
        0x49: (lambda cpu: cpu._ld_r_r("C", "C"), 1),  # LD C, C
        0x4A: (lambda cpu: cpu._ld_r_r("C", "D"), 1),  # LD C, D
        0x4B: (lambda cpu: cpu._ld_r_r("C", "E"), 1),  # LD C, E
        0x4C: (lambda cpu: cpu._ld_r_r("C", "H"), 1),  # LD C, H
        0x4D: (lambda cpu: cpu._ld_r_r("C", "L"), 1),  # LD C, L
        0x4E: (lambda cpu, m: cpu._ld_r_16("C", "H", "L", m), 2),  # LD C, (HL)
        0x4F: (lambda cpu: cpu._ld_r_r("C", "A"), 1),  # LD C, A

        0x50: (lambda cpu: cpu._ld_r_r("D", "B"), 1),  # LD D, B
        0x51: (lambda cpu: cpu._ld_r_r("D", "C"), 1),  # LD D, C
        0x52: (lambda cpu: cpu._ld_r_r("D", "D"), 1),  # LD D, D
        0x53: (lambda cpu: cpu._ld_r_r("D", "E"), 1),  # LD D, E
        0x54: (lambda cpu: cpu._ld_r_r("D", "H"), 1),  # LD D, H
        0x55: (lambda cpu: cpu._ld_r_r("D", "L"), 1),  # LD D, L
        0x56: (lambda cpu, m: cpu._ld_r_16("D", "H", "L", m), 2),  # LD D, (HL)
        0x57: (lambda cpu: cpu._ld_r_r("D", "A"), 1),  # LD D, A
        0x58: (lambda cpu: cpu._ld_r_r("E", "B"), 1),  # LD E, B
        0x59: (lambda cpu: cpu._ld_r_r("E", "C"), 1),  # LD E, C
        0x5A: (lambda cpu: cpu._ld_r_r("E", "D"), 1),  # LD E, D
        0x5B: (lambda cpu: cpu._ld_r_r("E", "E"), 1),  # LD E, E
        0x5C: (lambda cpu: cpu._ld_r_r("E", "H"), 1),  # LD E, H
        0x5D: (lambda cpu: cpu._ld_r_r("E", "L"), 1),  # LD E, L
        0x5E: (lambda cpu, m: cpu._ld_r_16("E", "H", "L", m), 2),  # LD E, (HL)
        0x5F: (lambda cpu: cpu._ld_r_r("E", "A"), 1),  # LD E, A

        0x60: (lambda cpu: cpu._ld_r_r("H", "B"), 1),  # LD H, B
        0x61: (lambda cpu: cpu._ld_r_r("H", "C"), 1),  # LD H, C
        0x62: (lambda cpu: cpu._ld_r_r("H", "D"), 1),  # LD H, D
        0x63: (lambda cpu: cpu._ld_r_r("H", "E"), 1),  # LD H, E
        0x64: (lambda cpu: cpu._ld_r_r("H", "H"), 1),  # LD H, H
        0x65: (lambda cpu: cpu._ld_r_r("H", "L"), 1),  # LD H, L
        0x66: (lambda cpu, m: cpu._ld_r_16("H", "H", "L", m), 2),  # LD H, (HL)
        0x67: (lambda cpu: cpu._ld_r_r("H", "A"), 1),  # LD H, A
        0x68: (lambda cpu: cpu._ld_r_r("L", "B"), 1),  # LD L, B
        0x69: (lambda cpu: cpu._ld_r_r("L", "C"), 1),  # LD L, C
        0x6A: (lambda cpu: cpu._ld_r_r("L", "D"), 1),  # LD L, D
        0x6B: (lambda cpu: cpu._ld_r_r("L", "E"), 1),  # LD L, E
        0x6C: (lambda cpu: cpu._ld_r_r("L", "H"), 1),  # LD L, H
        0x6D: (lambda cpu: cpu._ld_r_r("L", "L"), 1),  # LD L, L
        0x6E: (lambda cpu, m: cpu._ld_r_16("L", "H", "L", m), 2),  # LD L, (HL)

        0x6F: (lambda cpu: cpu._ld_r_r("L", "A"), 1),  # LD L, A
        0x70: (lambda cpu, m: cpu._ld_16_r("H", "L", "B", m), 2),  # LD (HL), B
        0x71: (lambda cpu, m: cpu._ld_16_r("H", "L", "C", m), 2),  # LD (HL), C
        0x72: (lambda cpu, m: cpu._ld_16_r("H", "L", "D", m), 2),  # LD (HL), D
        0x73: (lambda cpu, m: cpu._ld_16_r("H", "L", "E", m), 2),  # LD (HL), E
        0x74: (lambda cpu, m: cpu._ld_16_r("H", "L", "H", m), 2),  # LD (HL), H
        0x75: (lambda cpu, m: cpu._ld_16_r("H", "L", "L", m), 2),  # LD (HL), L

        0x76: (lambda cpu: cpu._halt(), 1),  # HALT (TODO: implement)

        0x77: (lambda cpu, m: cpu._ld_16_r("H", "L", "A", m), 2),  # LD (HL), A
        0x78: (lambda cpu: cpu._ld_r_r("A", "B"), 1),  # LD A, B
        0x79: (lambda cpu: cpu._ld_r_r("A", "C"), 1),  # LD A, C
        0x7A: (lambda cpu: cpu._ld_r_r("A", "D"), 1),  # LD A, D
        0x7B: (lambda cpu: cpu._ld_r_r("A", "E"), 1),  # LD A, E
        0x7C: (lambda cpu: cpu._ld_r_r("A", "H"), 1),  # LD A, H
        0x7D: (lambda cpu: cpu._ld_r_r("A", "L"), 1),  # LD A, L
        0x7E: (lambda cpu, m: cpu._ld_r_16("A", "H", "L", m), 2),  # LD A, (HL)
        0x7F: (lambda cpu: cpu._ld_r_r("A", "A"), 1),  # LD A, A

        0x80: (lambda cpu: cpu._add("A", "B"), 1),  # ADD A, B
        0x81: (lambda cpu: cpu._add("A", "C"), 1),  # ADD A, C
        0x82: (lambda cpu: cpu._add("A", "D"), 1),  # ADD A, D
        0x83: (lambda cpu: cpu._add("A", "E"), 1),  # ADD A, E
        0x84: (lambda cpu: cpu._add("A", "H"), 1),  # ADD A, H
        0x85: (lambda cpu: cpu._add("A", "L"), 1),  # ADD A, L
        # ADD A, (HL)
        0x86: (lambda cpu, m: cpu._add_from_16("A", "H", "L", m), 2),
        0x87: (lambda cpu: cpu._add("A", "A"), 1),  # ADD A, A

        0x88: (lambda cpu: cpu._adc("A", "B"), 1),  # ADC A, B
        0x89: (lambda cpu: cpu._adc("A", "C"), 1),  # ADC A, C
        0x8A: (lambda cpu: cpu._adc("A", "D"), 1),  # ADC A, D
        0x8B: (lambda cpu: cpu._adc("A", "E"), 1),  # ADC A, E
        0x8C: (lambda cpu: cpu._adc("A", "H"), 1),  # ADC A, H
        0x8D: (lambda cpu: cpu._adc("A", "L"), 1),  # ADC A, L
        # ADC A, (HL)
        0x8E: (lambda cpu, m: cpu._adc_from_16("A", "H", "L", m), 2),
        0x8F: (lambda cpu: cpu._adc("A", "A"), 1),  # ADC A, A

        0x90: (lambda cpu: cpu._sub("A", "B"), 1),  # SUB A, B
        0x91: (lambda cpu: cpu._sub("A", "C"), 1),  # SUB A, C
        0x92: (lambda cpu: cpu._sub("A", "D"), 1),  # SUB A, D
        0x93: (lambda cpu: cpu._sub("A", "E"), 1),  # SUB A, E
        0x94: (lambda cpu: cpu._sub("A", "H"), 1),  # SUB A, H
        0x95: (lambda cpu: cpu._sub("A", "L"), 1),  # SUB A, L
        # SUB A, (HL)
        0x96: (lambda cpu, m: cpu._sub_from_16("A", "H", "L", m), 2),
        0x97: (lambda cpu: cpu._sub("A", "A"), 1),  # SUB A, A

        0x98: (lambda cpu: cpu._sbc("A", "B"), 1),  # SBC A, B
        0x99: (lambda cpu: cpu._sbc("A", "C"), 1),  # SBC A, C
        0x9A: (lambda cpu: cpu._sbc("A", "D"), 1),  # SBC A, D
        0x9B: (lambda cpu: cpu._sbc("A", "E"), 1),  # SBC A, E
        0x9C: (lambda cpu: cpu._sbc("A", "H"), 1),  # SBC A, H
        0x9D: (lambda cpu: cpu._sbc("A", "L"), 1),  # SBC A, L
        # SBC A, (HL)
        0x9E: (lambda cpu, m: cpu._sbc_from_16("A", "H", "L", m), 2),
        0x9F: (lambda cpu: cpu._sbc("A", "A"), 1),  # SBC A, A

        0xA0: (lambda cpu: cpu._and("A", "B"), 1),  # AND A, B
        0xA1: (lambda cpu: cpu._and("A", "C"), 1),  # AND A, C
        0xA2: (lambda cpu: cpu._and("A", "D"), 1),  # AND A, D
        0xA3: (lambda cpu: cpu._and("A", "E"), 1),  # AND A, E
        0xA4: (lambda cpu: cpu._and("A", "H"), 1),  # AND A, H
        0xA5: (lambda cpu: cpu._and("A", "L"), 1),  # AND A, L
        # AND A, (HL)
        0xA6: (lambda cpu, m: cpu._and_from_16("A", "H", "L", m), 2),
        0xA7: (lambda cpu: cpu._and("A", "A"), 1),  # AND A, A

        0xA8: (lambda cpu: cpu._xor("A", "B"), 1),  # XOR A, B
        0xA9: (lambda cpu: cpu._xor("A", "C"), 1),  # XOR A, C
        0xAA: (lambda cpu: cpu._xor("A", "D"), 1),  # XOR A, D
        0xAB: (lambda cpu: cpu._xor("A", "E"), 1),  # XOR A, E
        0xAC: (lambda cpu: cpu._xor("A", "H"), 1),  # XOR A, H
        0xAD: (lambda cpu: cpu._xor("A", "L"), 1),  # XOR A, L
        # XOR A, (HL)
        0xAE: (lambda cpu, m: cpu._xor_from_16("A", "H", "L", m), 2),
        0xAF: (lambda cpu: cpu._xor("A", "A"), 1),  # XOR A, A

        0xB0: (lambda cpu: cpu._or("A", "B"), 1),  # OR A, B
        0xB1: (lambda cpu: cpu._or("A", "C"), 1),  # OR A, C
        0xB2: (lambda cpu: cpu._or("A", "D"), 1),  # OR A, D
        0xB3: (lambda cpu: cpu._or("A", "E"), 1),  # OR A, E
        0xB4: (lambda cpu: cpu._or("A", "H"), 1),  # OR A, H
        0xB5: (lambda cpu: cpu._or("A", "L"), 1),  # OR A, L
        # OR A, (HL)
        0xB6: (lambda cpu, m: cpu._or_from_16("A", "H", "L", m), 2),
        0xB7: (lambda cpu: cpu._or("A", "A"), 1),  # OR A, A

        0xB8: (lambda cpu: cpu._cp("B"), 1),  # CP A, B
        0xB9: (lambda cpu: cpu._cp("C"), 1),  # CP A, C
        0xBA: (lambda cpu: cpu._cp("D"), 1),  # CP A, D
        0xBB: (lambda cpu: cpu._cp("E"), 1),  # CP A, E
        0xBC: (lambda cpu: cpu._cp("H"), 1),  # CP A, H
        0xBD: (lambda cpu: cpu._cp("L"), 1),  # CP A, L
        0xBE: (lambda cpu, m: cpu._cp_from_16("H", "L", m), 2),  # CP A, (HL)
        0xBF: (lambda cpu: cpu._cp("A"), 1),  # CP A, A

        0xC0: (lambda cpu, m: cpu._ret_nz(m), 2),  # RET NZ
        0xC1: (lambda cpu, m: cpu._pop("B", "C", m), 3),  # POP BC
        0xC2: (lambda cpu, m: cpu._jp_nz(m), 3),  # JP NZ, nn
        0xC3: (lambda cpu, m: cpu._jp_nn(m), 4),  # JP nn
        0xC4: (lambda cpu, m: cpu._call_nz(m), 3),  # CALL NZ, nn
        0xC5: (lambda cpu, m: cpu._push("B", "C", m), 4),  # PUSH BC
        0xC6: (lambda cpu, m: cpu._add_n("A", m), 2),  # ADD A, n
        0xC7: (lambda cpu, m: cpu._rst(0x00, m), 4),  # RST 00H
        0xC8: (lambda cpu, m: cpu._ret_z(m), 5),  # RET Z
        0xC9: (lambda cpu, m: cpu._ret(m), 4),  # RET
        # JP Z, nn # 3 w/o branch, 4 w/ branch
        0xCA: (lambda cpu, m: cpu._jp_z_nn(m), 3),
        # CB prefix
        0xCB: (lambda cpu, m: cpu._cb(m.read_byte(cpu.PC + 1), m), 1),
        # CALL Z, nn # 3 w/o branch, 6 w/ branch
        0xCC: (lambda cpu, m: cpu._call_z_nn(m), 3),
        0xCD: (lambda cpu, m: cpu._call_nn(m), 6),  # CALL nn
        0xCE: (lambda cpu, m: cpu._adc_n("A", m), 2),  # ADC A, n
        0xCF: (lambda cpu, m: cpu._rst(0x08, m), 4),  # RST 08H

        0xD0: (lambda cpu, m: cpu._ret_nc(m), 2),  # RET NC 5 w/ branch
        0xD1: (lambda cpu, m: cpu._pop("D", "E", m), 3),  # POP DE
        0xD2: (lambda cpu, m: cpu._jp_nc_nn(m), 3),  # JP NC, nn cy4 w/ branch
        0xD3: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        # CALL NC, nn 6 w/ branch
        0xD4: (lambda cpu, m: cpu._call_nc_nn(m), 3),
        0xD5: (lambda cpu, m: cpu._push("D", "E", m), 4444),  # PUSH DE
        0xD6: (lambda cpu, m: cpu._sub_n("A", m), 2),  # SUB A, n
        0xD7: (lambda cpu, m: cpu._rst(0x10, m), 4),  # RST 10H
        0xD8: (lambda cpu, m: cpu._ret_c(m), 5),  # RET C
        0xD9: (lambda cpu: cpu._reti(), 4),  # RETI
        0xDA: (lambda cpu, m: cpu._jp_c_nn(m), 3),  # JP C, nn 4 w/ branch
        0xDB: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xDC: (lambda cpu, m: cpu._call_c_nn(m), 3),  # CALL C, nn 6 w/ branch
        0xDD: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xDE: (lambda cpu, m: cpu._sbc_n("A", m), 2),  # SBC A, n
        0xDF: (lambda cpu, m: cpu._rst(0x18, m), 4),  # RST 18H

        0xE0: (lambda cpu, m: cpu._ldh_n_a(m), 3),  # LDH (n), A
        0xE1: (lambda cpu, m: cpu._pop("H", "L", m), 3),  # POP HL
        0xE2: (lambda cpu, m: cpu._ldh_c_a(m), 2),  # LDH (C), A
        0xE3: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xE4: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xE5: (lambda cpu, m: cpu._push("H", "L", m), 4),  # PUSH HL
        0xE6: (lambda cpu, m: cpu._and_n("A", m), 2),  # AND A, n
        0xE7: (lambda cpu, m: cpu._rst(0x20, m), 4),  # RST 20H
        0xE8: (lambda cpu, m: cpu._add_sp_n(m), 4),  # ADD SP, n
        0xE9: (lambda cpu: cpu._jp_hl(), 1),  # JP (HL)
        0xEA: (lambda cpu, m: cpu._ld_nn_r(m), 4),  # LD (nn), A
        0xEB: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xEC: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xED: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xEE: (lambda cpu, m: cpu._xor_n("A", m), 2),  # XOR A, n
        0xEF: (lambda cpu, m: cpu._rst(0x28, m), 4),  # RST 28H

        0xF0: (lambda cpu, m: cpu._ldh_a_n(m), 3),  # LDH A, (n)
        0xF1: (lambda cpu, m: cpu._pop("A", "F", m), 3),  # POP AF
        0xF2: (lambda cpu, m: cpu._ldh_a_c(m), 2),  # LDH A, (C)
        0xF3: (lambda cpu: cpu._di(), 1),  # DI
        0xF4: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xF5: (lambda cpu, m: cpu._push("A", "F", m), 4),  # PUSH AF
        0xF6: (lambda cpu, m: cpu._or_n("A", m), 2),  # OR A, n
        0xF7: (lambda cpu, m: cpu._rst(0x30, m), 4),  # RST 30H
        0xF8: (lambda cpu, m: cpu._ld_hl_sp_n(m), 3),  # LD HL, SP+n
        0xF9: (lambda cpu: cpu._ld_sp_hl(), 2),  # LD SP, HL
        0xFA: (lambda cpu, m: cpu._ld_r_nn(m), 4),  # LD A, (nn)
        0xFB: (lambda cpu: cpu._ei(), 1),  # EI
        0xFC: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xFD: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xFE: (lambda cpu, m: cpu._cp_n(m), 2),  # CP A, n
        0xFF: (lambda cpu, m: cpu._rst(0x38, m), 4),  # RST 38H
    }

    cb_opcodes = {
        0x00: (lambda cpu: cpu._rlc(cpu.registers["B"]), 2),  # RLC B
        0x01: (lambda cpu: cpu._rlc(cpu.registers["C"]), 2),  # RLC C
        0x02: (lambda cpu: cpu._rlc(cpu.registers["D"]), 2),  # RLC D
        0x03: (lambda cpu: cpu._rlc(cpu.registers["E"]), 2),  # RLC E
        0x04: (lambda cpu: cpu._rlc(cpu.registers["H"]), 2),  # RLC H
        0x05: (lambda cpu: cpu._rlc(cpu.registers["L"]), 2),  # RLC L
        0x06: (lambda cpu, m: cpu._rlc(
            m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RLC (HL)
        0x07: (lambda cpu: cpu._rlc(cpu.registers["A"]), 2),  # RLC A
        0x08: (lambda cpu: cpu._rrc(cpu.registers["B"]), 2),  # RRC B
        0x09: (lambda cpu: cpu._rrc(cpu.registers["C"]), 2),  # RRC C
        0x0A: (lambda cpu: cpu._rrc(cpu.registers["D"]), 2),  # RRC D
        0x0B: (lambda cpu: cpu._rrc(cpu.registers["E"]), 2),  # RRC E
        0x0C: (lambda cpu: cpu._rrc(cpu.registers["H"]), 2),  # RRC H
        0x0D: (lambda cpu: cpu._rrc(cpu.registers["L"]), 2),  # RRC L
        0x0E: (lambda cpu, m: cpu._rrc(
            m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RRC (HL)
        0x0F: (lambda cpu: cpu._rrc(cpu.registers["A"]), 2),  # RRC A

        0x10: (lambda cpu: cpu._rl(cpu.registers["B"]), 2),  # RL B
        0x11: (lambda cpu: cpu._rl(cpu.registers["C"]), 2),  # RL C
        0x12: (lambda cpu: cpu._rl(cpu.registers["D"]), 2),  # RL D
        0x13: (lambda cpu: cpu._rl(cpu.registers["E"]), 2),  # RL E
        0x14: (lambda cpu: cpu._rl(cpu.registers["H"]), 2),  # RL H
        0x15: (lambda cpu: cpu._rl(cpu.registers["L"]), 2),  # RL L
        0x16: (lambda cpu, m: cpu._rl(
            m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RL (HL)
        0x17: (lambda cpu: cpu._rl(cpu.registers["A"]), 2),  # RL A
        0x18: (lambda cpu: cpu._rr(cpu.registers["B"]), 2),  # RR B
        0x19: (lambda cpu: cpu._rr(cpu.registers["C"]), 2),  # RR C
        0x1A: (lambda cpu: cpu._rr(cpu.registers["D"]), 2),  # RR D
        0x1B: (lambda cpu: cpu._rr(cpu.registers["E"]), 2),  # RR E
        0x1C: (lambda cpu: cpu._rr(cpu.registers["H"]), 2),  # RR H
        0x1D: (lambda cpu: cpu._rr(cpu.registers["L"]), 2),  # RR L
        0x1E: (lambda cpu, m: cpu._rr(
            m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RR (HL)
        0x1F: (lambda cpu: cpu._rr(cpu.registers["A"]), 2),  # RR A

        0x20: (lambda cpu: cpu._sla(cpu.registers["B"]), 2),  # SLA B
        0x21: (lambda cpu: cpu._sla(cpu.registers["C"]), 2),  # SLA C
        0x22: (lambda cpu: cpu._sla(cpu.registers["D"]), 2),  # SLA D
        0x23: (lambda cpu: cpu._sla(cpu.registers["E"]), 2),  # SLA E
        0x24: (lambda cpu: cpu._sla(cpu.registers["H"]), 2),  # SLA H
        0x25: (lambda cpu: cpu._sla(cpu.registers["L"]), 2),  # SLA L
        0x26: (lambda cpu, m: cpu._sla(
            m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SLA (HL)
        0x27: (lambda cpu: cpu._sla(cpu.registers["A"]), 2),  # SLA A
        0x28: (lambda cpu: cpu._sra(cpu.registers["B"]), 2),  # SRA B
        0x29: (lambda cpu: cpu._sra(cpu.registers["C"]), 2),  # SRA C
        0x2A: (lambda cpu: cpu._sra(cpu.registers["D"]), 2),  # SRA D
        0x2B: (lambda cpu: cpu._sra(cpu.registers["E"]), 2),  # SRA E
        0x2C: (lambda cpu: cpu._sra(cpu.registers["H"]), 2),  # SRA H
        0x2D: (lambda cpu: cpu._sra(cpu.registers["L"]), 2),  # SRA L
        0x2E: (lambda cpu, m: cpu._sra(
            m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SRA (HL)
        0x2F: (lambda cpu: cpu._sra(cpu.registers["A"]), 2),  # SRA A

        0x30: (lambda cpu: cpu._swap(cpu.registers["B"]), 2),  # SWAP B
        0x31: (lambda cpu: cpu._swap(cpu.registers["C"]), 2),  # SWAP C
        0x32: (lambda cpu: cpu._swap(cpu.registers["D"]), 2),  # SWAP D
        0x33: (lambda cpu: cpu._swap(cpu.registers["E"]), 2),  # SWAP E
        0x34: (lambda cpu: cpu._swap(cpu.registers["H"]), 2),  # SWAP H
        0x35: (lambda cpu: cpu._swap(cpu.registers["L"]), 2),  # SWAP L
        0x36: (lambda cpu, m: cpu._swap(
            m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SWAP (HL)
        0x37: (lambda cpu: cpu._swap(cpu.registers["A"]), 2),  # SWAP A
        0x38: (lambda cpu: cpu._srl(cpu.registers["B"]), 2),  # SRL B
        0x39: (lambda cpu: cpu._srl(cpu.registers["C"]), 2),  # SRL C
        0x3A: (lambda cpu: cpu._srl(cpu.registers["D"]), 2),  # SRL D
        0x3B: (lambda cpu: cpu._srl(cpu.registers["E"]), 2),  # SRL E
        0x3C: (lambda cpu: cpu._srl(cpu.registers["H"]), 2),  # SRL H
        0x3D: (lambda cpu: cpu._srl(cpu.registers["L"]), 2),  # SRL L
        0x3E: (lambda cpu, m: cpu._srl(
            m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SRL (HL)
        0x3F: (lambda cpu: cpu._srl(cpu.registers["A"]), 2),  # SRL A

        0x40: (lambda cpu: cpu._bit(0, cpu.registers["B"]), 2),  # BIT 0, B
        0x41: (lambda cpu: cpu._bit(0, cpu.registers["C"]), 2),  # BIT 0, C
        0x42: (lambda cpu: cpu._bit(0, cpu.registers["D"]), 2),  # BIT 0, D
        0x43: (lambda cpu: cpu._bit(0, cpu.registers["E"]), 2),  # BIT 0, E
        0x44: (lambda cpu: cpu._bit(0, cpu.registers["H"]), 2),  # BIT 0, H
        0x45: (lambda cpu: cpu._bit(0, cpu.registers["L"]), 2),  # BIT 0, L
        0x46: (lambda cpu, m: cpu._bit(
            0, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 3),  # BIT 0, (HL)
        0x47: (lambda cpu: cpu._bit(0, cpu.registers["A"]), 2),  # BIT 0, A
        0x48: (lambda cpu: cpu._bit(1, cpu.registers["B"]), 2),  # BIT 1, B
        0x49: (lambda cpu: cpu._bit(1, cpu.registers["C"]), 2),  # BIT 1, C
        0x4A: (lambda cpu: cpu._bit(1, cpu.registers["D"]), 2),  # BIT 1, D
        0x4B: (lambda cpu: cpu._bit(1, cpu.registers["E"]), 2),  # BIT 1, E
        0x4C: (lambda cpu: cpu._bit(1, cpu.registers["H"]), 2),  # BIT 1, H
        0x4D: (lambda cpu: cpu._bit(1, cpu.registers["L"]), 2),  # BIT 1, L
        0x4E: (lambda cpu, m: cpu._bit(
            1, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 3),  # BIT 1, (HL)
        0x4F: (lambda cpu: cpu._bit(1, cpu.registers["A"]), 2),  # BIT 1, A

        0x50: (lambda cpu: cpu._bit(2, cpu.registers["B"]), 2),  # BIT 2, B
        0x51: (lambda cpu: cpu._bit(2, cpu.registers["C"]), 2),  # BIT 2, C
        0x52: (lambda cpu: cpu._bit(2, cpu.registers["D"]), 2),  # BIT 2, D
        0x53: (lambda cpu: cpu._bit(2, cpu.registers["E"]), 2),  # BIT 2, E
        0x54: (lambda cpu: cpu._bit(2, cpu.registers["H"]), 2),  # BIT 2, H
        0x55: (lambda cpu: cpu._bit(2, cpu.registers["L"]), 2),  # BIT 2, L
        0x56: (lambda cpu, m: cpu._bit(
            2, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 3),  # BIT 2, (HL)
        0x57: (lambda cpu: cpu._bit(2, cpu.registers["A"]), 2),  # BIT 2, A
        0x58: (lambda cpu: cpu._bit(3, cpu.registers["B"]), 2),  # BIT 3, B
        0x59: (lambda cpu: cpu._bit(3, cpu.registers["C"]), 2),  # BIT 3, C
        0x5A: (lambda cpu: cpu._bit(3, cpu.registers["D"]), 2),  # BIT 3, D
        0x5B: (lambda cpu: cpu._bit(3, cpu.registers["E"]), 2),  # BIT 3, E
        0x5C: (lambda cpu: cpu._bit(3, cpu.registers["H"]), 2),  # BIT 3, H
        0x5D: (lambda cpu: cpu._bit(3, cpu.registers["L"]), 2),  # BIT 3, L
        0x5E: (lambda cpu, m: cpu._bit(
            3, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 3),  # BIT 3, (HL)
        0x5F: (lambda cpu: cpu._bit(3, cpu.registers["A"]), 2),  # BIT 3, A

        0x60: (lambda cpu: cpu._bit(4, cpu.registers["B"]), 2),  # BIT 4, B
        0x61: (lambda cpu: cpu._bit(4, cpu.registers["C"]), 2),  # BIT 4, C
        0x62: (lambda cpu: cpu._bit(4, cpu.registers["D"]), 2),  # BIT 4, D
        0x63: (lambda cpu: cpu._bit(4, cpu.registers["E"]), 2),  # BIT 4, E
        0x64: (lambda cpu: cpu._bit(4, cpu.registers["H"]), 2),  # BIT 4, H
        0x65: (lambda cpu: cpu._bit(4, cpu.registers["L"]), 2),  # BIT 4, L
        0x66: (lambda cpu, m: cpu._bit(
            4, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 3),  # BIT 4, (HL)
        0x67: (lambda cpu: cpu._bit(4, cpu.registers["A"]), 2),  # BIT 4, A
        0x68: (lambda cpu: cpu._bit(5, cpu.registers["B"]), 2),  # BIT 5, B
        0x69: (lambda cpu: cpu._bit(5, cpu.registers["C"]), 2),  # BIT 5, C
        0x6A: (lambda cpu: cpu._bit(5, cpu.registers["D"]), 2),  # BIT 5, D
        0x6B: (lambda cpu: cpu._bit(5, cpu.registers["E"]), 2),  # BIT 5, E
        0x6C: (lambda cpu: cpu._bit(5, cpu.registers["H"]), 2),  # BIT 5, H
        0x6D: (lambda cpu: cpu._bit(5, cpu.registers["L"]), 2),  # BIT 5, L
        0x6E: (lambda cpu, m: cpu._bit(
            5, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 3),  # BIT 5, (HL)
        0x6F: (lambda cpu: cpu._bit(5, cpu.registers["A"]), 2),  # BIT 5, A

        0x70: (lambda cpu: cpu._bit(6, cpu.registers["B"]), 2),  # BIT 6, B
        0x71: (lambda cpu: cpu._bit(6, cpu.registers["C"]), 2),  # BIT 6, C
        0x72: (lambda cpu: cpu._bit(6, cpu.registers["D"]), 2),  # BIT 6, D
        0x73: (lambda cpu: cpu._bit(6, cpu.registers["E"]), 2),  # BIT 6, E
        0x74: (lambda cpu: cpu._bit(6, cpu.registers["H"]), 2),  # BIT 6, H
        0x75: (lambda cpu: cpu._bit(6, cpu.registers["L"]), 2),  # BIT 6, L
        0x76: (lambda cpu, m: cpu._bit(
            6, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 3),  # BIT 6, (HL)
        0x77: (lambda cpu: cpu._bit(6, cpu.registers["A"]), 2),  # BIT 6, A
        0x78: (lambda cpu: cpu._bit(7, cpu.registers["B"]), 2),  # BIT 7, B
        0x79: (lambda cpu: cpu._bit(7, cpu.registers["C"]), 2),  # BIT 7, C
        0x7A: (lambda cpu: cpu._bit(7, cpu.registers["D"]), 2),  # BIT 7, D
        0x7B: (lambda cpu: cpu._bit(7, cpu.registers["E"]), 2),  # BIT 7, E
        0x7C: (lambda cpu: cpu._bit(7, cpu.registers["H"]), 2),  # BIT 7, H
        0x7D: (lambda cpu: cpu._bit(7, cpu.registers["L"]), 2),  # BIT 7, L
        0x7E: (lambda cpu, m: cpu._bit(
            7, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 3),  # BIT 7, (HL)
        0x7F: (lambda cpu: cpu._bit(7, cpu.registers["A"]), 2),  # BIT 7, A

        0x80: (lambda cpu: cpu._res(0, cpu.registers["B"]), 2),  # RES 0, B
        0x81: (lambda cpu: cpu._res(0, cpu.registers["C"]), 2),  # RES 0, C
        0x82: (lambda cpu: cpu._res(0, cpu.registers["D"]), 2),  # RES 0, D
        0x83: (lambda cpu: cpu._res(0, cpu.registers["E"]), 2),  # RES 0, E
        0x84: (lambda cpu: cpu._res(0, cpu.registers["H"]), 2),  # RES 0, H
        0x85: (lambda cpu: cpu._res(0, cpu.registers["L"]), 2),  # RES 0, L
        0x86: (lambda cpu, m: cpu._res(
            0, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RES 0, (HL)
        0x87: (lambda cpu: cpu._res(0, cpu.registers["A"]), 2),  # RES 0, A
        0x88: (lambda cpu: cpu._res(1, cpu.registers["B"]), 2),  # RES 1, B
        0x89: (lambda cpu: cpu._res(1, cpu.registers["C"]), 2),  # RES 1, C
        0x8A: (lambda cpu: cpu._res(1, cpu.registers["D"]), 2),  # RES 1, D
        0x8B: (lambda cpu: cpu._res(1, cpu.registers["E"]), 2),  # RES 1, E
        0x8C: (lambda cpu: cpu._res(1, cpu.registers["H"]), 2),  # RES 1, H
        0x8D: (lambda cpu: cpu._res(1, cpu.registers["L"]), 2),  # RES 1, L
        0x8E: (lambda cpu, m: cpu._res(
            1, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RES 1, (HL)
        0x8F: (lambda cpu: cpu._res(1, cpu.registers["A"]), 2),  # RES 1, A

        0x90: (lambda cpu: cpu._res(2, cpu.registers["B"]), 2),  # RES 2, B
        0x91: (lambda cpu: cpu._res(2, cpu.registers["C"]), 2),  # RES 2, C
        0x92: (lambda cpu: cpu._res(2, cpu.registers["D"]), 2),  # RES 2, D
        0x93: (lambda cpu: cpu._res(2, cpu.registers["E"]), 2),  # RES 2, E
        0x94: (lambda cpu: cpu._res(2, cpu.registers["H"]), 2),  # RES 2, H
        0x95: (lambda cpu: cpu._res(2, cpu.registers["L"]), 2),  # RES 2, L
        0x96: (lambda cpu, m: cpu._res(
            2, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RES 2, (HL)
        0x97: (lambda cpu: cpu._res(2, cpu.registers["A"]), 2),  # RES 2, A
        0x98: (lambda cpu: cpu._res(3, cpu.registers["B"]), 2),  # RES 3, B
        0x99: (lambda cpu: cpu._res(3, cpu.registers["C"]), 2),  # RES 3, C
        0x9A: (lambda cpu: cpu._res(3, cpu.registers["D"]), 2),  # RES 3, D
        0x9B: (lambda cpu: cpu._res(3, cpu.registers["E"]), 2),  # RES 3, E
        0x9C: (lambda cpu: cpu._res(3, cpu.registers["H"]), 2),  # RES 3, H
        0x9D: (lambda cpu: cpu._res(3, cpu.registers["L"]), 2),  # RES 3, L
        0x9E: (lambda cpu, m: cpu._res(
            3, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RES 3, (HL)
        0x9F: (lambda cpu: cpu._res(3, cpu.registers["A"]), 2),  # RES 3, A

        0xA0: (lambda cpu: cpu._res(4, cpu.registers["B"]), 2),  # RES 4, B
        0xA1: (lambda cpu: cpu._res(4, cpu.registers["C"]), 2),  # RES 4, C
        0xA2: (lambda cpu: cpu._res(4, cpu.registers["D"]), 2),  # RES 4, D
        0xA3: (lambda cpu: cpu._res(4, cpu.registers["E"]), 2),  # RES 4, E
        0xA4: (lambda cpu: cpu._res(4, cpu.registers["H"]), 2),  # RES 4, H
        0xA5: (lambda cpu: cpu._res(4, cpu.registers["L"]), 2),  # RES 4, L
        0xA6: (lambda cpu, m: cpu._res(
            4, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RES 4, (HL)
        0xA7: (lambda cpu: cpu._res(4, cpu.registers["A"]), 2),  # RES 4, A
        0xA8: (lambda cpu: cpu._res(5, cpu.registers["B"]), 2),  # RES 5, B
        0xA9: (lambda cpu: cpu._res(5, cpu.registers["C"]), 2),  # RES 5, C
        0xAA: (lambda cpu: cpu._res(5, cpu.registers["D"]), 2),  # RES 5, D
        0xAB: (lambda cpu: cpu._res(5, cpu.registers["E"]), 2),  # RES 5, E
        0xAC: (lambda cpu: cpu._res(5, cpu.registers["H"]), 2),  # RES 5, H
        0xAD: (lambda cpu: cpu._res(5, cpu.registers["L"]), 2),  # RES 5, L
        0xAE: (lambda cpu, m: cpu._res(
            5, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RES 5, (HL)
        0xAF: (lambda cpu: cpu._res(5, cpu.registers["A"]), 2),  # RES 5, A

        0xB0: (lambda cpu: cpu._res(6, cpu.registers["B"]), 2),  # RES 6, B
        0xB1: (lambda cpu: cpu._res(6, cpu.registers["C"]), 2),  # RES 6, C
        0xB2: (lambda cpu: cpu._res(6, cpu.registers["D"]), 2),  # RES 6, D
        0xB3: (lambda cpu: cpu._res(6, cpu.registers["E"]), 2),  # RES 6, E
        0xB4: (lambda cpu: cpu._res(6, cpu.registers["H"]), 2),  # RES 6, H
        0xB5: (lambda cpu: cpu._res(6, cpu.registers["L"]), 2),  # RES 6, L
        0xB6: (lambda cpu, m: cpu._res(
            6, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RES 6, (HL)
        0xB7: (lambda cpu: cpu._res(6, cpu.registers["A"]), 2),  # RES 6, A
        0xB8: (lambda cpu: cpu._res(7, cpu.registers["B"]), 2),  # RES 7, B
        0xB9: (lambda cpu: cpu._res(7, cpu.registers["C"]), 2),  # RES 7, C
        0xBA: (lambda cpu: cpu._res(7, cpu.registers["D"]), 2),  # RES 7, D
        0xBB: (lambda cpu: cpu._res(7, cpu.registers["E"]), 2),  # RES 7, E
        0xBC: (lambda cpu: cpu._res(7, cpu.registers["H"]), 2),  # RES 7, H
        0xBD: (lambda cpu: cpu._res(7, cpu.registers["L"]), 2),  # RES 7, L
        0xBE: (lambda cpu, m: cpu._res(
            7, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # RES 7, (HL)
        0xBF: (lambda cpu: cpu._res(7, cpu.registers["A"]), 2),  # RES 7, A

        0xC0: (lambda cpu: cpu._set(0, cpu.registers["B"]), 2),  # SET 0, B
        0xC1: (lambda cpu: cpu._set(0, cpu.registers["C"]), 2),  # SET 0, C
        0xC2: (lambda cpu: cpu._set(0, cpu.registers["D"]), 2),  # SET 0, D
        0xC3: (lambda cpu: cpu._set(0, cpu.registers["E"]), 2),  # SET 0, E
        0xC4: (lambda cpu: cpu._set(0, cpu.registers["H"]), 2),  # SET 0, H
        0xC5: (lambda cpu: cpu._set(0, cpu.registers["L"]), 2),  # SET 0, L
        0xC6: (lambda cpu, m: cpu._set(
            0, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SET 0, (HL)
        0xC7: (lambda cpu: cpu._set(0, cpu.registers["A"]), 2),  # SET 0, A
        0xC8: (lambda cpu: cpu._set(1, cpu.registers["B"]), 2),  # SET 1, B
        0xC9: (lambda cpu: cpu._set(1, cpu.registers["C"]), 2),  # SET 1, C
        0xCA: (lambda cpu: cpu._set(1, cpu.registers["D"]), 2),  # SET 1, D
        0xCB: (lambda cpu: cpu._set(1, cpu.registers["E"]), 2),  # SET 1, E
        0xCC: (lambda cpu: cpu._set(1, cpu.registers["H"]), 2),  # SET 1, H
        0xCD: (lambda cpu: cpu._set(1, cpu.registers["L"]), 2),  # SET 1, L
        0xCE: (lambda cpu, m: cpu._set(
            1, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SET 1, (HL)
        0xCF: (lambda cpu: cpu._set(1, cpu.registers["A"]), 1),  # SET 1, A

        0xD0: (lambda cpu: cpu._set(2, cpu.registers["B"]), 2),  # SET 2, B
        0xD1: (lambda cpu: cpu._set(2, cpu.registers["C"]), 2),  # SET 2, C
        0xD2: (lambda cpu: cpu._set(2, cpu.registers["D"]), 2),  # SET 2, D
        0xD3: (lambda cpu: cpu._set(2, cpu.registers["E"]), 2),  # SET 2, E
        0xD4: (lambda cpu: cpu._set(2, cpu.registers["H"]), 2),  # SET 2, H
        0xD5: (lambda cpu: cpu._set(2, cpu.registers["L"]), 2),  # SET 2, L
        0xD6: (lambda cpu, m: cpu._set(
            2, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SET 2, (HL)
        0xD7: (lambda cpu: cpu._set(2, cpu.registers["A"]), 2),  # SET 2, A
        0xD8: (lambda cpu: cpu._set(3, cpu.registers["B"]), 2),  # SET 3, B
        0xD9: (lambda cpu: cpu._set(3, cpu.registers["C"]), 2),  # SET 3, C
        0xDA: (lambda cpu: cpu._set(3, cpu.registers["D"]), 2),  # SET 3, D
        0xDB: (lambda cpu: cpu._set(3, cpu.registers["E"]), 2),  # SET 3, E
        0xDC: (lambda cpu: cpu._set(3, cpu.registers["H"]), 2),  # SET 3, H
        0xDD: (lambda cpu: cpu._set(3, cpu.registers["L"]), 2),  # SET 3, L
        0xDE: (lambda cpu, m: cpu._set(
            3, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SET 3, (HL)
        0xDF: (lambda cpu: cpu._set(3, cpu.registers["A"]), 2),  # SET 3, A

        0xE0: (lambda cpu: cpu._set(4, cpu.registers["B"]), 2),  # SET 4, B
        0xE1: (lambda cpu: cpu._set(4, cpu.registers["C"]), 2),  # SET 4, C
        0xE2: (lambda cpu: cpu._set(4, cpu.registers["D"]), 2),  # SET 4, D
        0xE3: (lambda cpu: cpu._set(4, cpu.registers["E"]), 2),  # SET 4, E
        0xE4: (lambda cpu: cpu._set(4, cpu.registers["H"]), 2),  # SET 4, H
        0xE5: (lambda cpu: cpu._set(4, cpu.registers["L"]), 2),  # SET 4, L
        0xE6: (lambda cpu, m: cpu._set(
            4, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SET 4, (HL)
        0xE7: (lambda cpu: cpu._set(4, cpu.registers["A"]), 2),  # SET 4, A
        0xE8: (lambda cpu: cpu._set(5, cpu.registers["B"]), 2),  # SET 5, B
        0xE9: (lambda cpu: cpu._set(5, cpu.registers["C"]), 2),  # SET 5, C
        0xEA: (lambda cpu: cpu._set(5, cpu.registers["D"]), 2),  # SET 5, D
        0xEB: (lambda cpu: cpu._set(5, cpu.registers["E"]), 2),  # SET 5, E
        0xEC: (lambda cpu: cpu._set(5, cpu.registers["H"]), 2),  # SET 5, H
        0xED: (lambda cpu: cpu._set(5, cpu.registers["L"]), 2),  # SET 5, L
        0xEE: (lambda cpu, m: cpu._set(
            5, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SET 5, (HL)
        0xEF: (lambda cpu: cpu._set(5, cpu.registers["A"]), 2),  # SET 5, A

        0xF0: (lambda cpu: cpu._set(6, cpu.registers["B"]), 2),  # SET 6, B
        0xF1: (lambda cpu: cpu._set(6, cpu.registers["C"]), 2),  # SET 6, C
        0xF2: (lambda cpu: cpu._set(6, cpu.registers["D"]), 2),  # SET 6, D
        0xF3: (lambda cpu: cpu._set(6, cpu.registers["E"]), 2),  # SET 6, E
        0xF4: (lambda cpu: cpu._set(6, cpu.registers["H"]), 2),  # SET 6, H
        0xF5: (lambda cpu: cpu._set(6, cpu.registers["L"]), 2),  # SET 6, L
        0xF6: (lambda cpu, m: cpu._set(
            6, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SET 6, (HL)
        0xF7: (lambda cpu: cpu._set(6, cpu.registers["A"]), 2),  # SET 6, A
        0xF8: (lambda cpu: cpu._set(7, cpu.registers["B"]), 2),  # SET 7, B
        0xF9: (lambda cpu: cpu._set(7, cpu.registers["C"]), 2),  # SET 7, C
        0xFA: (lambda cpu: cpu._set(7, cpu.registers["D"]), 2),  # SET 7, D
        0xFB: (lambda cpu: cpu._set(7, cpu.registers["E"]), 2),  # SET 7, E
        0xFC: (lambda cpu: cpu._set(7, cpu.registers["H"]), 2),  # SET 7, H
        0xFD: (lambda cpu: cpu._set(7, cpu.registers["L"]), 2),  # SET 7, L
        0xFE: (lambda cpu, m: cpu._set(
            7, m.read_byte(cpu.registers["H"] << 8 | cpu.registers["L"])
        ), 4),  # SET 7, (HL)
        0xFF: (lambda cpu: cpu._set(7, cpu.registers["A"]), 2),  # SET 7, A
    }

    # TIMERS

    def update_timers(self, cycles):
        self.divider_counter += cycles
        if self.divider_counter >= 256:
            self.divider_counter = 0
            self.divider_register += 1

        if self.timer_enabled:
            self.timer_counter += cycles
            if self.timer_counter >= self.timer_clocks[self.timer_clock_select]:
                self.timer_counter = 0
                self.timer_register += 1
                if self.timer_register == 0:
                    self.timer_register = self.timer_modulo
                    self.request_interrupt(2)

    def update_interrupts(self):
        if self.interrupt_master_enable:
            for i in range(5):
                if self.interrupt_flags & (1 << i) and self.interrupt_enable & (1 << i):
                    self.interrupt_master_enable = False
                    self.interrupt_flags &= ~(1 << i)
                    self.push_stack(self.PC)
                    self.PC = self.interrupt_vectors[i]
                    break

    def request_interrupt(self, interrupt):
        self.interrupt_flags |= 1 << interrupt

    # Helper methods for flag manipulation

    def set_flag(self, flag, value):
        if flag in self.flags:
            if value:
                self.registers["F"] |= self.flags[flag]
            else:
                self.registers["F"] &= ~self.flags[flag]
        else:
            raise ValueError("Invalid flag")

    def get_flag(self, flag):
        if flag in self.flags:
            return (self.registers["F"] & self.flags[flag]) != 0
        else:
            raise ValueError("Invalid flag")

    def execute_next_instruction(self, m):
        print("Executing next instruction")
        opcode = m.read_byte(self.PC)
        self.PC += 1
        if not self.check_cb:
            self.execute_opcode(opcode, m)
        else:
            self._cb(opcode, m)
            self.check_cb = False  # reset cb mode

    def execute_opcode(self, opcode, m):
        try:
            print("Executing opcode: ", hex(opcode))
            if opcode in self.opcodes:
                opcode_func, cycles = self.opcodes[opcode]
                num_args = len(inspect.signature(opcode_func).parameters)

                if num_args == 1:
                    opcode_func(self)
                elif num_args == 2:
                    opcode_func(self, m)
                else:
                    raise ValueError(
                        f"Unexpected number of arguments for opcode {hex(opcode)}: {num_args}")

                self.total_cycles += cycles
            else:
                raise ValueError(f"Unknown opcode: {hex(opcode)}")
        except ValueError as e:
            print(f"Error: {e}")

    def _cb(self, opcode, m):
        try:
            self.check_cb = True  # cb mode
            print("Executing CB prefixed opcode: ", hex(opcode))
            if opcode in self.cb_opcodes:
                opcode_func, cycles = self.cb_opcodes[opcode]
                num_args = len(inspect.signature(opcode_func).parameters)

                if num_args == 1:
                    opcode_func(self)
                    self.PC += 2
                elif num_args == 2:
                    opcode_func(self, m)
                    self.PC += 2
                else:
                    raise ValueError(
                        f"Unexpected number of arguments for opcode {hex(opcode)}: {num_args}")

                self.total_cycles = cycles
            else:
                raise ValueError(f"Unknown opcode: {hex(opcode)}")
        except ValueError as e:
            print(f"Error: {e}")

    ### Instructions ###

    def _nop(self):
        self.PC += 1

    def _halt(self):  # TODO: implement further
        self.PC += 1

    def _stop(self, m):  # TODO: implement once input is implemented
        m.read_byte(self.PC)
        self.PC += 1

    # rotate functions

    def _rlca(self):
        carry = (self.registers["A"] & 0x80) >> 7
        self.registers["A"] = ((self.registers["A"] << 1) & 0xFF) | carry
        self.registers["F"] = (self.registers["F"] & 0xEF) | (
            carry << 4)  # Update the carry flag
        self.PC += 1

    def _rrca(self):
        carry = self.registers["A"] & 0x01
        self.registers["A"] = (self.registers["A"] >> 1) | (carry << 7)
        self.registers["F"] = (self.registers["F"] & 0xEF) | (
            carry << 4)  # Update the carry flag
        self.PC += 1

    def _rla(self):
        carry = (self.registers["A"] & 0x80) >> 7
        self.registers["A"] = ((self.registers["A"] << 1)
                               & 0xFF) | self.get_flag("C")
        self.registers["F"] = (self.registers["F"] & 0xEF) | (
            carry << 4)

    def _rra(self):
        carry = self.registers["A"] & 0x01
        self.registers["A"] = (self.registers["A"] >> 1) | (
            self.get_flag("C") << 7)
        self.registers["F"] = (self.registers["F"] & 0xEF) | (
            carry << 4)

    def _scf(self):
        self.registers["F"] = (self.registers["F"] & 0x90) | 0x10
        self.PC += 1

    def _cpl(self):
        self.registers["A"] = ~self.registers["A"]
        self.registers["F"] = (self.registers["F"] & 0x10) | 0x60
        self.PC += 1

    # LD functions
    def _ld_r_r(self, reg1, reg2):
        self.registers[reg1] = self.registers[reg2]

    def _ld_r_n(self, reg1, m):
        self.registers[reg1] = m.read_byte(self.PC)
        self.PC += 1

    def _ld_rr_nn(self, reg1, reg2, m):
        nn = m.read_word(self.PC)
        self.PC += 2
        self.registers[reg1] = (nn >> 8) & 0xFF
        self.registers[reg2] = nn & 0xFF

    def _ld_r_nn(self, m):
        nn = m.read_word(self.PC)
        self.PC += 2
        self.registers["A"] = m.read_byte(nn)

    def _ld_nn_r(self, m):
        m.write_byte(m.read_word(self.PC), self.registers["A"])
        self.PC += 2

    def _ld_nn_sp(self, m):
        nn = m.read_word(self.PC)
        self.PC += 2
        m.write_word(nn, self.SP)

    def _ld_sp_nn(self, m):
        self.SP = m.read_word(self.PC)
        self.PC += 2

    def _ld_sp_hl(self):
        self.SP = self.registers["H"] << 8 | self.registers["L"]

    def _ld_hl_sp_n(self, m):
        n = m.read_byte(self.PC)
        self.PC += 1
        self.SP += n
        self.set_flag("Z", False)
        self.set_flag("N", False)
        self.set_flag("H", (self.SP & 0x0F) < (n & 0x0F))
        self.set_flag("C", (self.SP & 0xFF) < (n & 0xFF))

    def _ld_r_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = m.read_byte(address)

    def _ld_16_r(self, reg1, reg2, reg3, m):
        address = self.registers[reg1] << 8 | self.registers[reg2]
        m.write_byte(address, self.registers[reg3])

    def _ld_n_to_16(self, reg1, reg2, m):
        address = self.registers[reg1] << 8 | self.registers[reg2]
        m.write_byte(address, m.read_byte(self.PC))
        self.PC += 1

    def _ldi_to_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg1] << 8 | self.registers[reg2]
        m.write_byte(address, self.registers[reg3])
        self._inc_16(reg1, reg2)

    def _ldi_from_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = m.read_byte(address)
        self._inc_16(reg2, reg3)

    def _ldd_to_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg1] << 8 | self.registers[reg2]
        m.write_byte(address, self.registers[reg3])
        self._dec_16(reg1, reg2)

    def _ldd_from_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = m.read_byte(address)
        self._dec_16(reg2, reg3)

    # JR functions
    def _jr_n(self, m):
        self.PC += m.read_byte(self.PC) + 1

    def _jr_nz_n(self, m):
        if not self.get_flag("Z"):
            self.PC += m.read_byte(self.PC) + 1
        else:
            self.PC += 1

    def _jr_z_n(self, m):
        if self.get_flag("Z"):
            self.PC += m.read_byte(self.PC) + 1
        else:
            self.PC += 1

    def _jr_nc_n(self, m):
        if not self.get_flag("C"):
            self.PC += m.read_byte(self.PC) + 1
        else:
            self.PC += 1

    def _jr_c_n(self, m):
        if self.get_flag("C"):
            self.PC += m.read_byte(self.PC) + 1
        else:
            self.PC += 1

    def _daa(self):
        if not self.get_flag("N"):
            if self.get_flag("C") or self.registers["A"] > 0x99:
                self.registers["A"] += 0x60
                self.set_flag("C", True)
            if self.get_flag("H") or (self.registers["A"] & 0x0F) > 0x09:
                self.registers["A"] += 0x06
        else:
            if self.get_flag("C"):
                self.registers["A"] -= 0x60
            if self.get_flag("H"):
                self.registers["A"] -= 0x06

        self.set_flag("Z", self.registers["A"] == 0)
        self.set_flag("H", False)

    def _di(self):
        self.IME = False

    def _ei(self):
        self.IME = True

    # INC/DEC functions

    def _inc(self, reg_val):
        reg_val = (reg_val + 1) & 0xFF
        self.inc_flag(reg_val)

    def _inc_16(self, reg1, reg2):
        reg16 = ((self.registers[reg1] << 8 |
                  self.registers[reg2]) + 1) & 0xFFFF
        self.registers[reg1] = (reg16 >> 8) & 0xFF
        self.registers[reg2] = reg16 & 0xFF

    def _inc_pntrs(self, reg):
        if reg == "PC":
            self.PC = (self.PC + 1) & 0xFFFF
        elif reg == "SP":
            self.SP = (self.SP + 1) & 0xFFFF
        else:
            raise ValueError("Invalid register")

    def _dec_pntrs(self, reg):
        if reg == "PC":
            self.PC = (self.PC - 1) & 0xFFFF
        elif reg == "SP":
            self.SP = (self.SP - 1) & 0xFFFF
        else:
            raise ValueError("Invalid register")

    def _dec(self, reg_val):
        reg_val = (reg_val - 1) & 0xFF
        self.dec_flag(reg_val)

    def _dec_16(self, reg1, reg2):
        reg16 = ((self.registers[reg1] << 8 |
                  self.registers[reg2]) - 1) & 0xFFFF
        self.registers[reg1] = (reg16 >> 8) & 0xFF
        self.registers[reg2] = reg16 & 0xFF

    # ADD functions

    def _add(self, reg1, reg2):
        self.registers[reg1] = (self.registers[reg1] +
                                self.registers[reg2]) & 0xFF
        self.add_flags(self.registers[reg1])

    def _add_n(self, reg1, m):
        self.registers[reg1] = (self.registers[reg1] +
                                m.read_byte(self.PC)) & 0xFF
        self.PC += 1
        self.add_flags(self.registers[reg1])

    def _add_sp_n(self, m):
        self.SP = (self.SP + m.read_byte(self.PC)) & 0xFFFF
        self.PC += 1
        self.add_flags(self.SP)

    def _add_16_16(self, reg1, reg2, reg3, reg4):
        reg16 = (self.registers[reg1] << 8 | self.registers[reg2]) + \
            (self.registers[reg3] << 8 | self.registers[reg4]) & 0xFFFF
        self.registers[reg1] = (reg16 >> 8) & 0xFF
        self.registers[reg2] = reg16 & 0xFF
        self.ADD_16_16_Flags(reg16)

    def _add_16_sp(self, reg1, reg2):
        reg16 = (self.registers[reg1] << 8 |
                 self.registers[reg2]) + self.SP & 0xFFFF
        self.registers[reg1] = (reg16 >> 8) & 0xFF
        self.registers[reg2] = reg16 & 0xFF
        self.ADD_16_16_Flags(reg16)

    def ADD_16_16_Flags(self, reg_val):
        self.set_flag("N", False)
        self.set_flag("H", (reg_val & 0xFFF) > 0xFFF)
        self.set_flag("C", reg_val > 0xFFFF)

    # ADC functions

    def _adc(self, reg1, reg2):
        self.registers[reg1] = (self.registers[reg1] + self.registers[reg2] +
                                self.get_flag("C")) & 0xFF
        self.add_flags(self.registers[reg1])

    def _adc_from_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = (self.registers[reg1] + m.read_byte(address) +
                                self.get_flag("C")) & 0xFF
        self.add_flags(self.registers[reg1])

    def _adc_n(self, reg1, m):
        self.registers[reg1] = (self.registers[reg1] +
                                m.read_byte(self.PC) +
                                self.get_flag("C")) & 0xFF
        self.PC += 1
        self.add_flags(self.registers[reg1])

    # SUB functions

    def _sub(self, reg1, reg2):
        self.registers[reg1] = (self.registers[reg1] -
                                self.registers[reg2]) & 0xFF
        self.sub_flags(self.registers[reg1])

    def _sub_from_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = (self.registers[reg1] -
                                m.read_byte(address)) & 0xFF
        self.sub_flags(self.registers[reg1])

    def _sub_n(self, reg1, m):
        self.registers[reg1] = (self.registers[reg1] -
                                m.read_byte(self.PC)) & 0xFF
        self.PC += 1
        self.sub_flags(self.registers[reg1])

    # SBC functions

    def _sbc(self, reg1, reg2):
        self.registers[reg1] = (self.registers[reg1] - self.registers[reg2] -
                                self.get_flag("C")) & 0xFF
        self.sub_flags(self.registers[reg1])

    def _sbc_from_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = (self.registers[reg1] -
                                m.read_byte(address) - self.get_flag("C")) & 0xFF
        self.sub_flags(self.registers[reg1])

    def _sbc_n(self, reg, m):
        self.registers[reg] = (self.registers[reg] -
                               m.read_byte(self.PC) - self.get_flag("C")) & 0xFF
        self.PC += 1
        self.sub_flags(self.registers[reg])

    # AND functions
    def _and(self, reg1, reg2):
        self.registers[reg1] = self.registers[reg1] & self.registers[reg2]
        self.and_flags(self.registers[reg1])

    def _and_from_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = self.registers[reg1] & m.read_byte(address)
        self.and_flags(self.registers[reg1])

    def _and_n(self, reg, m):
        self.registers[reg] = self.registers[reg] & m.read_byte(self.PC)
        self.PC += 1
        self.and_flags(self.registers[reg])

    # OR functions
    def _or(self, reg1, reg2):
        self.registers[reg1] = self.registers[reg1] | self.registers[reg2]
        self.or_flags(self.registers[reg1])

    def _or_from_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = self.registers[reg1] | m.read_byte(address)
        self.or_flags(self.registers[reg1])

    def _or_n(self, reg, m):
        self.registers[reg] = self.registers[reg] | m.read_byte(self.PC)
        self.PC += 1
        self.or_flags(self.registers[reg])

    # XOR functions
    def _xor(self, reg1, reg2):
        self.registers[reg1] = self.registers[reg1] ^ self.registers[reg2]
        self.or_flags(self.registers[reg1])

    def _xor_from_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = self.registers[reg1] ^ m.read_byte(address)
        self.or_flags(self.registers[reg1])

    def _xor_n(self, reg, m):
        self.registers[reg] = self.registers[reg] ^ m.read_byte(self.PC)
        self.PC += 1
        self.or_flags(self.registers[reg])

    # CP functions
    def _cp(self, reg):
        self.cp_flags(self.registers[reg])

    def _cp_from_16(self, reg1, reg2, m):
        address = self.registers[reg1] << 8 | self.registers[reg2]
        self.cp_flags(m.read_byte(address))

    def _cp_n(self, m):
        self.cp_flags(m.read_byte(self.PC))
        self.PC += 1

    # RET functions
    def _ret_nz(self, m):
        if not self.get_flag("Z"):
            self.PC = m.read_word(self.SP)
            self.SP += 2

    def _ret_z(self, m):
        if self.get_flag("Z"):
            self.PC = m.read_word(self.SP)
            self.SP += 2

    def _ret_nc(self, m):
        if not self.get_flag("C"):
            self.PC = m.read_word(self.SP)
            self.SP += 2

    def _ret_c(self, m):
        if self.get_flag("C"):
            self.PC = m.read_word(self.SP)
            self.SP += 2

    def _ret(self, m):
        self.PC = m.read_word(self.SP)
        self.SP += 2

    # RETI functions

    def _reti(self, m):
        self._ret(m)
        self.IME = True

    # PUSH/POP functions

    def _pop(self, reg1, reg2, m):
        self.registers[reg1] = m.read_byte(self.SP)
        self.registers[reg2] = m.read_byte((self.SP + 1) & 0xFF)
        self.SP += 2

    def _push(self, reg1, reg2, m):
        self.SP -= 2
        m.write_byte(self.SP, self.registers[reg1])
        m.write_byte((self.SP + 1) & 0xFF, self.registers[reg2])

    # JP functions

    def _jp_nz(self, m):
        if not self.get_flag("Z"):
            self.PC = m.read_word(self.PC)

    def _jp_nn(self, m):
        self.PC = m.read_word(self.PC)

    def _jp_z_nn(self, m):
        if self.get_flag("Z"):
            self.PC = m.read_word(self.PC)

    def _jp_nc_nn(self, m):
        if not self.get_flag("C"):
            self.PC = m.read_word(self.PC)

    def _jp_c_nn(self, m):
        if self.get_flag("C"):
            self.PC = m.read_word(self.PC)

    def _jp_hl(self):
        self.PC = self.registers["H"] << 8 | self.registers["L"]

    # CALL functions

    def _call_nz(self, m):
        if not self.get_flag("Z"):
            self.SP -= 2
            m.write_word(self.SP, self.PC + 2)
            self.PC = m.read_word(self.PC)

    def _call_z_nn(self, m):
        if self.get_flag("Z"):
            self.SP -= 2
            m.write_word(self.SP, self.PC + 2)
            self.PC = m.read_word(self.PC)

    def _call_nn(self, m):
        self.SP -= 2
        m.write_word(self.SP, self.PC + 2)
        self.PC = m.read_word(self.PC)

    def _call_nc_nn(self, m):
        if not self.get_flag("C"):
            self.SP -= 2
            m.write_word(self.SP, self.PC + 2)
            self.PC = m.read_word(self.PC)

    def _call_c_nn(self, m):
        if self.get_flag("C"):
            self.SP -= 2
            m.write_word(self.SP, self.PC + 2)
            self.PC = m.read_word(self.PC)

    # RST functions

    def _rst(self, addr, m):
        self.SP -= 2
        m.write_word(self.SP, self.PC)
        self.PC = addr

    # LDH functions

    def _ldh_n_a(self, m):
        m.write_byte(0xFF00 + m.read_byte(self.PC + 1), self.registers["A"])
        self.PC += 1

    def _ldh_a_n(self, m):
        self.registers["A"] = m.read_byte(0xFF00 + m.read_byte(self.PC + 1))
        self.PC += 1

    def _ldh_c_a(self, m):
        m.write_byte(0xFF00 + self.registers["C"], self.registers["A"])

    def _ldh_a_c(self, m):
        self.registers["A"] = m.read_byte(0xFF00 + self.registers["C"])

    def _illegal(self):
        print("Illegal instruction at 0x{:04X}".format(self.PC))
        self.PC += 1

    # xCB prefixed functions

    # RLC/RRC functions
    def _rlc(self, reg_val):
        self.set_flag("C", reg_val & 0x80)
        reg_val = ((reg_val << 1)
                   & 0xFF) | (self.get_flag("C") >> 7)
        self.rl_rr_flags(reg_val)

    def _rrc(self, reg_val):
        self.set_flag("C", reg_val & 0x01)
        reg_val = (reg_val >> 1) | (
            self.get_flag("C") << 7)
        self.rl_rr_flags(reg_val)

    # RL/RR functions
    def _rl(self, reg_val):
        carry = self.get_flag("C")
        self.set_flag("C", reg_val & 0x80)
        reg_val = ((reg_val << 1) & 0xFF) | carry
        self.rl_rr_flags(reg_val)

    def _rr(self, reg_val):
        carry = self.get_flag("C")
        self.set_flag("C", reg_val & 0x01)
        reg_val = (reg_val >> 1) | (carry << 7)
        self.rl_rr_flags(reg_val)

    # SLA/SRA functions
    def _sla(self, reg_val):
        self.set_flag("C", reg_val & 0x80)
        reg_val = (reg_val << 1) & 0xFF
        self.rl_rr_flags(reg_val)
        self.PC += 1

    def _sra(self, reg_val):
        self.set_flag("C", reg_val & 0x01)
        reg_val = (reg_val >> 1) | (
            reg_val & 0x80)
        self.rl_rr_flags(reg_val)
        self.PC += 1

    # SWAP functions
    def _swap(self, reg_val):
        reg_val = ((reg_val & 0xF) << 4) | (
            (reg_val & 0xF0) >> 4)
        self.or_flags(reg_val)
        self.PC += 1

    # SRL functions
    def _srl(self, reg_val):
        self.set_flag("C", reg_val & 0x01)
        reg_val = reg_val >> 1
        self.rl_rr_flags(reg_val)

        self.PC += 1

    # BIT functions
    def _bit(self, bit, reg_val):
        self.set_flag("Z", not (reg_val & (1 << bit)))
        self.set_flag("N", False)
        self.set_flag("H", True)
        self.PC += 1

    # RES functions
    def _res(self, bit, reg_val):
        reg_val &= ~(1 << bit)
        self.PC += 1

    # SET functions
    def _set(self, bit, reg_val):
        reg_val |= (1 << bit)
        self.PC += 1

    # Flag set implementations

    def add_flags(self, reg_val):
        self.set_flag("Z", reg_val == 0)
        self.set_flag("N", False)
        self.set_flag("H", (reg_val & 0xF) > 0xF)
        self.set_flag("C", reg_val > 0xFF)

    def sub_flags(self, reg_val):
        self.set_flag("Z", reg_val == 0)
        self.set_flag("N", True)
        self.set_flag("H", (reg_val & 0xF) > 0xF)
        self.set_flag("C", reg_val > 0xFF)

    def inc_flag(self, reg_val):
        self.set_flag("Z", reg_val == 0)
        self.set_flag("N", False)
        self.set_flag("H", (reg_val & 0xF) == 0x0)

    def dec_flag(self, reg_val):
        self.set_flag("Z", reg_val == 0)
        self.set_flag("N", True)
        self.set_flag("H", (reg_val & 0xF) == 0xF)

    # for AND opcodes (same flags)

    def and_flags(self, reg_val):
        self.set_flag("Z", reg_val == 0)
        self.set_flag("N", False)
        self.set_flag("H", True)
        self.set_flag("C", False)

    # for OR opcodes (same flags)
    def or_flags(self, reg_val):
        self.set_flag("Z", reg_val == 0)
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.set_flag("C", False)

    # CP opcodes (same flags)
    def cp_flags(self, reg_val):
        self.set_flag("Z", self.registers["A"] == reg_val)
        self.set_flag("N", True)
        self.set_flag("H", (self.registers["A"] & 0xF) < (reg_val & 0xF))
        self.set_flag("C", self.registers["A"] < reg_val)

    def rot_flags(self, reg_val):
        self.set_flag("Z", reg_val == 0)
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.set_flag("C", reg_val & 0x01)

    def rl_rr_flags(self, reg_val):
        self.set_flag("Z", reg_val == 0)
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.set_flag("H", False)

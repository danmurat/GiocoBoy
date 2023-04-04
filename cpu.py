import io
import sys

# from numba.experimental import jitclass


# @jitclass
class CPU:  # 8bit
    def __init__(self):
        self.m = None  # memory
        self.debug_mode = False

        self.registers = {
            "A": 0x01,
            "F": 0xB0,
            "B": 0x00,
            "C": 0x13,
            "D": 0x00,
            "E": 0xD8,
            "H": 0x01,
            "L": 0x4D,
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
        self.set_IME_next = False
        self.halted = False

        # timers
        self.div_counter = 256
        self.tima_counter = 0

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
        0x01: (lambda cpu: cpu._ld_rr_nn("B", "C"), 3),  # LD BC, nn
        0x02: (lambda cpu: cpu._ld_16_r("B", "C", "A"), 2),  # LD (BC), A
        0x03: (lambda cpu: cpu._inc_16("B", "C"), 2),  # INC BC
        0x04: (lambda cpu: cpu._inc("B"), 1),  # INC B
        0x05: (lambda cpu: cpu._dec("B"), 1),  # DEC B
        0x06: (lambda cpu: cpu._ld_r_n("B"), 2),  # LD B, n
        0x07: (lambda cpu: cpu._rlca(), 1),  # RLCA
        0x08: (lambda cpu: cpu._ld_nn_sp(), 5),  # LD (nn), SP
        0x09: (lambda cpu: cpu._add_16_16("H", "L", "B", "C"), 2),  # ADD HL, BC
        0x0A: (lambda cpu: cpu._ld_r_16("A", "B", "C"), 2),  # LD A, (BC)
        0x0B: (lambda cpu: cpu._dec_16("B", "C"), 2),  # DEC BC
        0x0C: (lambda cpu: cpu._inc("C"), 1),  # INC C
        0x0D: (lambda cpu: cpu._dec("C"), 1),  # DEC C
        0x0E: (lambda cpu: cpu._ld_r_n("C"), 2),  # LD C, n
        0x0F: (lambda cpu: cpu._rrca(), 1),  # RRCA

        0x10: (lambda cpu: cpu._stop(), 0),  # STOP
        0x11: (lambda cpu: cpu._ld_rr_nn("D", "E"), 3),  # LD DE, nn
        0x12: (lambda cpu: cpu._ld_16_r("D", "E", "A"), 2),  # LD (DE), A
        0x13: (lambda cpu: cpu._inc_16("D", "E"), 2),  # INC DE
        0x14: (lambda cpu: cpu._inc("D"), 1),  # INC D
        0x15: (lambda cpu: cpu._dec("D"), 1),  # DEC D
        0x16: (lambda cpu: cpu._ld_r_n("D"), 2),  # LD D, n
        0x17: (lambda cpu: cpu._rla(), 1),  # RLA
        0x18: (lambda cpu: cpu._jr_n(), 3),  # JR n
        0x19: (lambda cpu: cpu._add_16_16("H", "L", "D", "E"), 2),  # ADD HL, DE
        0x1A: (lambda cpu: cpu._ld_r_16("A", "D", "E"), 2),  # LD A, (DE)
        0x1B: (lambda cpu: cpu._dec_16("D", "E"), 2),  # DEC DE
        0x1C: (lambda cpu: cpu._inc("E"), 1),  # INC E
        0x1D: (lambda cpu: cpu._dec("E"), 1),  # DEC E
        0x1E: (lambda cpu: cpu._ld_r_n("E"), 2),  # LD E, n
        0x1F: (lambda cpu: cpu._rra(), 1),  # RRA

        0x20: (lambda cpu: cpu._jr_nz_n(), 2),  # JR NZ, n
        0x21: (lambda cpu: cpu._ld_rr_nn("H", "L"), 3),  # LD HL, nn
        0x22: (lambda cpu: cpu._ldi_to_16("H", "L", "A"), 2),  # LDI (HL), A
        0x23: (lambda cpu: cpu._inc_16("H", "L"), 2),  # INC HL
        0x24: (lambda cpu: cpu._inc("H"), 1),  # INC H
        0x25: (lambda cpu: cpu._dec("H"), 1),  # DEC H
        0x26: (lambda cpu: cpu._ld_r_n("H"), 2),  # LD H, n
        0x27: (lambda cpu: cpu._daa(), 1),  # DAA
        0x28: (lambda cpu: cpu._jr_z_n(), 2),  # JR Z, n
        0x29: (lambda cpu: cpu._add_16_16("H", "L", "H", "L"), 2),  # ADD HL, HL
        0x2A: (lambda cpu: cpu._ldi_from_16("A", "H", "L"), 2),  # LDI A, (HL)
        0x2B: (lambda cpu: cpu._dec_16("H", "L"), 2),  # DEC HL
        0x2C: (lambda cpu: cpu._inc("L"), 1),  # INC L
        0x2D: (lambda cpu: cpu._dec("L"), 1),  # DEC L
        0x2E: (lambda cpu: cpu._ld_r_n("L"), 2),  # LD L, n
        0x2F: (lambda cpu: cpu._cpl(), 1),  # CPL

        0x30: (lambda cpu: cpu._jr_nc_n(), 2),  # JR NC, n
        0x31: (lambda cpu: cpu._ld_sp_nn(), 3),  # LD SP, nn
        0x32: (lambda cpu: cpu._ldd_to_16("H", "L", "A"), 2),  # LDD (HL), A
        0x33: (lambda cpu: cpu._inc_sp(), 2),  # INC SP
        0x34: (lambda cpu: cpu._inc_hl(), 3),  # INC (HL)
        0x35: (lambda cpu: cpu._dec_hl(), 3),  # DEC (HL)
        0x36: (lambda cpu: cpu._ld_n_to_16("H", "L"), 3),  # LD (HL), n
        0x37: (lambda cpu: cpu._scf(), 1),  # SCF
        0x38: (lambda cpu: cpu._jr_c_n(), 2),  # JR C, n
        0x39: (lambda cpu: cpu._add_16_sp("H", "L"), 2),  # ADD HL, SP
        0x3A: (lambda cpu: cpu._ldd_from_16("A", "H", "L"), 2),  # LDD A, (HL)
        0x3B: (lambda cpu: cpu._dec_sp(), 2),  # DEC SP
        0x3C: (lambda cpu: cpu._inc("A"), 1),  # INC A
        0x3D: (lambda cpu: cpu._dec("A"), 1),  # DEC A
        0x3E: (lambda cpu: cpu._ld_r_n("A"), 2),  # LD A, n
        0x3F: (lambda cpu: cpu._ccf(), 1),  # CCF

        0x40: (lambda cpu: cpu._ld_r_r("B", "B"), 1),  # LD B, B
        0x41: (lambda cpu: cpu._ld_r_r("B", "C"), 1),  # LD B, C
        0x42: (lambda cpu: cpu._ld_r_r("B", "D"), 1),  # LD B, D
        0x43: (lambda cpu: cpu._ld_r_r("B", "E"), 1),  # LD B, E
        0x44: (lambda cpu: cpu._ld_r_r("B", "H"), 1),  # LD B, H
        0x45: (lambda cpu: cpu._ld_r_r("B", "L"), 1),  # LD B, L
        0x46: (lambda cpu: cpu._ld_r_16("B", "H", "L"), 2),  # LD B, (HL)
        0x47: (lambda cpu: cpu._ld_r_r("B", "A"), 1),  # LD B, A
        0x48: (lambda cpu: cpu._ld_r_r("C", "B"), 1),  # LD C, B
        0x49: (lambda cpu: cpu._ld_r_r("C", "C"), 1),  # LD C, C
        0x4A: (lambda cpu: cpu._ld_r_r("C", "D"), 1),  # LD C, D
        0x4B: (lambda cpu: cpu._ld_r_r("C", "E"), 1),  # LD C, E
        0x4C: (lambda cpu: cpu._ld_r_r("C", "H"), 1),  # LD C, H
        0x4D: (lambda cpu: cpu._ld_r_r("C", "L"), 1),  # LD C, L
        0x4E: (lambda cpu: cpu._ld_r_16("C", "H", "L"), 2),  # LD C, (HL)
        0x4F: (lambda cpu: cpu._ld_r_r("C", "A"), 1),  # LD C, A

        0x50: (lambda cpu: cpu._ld_r_r("D", "B"), 1),  # LD D, B
        0x51: (lambda cpu: cpu._ld_r_r("D", "C"), 1),  # LD D, C
        0x52: (lambda cpu: cpu._ld_r_r("D", "D"), 1),  # LD D, D
        0x53: (lambda cpu: cpu._ld_r_r("D", "E"), 1),  # LD D, E
        0x54: (lambda cpu: cpu._ld_r_r("D", "H"), 1),  # LD D, H
        0x55: (lambda cpu: cpu._ld_r_r("D", "L"), 1),  # LD D, L
        0x56: (lambda cpu: cpu._ld_r_16("D", "H", "L"), 2),  # LD D, (HL)
        0x57: (lambda cpu: cpu._ld_r_r("D", "A"), 1),  # LD D, A
        0x58: (lambda cpu: cpu._ld_r_r("E", "B"), 1),  # LD E, B
        0x59: (lambda cpu: cpu._ld_r_r("E", "C"), 1),  # LD E, C
        0x5A: (lambda cpu: cpu._ld_r_r("E", "D"), 1),  # LD E, D
        0x5B: (lambda cpu: cpu._ld_r_r("E", "E"), 1),  # LD E, E
        0x5C: (lambda cpu: cpu._ld_r_r("E", "H"), 1),  # LD E, H
        0x5D: (lambda cpu: cpu._ld_r_r("E", "L"), 1),  # LD E, L
        0x5E: (lambda cpu: cpu._ld_r_16("E", "H", "L"), 2),  # LD E, (HL)
        0x5F: (lambda cpu: cpu._ld_r_r("E", "A"), 1),  # LD E, A

        0x60: (lambda cpu: cpu._ld_r_r("H", "B"), 1),  # LD H, B
        0x61: (lambda cpu: cpu._ld_r_r("H", "C"), 1),  # LD H, C
        0x62: (lambda cpu: cpu._ld_r_r("H", "D"), 1),  # LD H, D
        0x63: (lambda cpu: cpu._ld_r_r("H", "E"), 1),  # LD H, E
        0x64: (lambda cpu: cpu._ld_r_r("H", "H"), 1),  # LD H, H
        0x65: (lambda cpu: cpu._ld_r_r("H", "L"), 1),  # LD H, L
        0x66: (lambda cpu: cpu._ld_r_16("H", "H", "L"), 2),  # LD H, (HL)
        0x67: (lambda cpu: cpu._ld_r_r("H", "A"), 1),  # LD H, A
        0x68: (lambda cpu: cpu._ld_r_r("L", "B"), 1),  # LD L, B
        0x69: (lambda cpu: cpu._ld_r_r("L", "C"), 1),  # LD L, C
        0x6A: (lambda cpu: cpu._ld_r_r("L", "D"), 1),  # LD L, D
        0x6B: (lambda cpu: cpu._ld_r_r("L", "E"), 1),  # LD L, E
        0x6C: (lambda cpu: cpu._ld_r_r("L", "H"), 1),  # LD L, H
        0x6D: (lambda cpu: cpu._ld_r_r("L", "L"), 1),  # LD L, L
        0x6E: (lambda cpu: cpu._ld_r_16("L", "H", "L"), 2),  # LD L, (HL)
        0x6F: (lambda cpu: cpu._ld_r_r("L", "A"), 1),  # LD L, A

        0x70: (lambda cpu: cpu._ld_16_r("H", "L", "B"), 2),  # LD (HL), B
        0x71: (lambda cpu: cpu._ld_16_r("H", "L", "C"), 2),  # LD (HL), C
        0x72: (lambda cpu: cpu._ld_16_r("H", "L", "D"), 2),  # LD (HL), D
        0x73: (lambda cpu: cpu._ld_16_r("H", "L", "E"), 2),  # LD (HL), E
        0x74: (lambda cpu: cpu._ld_16_r("H", "L", "H"), 2),  # LD (HL), H
        0x75: (lambda cpu: cpu._ld_16_r("H", "L", "L"), 2),  # LD (HL), L
        0x76: (lambda cpu: cpu._halt(), 0),  # HALT
        0x77: (lambda cpu: cpu._ld_16_r("H", "L", "A"), 2),  # LD (HL), A
        0x78: (lambda cpu: cpu._ld_r_r("A", "B"), 1),  # LD A, B
        0x79: (lambda cpu: cpu._ld_r_r("A", "C"), 1),  # LD A, C
        0x7A: (lambda cpu: cpu._ld_r_r("A", "D"), 1),  # LD A, D
        0x7B: (lambda cpu: cpu._ld_r_r("A", "E"), 1),  # LD A, E
        0x7C: (lambda cpu: cpu._ld_r_r("A", "H"), 1),  # LD A, H
        0x7D: (lambda cpu: cpu._ld_r_r("A", "L"), 1),  # LD A, L
        0x7E: (lambda cpu: cpu._ld_r_16("A", "H", "L"), 2),  # LD A, (HL)
        0x7F: (lambda cpu: cpu._ld_r_r("A", "A"), 1),  # LD A, A

        0x80: (lambda cpu: cpu._add("A", "B"), 1),  # ADD A, B
        0x81: (lambda cpu: cpu._add("A", "C"), 1),  # ADD A, C
        0x82: (lambda cpu: cpu._add("A", "D"), 1),  # ADD A, D
        0x83: (lambda cpu: cpu._add("A", "E"), 1),  # ADD A, E
        0x84: (lambda cpu: cpu._add("A", "H"), 1),  # ADD A, H
        0x85: (lambda cpu: cpu._add("A", "L"), 1),  # ADD A, L
        0x86: (lambda cpu: cpu._add_a_hl(), 2),     # ADD A, (HL)
        0x87: (lambda cpu: cpu._add("A", "A"), 1),  # ADD A, A
        0x88: (lambda cpu: cpu._adc("A", "B"), 1),  # ADC A, B
        0x89: (lambda cpu: cpu._adc("A", "C"), 1),  # ADC A, C
        0x8A: (lambda cpu: cpu._adc("A", "D"), 1),  # ADC A, D
        0x8B: (lambda cpu: cpu._adc("A", "E"), 1),  # ADC A, E
        0x8C: (lambda cpu: cpu._adc("A", "H"), 1),  # ADC A, H
        0x8D: (lambda cpu: cpu._adc("A", "L"), 1),  # ADC A, L
        0x8E: (lambda cpu: cpu._adc_from_16("A", "H", "L"), 2),  # ADC A, (HL)
        0x8F: (lambda cpu: cpu._adc("A", "A"), 1),  # ADC A, A

        0x90: (lambda cpu: cpu._sub("A", "B"), 1),  # SUB A, B
        0x91: (lambda cpu: cpu._sub("A", "C"), 1),  # SUB A, C
        0x92: (lambda cpu: cpu._sub("A", "D"), 1),  # SUB A, D
        0x93: (lambda cpu: cpu._sub("A", "E"), 1),  # SUB A, E
        0x94: (lambda cpu: cpu._sub("A", "H"), 1),  # SUB A, H
        0x95: (lambda cpu: cpu._sub("A", "L"), 1),  # SUB A, L
        0x96: (lambda cpu: cpu._sub_from_16("A", "H", "L"), 2),  # SUB A, (HL)
        0x97: (lambda cpu: cpu._sub("A", "A"), 1),  # SUB A, A
        0x98: (lambda cpu: cpu._sbc("A", "B"), 1),  # SBC A, B
        0x99: (lambda cpu: cpu._sbc("A", "C"), 1),  # SBC A, C
        0x9A: (lambda cpu: cpu._sbc("A", "D"), 1),  # SBC A, D
        0x9B: (lambda cpu: cpu._sbc("A", "E"), 1),  # SBC A, E
        0x9C: (lambda cpu: cpu._sbc("A", "H"), 1),  # SBC A, H
        0x9D: (lambda cpu: cpu._sbc("A", "L"), 1),  # SBC A, L
        0x9E: (lambda cpu: cpu._sbc_from_16("A", "H", "L"), 2),  # SBC A, (HL)
        0x9F: (lambda cpu: cpu._sbc("A", "A"), 1),  # SBC A, A

        0xA0: (lambda cpu: cpu._and("A", "B"), 1),  # AND A, B
        0xA1: (lambda cpu: cpu._and("A", "C"), 1),  # AND A, C
        0xA2: (lambda cpu: cpu._and("A", "D"), 1),  # AND A, D
        0xA3: (lambda cpu: cpu._and("A", "E"), 1),  # AND A, E
        0xA4: (lambda cpu: cpu._and("A", "H"), 1),  # AND A, H
        0xA5: (lambda cpu: cpu._and("A", "L"), 1),  # AND A, L
        0xA6: (lambda cpu: cpu._and_from_16("A", "H", "L"), 2),  # AND A, (HL)
        0xA7: (lambda cpu: cpu._and("A", "A"), 1),  # AND A, A
        0xA8: (lambda cpu: cpu._xor("A", "B"), 1),  # XOR A, B
        0xA9: (lambda cpu: cpu._xor("A", "C"), 1),  # XOR A, C
        0xAA: (lambda cpu: cpu._xor("A", "D"), 1),  # XOR A, D
        0xAB: (lambda cpu: cpu._xor("A", "E"), 1),  # XOR A, E
        0xAC: (lambda cpu: cpu._xor("A", "H"), 1),  # XOR A, H
        0xAD: (lambda cpu: cpu._xor("A", "L"), 1),  # XOR A, L
        0xAE: (lambda cpu: cpu._xor_from_16("A", "H", "L"), 2),  # XOR A, (HL)
        0xAF: (lambda cpu: cpu._xor("A", "A"), 1),  # XOR A, A

        0xB0: (lambda cpu: cpu._or("A", "B"), 1),  # OR A, B
        0xB1: (lambda cpu: cpu._or("A", "C"), 1),  # OR A, C
        0xB2: (lambda cpu: cpu._or("A", "D"), 1),  # OR A, D
        0xB3: (lambda cpu: cpu._or("A", "E"), 1),  # OR A, E
        0xB4: (lambda cpu: cpu._or("A", "H"), 1),  # OR A, H
        0xB5: (lambda cpu: cpu._or("A", "L"), 1),  # OR A, L
        0xB6: (lambda cpu: cpu._or_from_16("A", "H", "L"), 2),  # OR A, (HL)
        0xB7: (lambda cpu: cpu._or("A", "A"), 1),  # OR A, A
        0xB8: (lambda cpu: cpu._cp("B"), 1),  # CP A, B
        0xB9: (lambda cpu: cpu._cp("C"), 1),  # CP A, C
        0xBA: (lambda cpu: cpu._cp("D"), 1),  # CP A, D
        0xBB: (lambda cpu: cpu._cp("E"), 1),  # CP A, E
        0xBC: (lambda cpu: cpu._cp("H"), 1),  # CP A, H
        0xBD: (lambda cpu: cpu._cp("L"), 1),  # CP A, L
        0xBE: (lambda cpu: cpu._cp_from_16("H", "L"), 2),  # CP A, (HL)
        0xBF: (lambda cpu: cpu._cp("A"), 1),  # CP A, A

        0xC0: (lambda cpu: cpu._ret_nz(), 2),  # RET NZ
        0xC1: (lambda cpu: cpu._pop("B", "C"), 3),  # POP BC
        0xC2: (lambda cpu: cpu._jp_nz(), 3),  # JP NZ, nn
        0xC3: (lambda cpu: cpu._jp_nn(), 4),  # JP nn
        0xC4: (lambda cpu: cpu._call_nz(), 3),  # CALL NZ, nn
        0xC5: (lambda cpu: cpu._push("B", "C"), 4),  # PUSH BC
        0xC6: (lambda cpu: cpu._add_n("A"), 2),  # ADD A, n
        0xC7: (lambda cpu: cpu._rst(0x00), 4),  # RST 00H
        0xC8: (lambda cpu: cpu._ret_z(), 2),  # RET Z
        0xC9: (lambda cpu: cpu._ret(), 4),  # RET
        # JP Z, nn # 3 w/o branch, 4 w/ branch
        0xCA: (lambda cpu: cpu._jp_z_nn(), 3),
        0xCB: (lambda cpu: cpu._cb(), 0),  # CB prefix
        # CALL Z, nn # 3 w/o branch, 6 w/ branch
        0xCC: (lambda cpu: cpu._call_z_nn(), 3),
        0xCD: (lambda cpu: cpu._call_nn(), 6),  # CALL nn
        0xCE: (lambda cpu: cpu._adc_n("A"), 2),  # ADC A, n
        0xCF: (lambda cpu: cpu._rst(0x08), 4),  # RST 08H

        0xD0: (lambda cpu: cpu._ret_nc(), 2),  # RET NC 5 w/ branch
        0xD1: (lambda cpu: cpu._pop("D", "E"), 3),  # POP DE
        0xD2: (lambda cpu: cpu._jp_nc_nn(), 3),  # JP NC, nn cy4 w/ branch
        0xD3: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xD4: (lambda cpu: cpu._call_nc_nn(), 3),  # CALL NC, nn 6 w/ branch
        0xD5: (lambda cpu: cpu._push("D", "E"), 4),  # PUSH DE
        0xD6: (lambda cpu: cpu._sub_n("A"), 2),  # SUB A, n
        0xD7: (lambda cpu: cpu._rst(0x10), 4),  # RST 10H
        0xD8: (lambda cpu: cpu._ret_c(), 2),  # RET C
        0xD9: (lambda cpu: cpu._reti(), 4),  # RETI
        0xDA: (lambda cpu: cpu._jp_c_nn(), 3),  # JP C, nn 4 w/ branch
        0xDB: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xDC: (lambda cpu: cpu._call_c_nn(), 3),  # CALL C, nn 6 w/ branch
        0xDD: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xDE: (lambda cpu: cpu._sbc_n("A"), 2),  # SBC A, n
        0xDF: (lambda cpu: cpu._rst(0x18), 4),  # RST 18H

        0xE0: (lambda cpu: cpu._ldh_n_a(), 3),  # LDH (n), A
        0xE1: (lambda cpu: cpu._pop("H", "L"), 3),  # POP HL
        0xE2: (lambda cpu: cpu._ldh_c_a(), 2),  # LDH (C), A
        0xE3: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xE4: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xE5: (lambda cpu: cpu._push("H", "L"), 4),  # PUSH HL
        0xE6: (lambda cpu: cpu._and_n("A"), 2),  # AND A, n
        0xE7: (lambda cpu: cpu._rst(0x20), 4),  # RST 20H
        0xE8: (lambda cpu: cpu._add_sp_n(), 4),  # ADD SP, n
        0xE9: (lambda cpu: cpu._jp_hl(), 1),  # JP (HL)
        0xEA: (lambda cpu: cpu._ld_nn_r(), 4),  # LD (nn), A
        0xEB: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xEC: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xED: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xEE: (lambda cpu: cpu._xor_n("A"), 2),  # XOR A, n
        0xEF: (lambda cpu: cpu._rst(0x28), 4),  # RST 28H

        0xF0: (lambda cpu: cpu._ldh_a_n(), 3),  # LDH A, (n)
        0xF1: (lambda cpu: cpu._pop("A", "F"), 3),  # POP AF
        0xF2: (lambda cpu: cpu._ldh_a_c(), 2),  # LDH A, (C)
        0xF3: (lambda cpu: cpu._di(), 1),  # DI
        0xF4: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xF5: (lambda cpu: cpu._push("A", "F"), 4),  # PUSH AF
        0xF6: (lambda cpu: cpu._or_n("A"), 2),  # OR A, n
        0xF7: (lambda cpu: cpu._rst(0x30), 4),  # RST 30H
        0xF8: (lambda cpu: cpu._ld_hl_sp_n(), 3),  # LD HL, SP+n
        0xF9: (lambda cpu: cpu._ld_sp_hl(), 2),  # LD SP, HL
        0xFA: (lambda cpu: cpu._ld_r_nn(), 4),  # LD A, (nn)
        0xFB: (lambda cpu: cpu._ei(), 1),  # EI
        0xFC: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xFD: (lambda cpu: cpu._illegal(), 0),  # Illegal opcode
        0xFE: (lambda cpu: cpu._cp_n(), 2),  # CP A, n
        0xFF: (lambda cpu: cpu._rst(0x38), 4),  # RST 38H
    }

    cb_opcodes = {
        0x00: (lambda cpu: cpu._rlc("B"), 2),  # RLC B
        0x01: (lambda cpu: cpu._rlc("C"), 2),  # RLC C
        0x02: (lambda cpu: cpu._rlc("D"), 2),  # RLC D
        0x03: (lambda cpu: cpu._rlc("E"), 2),  # RLC E
        0x04: (lambda cpu: cpu._rlc("H"), 2),  # RLC H
        0x05: (lambda cpu: cpu._rlc("L"), 2),  # RLC L
        0x06: (lambda cpu: cpu._rlc_hl(), 4),  # RLC (HL)
        0x07: (lambda cpu: cpu._rlc("A"), 2),  # RLC A
        0x08: (lambda cpu: cpu._rrc("B"), 2),  # RRC B
        0x09: (lambda cpu: cpu._rrc("C"), 2),  # RRC C
        0x0A: (lambda cpu: cpu._rrc("D"), 2),  # RRC D
        0x0B: (lambda cpu: cpu._rrc("E"), 2),  # RRC E
        0x0C: (lambda cpu: cpu._rrc("H"), 2),  # RRC H
        0x0D: (lambda cpu: cpu._rrc("L"), 2),  # RRC L
        0x0E: (lambda cpu: cpu._rrc_hl(), 4),  # RRC (HL)
        0x0F: (lambda cpu: cpu._rrc("A"), 2),  # RRC A

        0x10: (lambda cpu: cpu._rl("B"), 2),  # RL B
        0x11: (lambda cpu: cpu._rl("C"), 2),  # RL C
        0x12: (lambda cpu: cpu._rl("D"), 2),  # RL D
        0x13: (lambda cpu: cpu._rl("E"), 2),  # RL E
        0x14: (lambda cpu: cpu._rl("H"), 2),  # RL H
        0x15: (lambda cpu: cpu._rl("L"), 2),  # RL L
        0x16: (lambda cpu: cpu._rl_hl(), 4),  # RL (HL)
        0x17: (lambda cpu: cpu._rl("A"), 2),  # RL A
        0x18: (lambda cpu: cpu._rr("B"), 2),  # RR B
        0x19: (lambda cpu: cpu._rr("C"), 2),  # RR C
        0x1A: (lambda cpu: cpu._rr("D"), 2),  # RR D
        0x1B: (lambda cpu: cpu._rr("E"), 2),  # RR E
        0x1C: (lambda cpu: cpu._rr("H"), 2),  # RR H
        0x1D: (lambda cpu: cpu._rr("L"), 2),  # RR L
        0x1E: (lambda cpu: cpu._rr_hl(), 4),  # RR (HL)
        0x1F: (lambda cpu: cpu._rr("A"), 2),  # RR A

        0x20: (lambda cpu: cpu._sla("B"), 2),  # SLA B
        0x21: (lambda cpu: cpu._sla("C"), 2),  # SLA C
        0x22: (lambda cpu: cpu._sla("D"), 2),  # SLA D
        0x23: (lambda cpu: cpu._sla("E"), 2),  # SLA E
        0x24: (lambda cpu: cpu._sla("H"), 2),  # SLA H
        0x25: (lambda cpu: cpu._sla("L"), 2),  # SLA L
        0x26: (lambda cpu: cpu._sla_hl(), 4),  # SLA (HL)
        0x27: (lambda cpu: cpu._sla("A"), 2),  # SLA A
        0x28: (lambda cpu: cpu._sra("B"), 2),  # SRA B
        0x29: (lambda cpu: cpu._sra("C"), 2),  # SRA C
        0x2A: (lambda cpu: cpu._sra("D"), 2),  # SRA D
        0x2B: (lambda cpu: cpu._sra("E"), 2),  # SRA E
        0x2C: (lambda cpu: cpu._sra("H"), 2),  # SRA H
        0x2D: (lambda cpu: cpu._sra("L"), 2),  # SRA L
        0x2E: (lambda cpu: cpu._sra_hl(), 4),  # SRA (HL)
        0x2F: (lambda cpu: cpu._sra("A"), 2),  # SRA A

        0x30: (lambda cpu: cpu._swap("B"), 2),  # SWAP B
        0x31: (lambda cpu: cpu._swap("C"), 2),  # SWAP C
        0x32: (lambda cpu: cpu._swap("D"), 2),  # SWAP D
        0x33: (lambda cpu: cpu._swap("E"), 2),  # SWAP E
        0x34: (lambda cpu: cpu._swap("H"), 2),  # SWAP H
        0x35: (lambda cpu: cpu._swap("L"), 2),  # SWAP L
        0x36: (lambda cpu: cpu._swap_hl(), 4),  # SWAP (HL)
        0x37: (lambda cpu: cpu._swap("A"), 2),  # SWAP A
        0x38: (lambda cpu: cpu._srl("B"), 2),  # SRL B
        0x39: (lambda cpu: cpu._srl("C"), 2),  # SRL C
        0x3A: (lambda cpu: cpu._srl("D"), 2),  # SRL D
        0x3B: (lambda cpu: cpu._srl("E"), 2),  # SRL E
        0x3C: (lambda cpu: cpu._srl("H"), 2),  # SRL H
        0x3D: (lambda cpu: cpu._srl("L"), 2),  # SRL L
        0x3E: (lambda cpu: cpu._srl_hl(), 4),  # SRL (HL)
        0x3F: (lambda cpu: cpu._srl("A"), 2),  # SRL A

        0x40: (lambda cpu: cpu._bit(0, "B"), 2),  # BIT 0, B
        0x41: (lambda cpu: cpu._bit(0, "C"), 2),  # BIT 0, C
        0x42: (lambda cpu: cpu._bit(0, "D"), 2),  # BIT 0, D
        0x43: (lambda cpu: cpu._bit(0, "E"), 2),  # BIT 0, E
        0x44: (lambda cpu: cpu._bit(0, "H"), 2),  # BIT 0, H
        0x45: (lambda cpu: cpu._bit(0, "L"), 2),  # BIT 0, L
        0x46: (lambda cpu: cpu._bit_hl(0), 3),  # BIT 0, (HL)
        0x47: (lambda cpu: cpu._bit(0, "A"), 2),  # BIT 0, A
        0x48: (lambda cpu: cpu._bit(1, "B"), 2),  # BIT 1, B
        0x49: (lambda cpu: cpu._bit(1, "C"), 2),  # BIT 1, C
        0x4A: (lambda cpu: cpu._bit(1, "D"), 2),  # BIT 1, D
        0x4B: (lambda cpu: cpu._bit(1, "E"), 2),  # BIT 1, E
        0x4C: (lambda cpu: cpu._bit(1, "H"), 2),  # BIT 1, H
        0x4D: (lambda cpu: cpu._bit(1, "L"), 2),  # BIT 1, L
        0x4E: (lambda cpu: cpu._bit_hl(1), 3),  # BIT 1, (HL)
        0x4F: (lambda cpu: cpu._bit(1, "A"), 2),  # BIT 1, A

        0x50: (lambda cpu: cpu._bit(2, "B"), 2),  # BIT 2, B
        0x51: (lambda cpu: cpu._bit(2, "C"), 2),  # BIT 2, C
        0x52: (lambda cpu: cpu._bit(2, "D"), 2),  # BIT 2, D
        0x53: (lambda cpu: cpu._bit(2, "E"), 2),  # BIT 2, E
        0x54: (lambda cpu: cpu._bit(2, "H"), 2),  # BIT 2, H
        0x55: (lambda cpu: cpu._bit(2, "L"), 2),  # BIT 2, L
        0x56: (lambda cpu: cpu._bit_hl(2), 3),  # BIT 2, (HL)
        0x57: (lambda cpu: cpu._bit(2, "A"), 2),  # BIT 2, A
        0x58: (lambda cpu: cpu._bit(3, "B"), 2),  # BIT 3, B
        0x59: (lambda cpu: cpu._bit(3, "C"), 2),  # BIT 3, C
        0x5A: (lambda cpu: cpu._bit(3, "D"), 2),  # BIT 3, D
        0x5B: (lambda cpu: cpu._bit(3, "E"), 2),  # BIT 3, E
        0x5C: (lambda cpu: cpu._bit(3, "H"), 2),  # BIT 3, H
        0x5D: (lambda cpu: cpu._bit(3, "L"), 2),  # BIT 3, L
        0x5E: (lambda cpu: cpu._bit_hl(3), 3),  # BIT 3, (HL)
        0x5F: (lambda cpu: cpu._bit(3, "A"), 2),  # BIT 3, A

        0x60: (lambda cpu: cpu._bit(4, "B"), 2),  # BIT 4, B
        0x61: (lambda cpu: cpu._bit(4, "C"), 2),  # BIT 4, C
        0x62: (lambda cpu: cpu._bit(4, "D"), 2),  # BIT 4, D
        0x63: (lambda cpu: cpu._bit(4, "E"), 2),  # BIT 4, E
        0x64: (lambda cpu: cpu._bit(4, "H"), 2),  # BIT 4, H
        0x65: (lambda cpu: cpu._bit(4, "L"), 2),  # BIT 4, L
        0x66: (lambda cpu: cpu._bit_hl(4), 3),  # BIT 4, (HL)
        0x67: (lambda cpu: cpu._bit(4, "A"), 2),  # BIT 4, A
        0x68: (lambda cpu: cpu._bit(5, "B"), 2),  # BIT 5, B
        0x69: (lambda cpu: cpu._bit(5, "C"), 2),  # BIT 5, C
        0x6A: (lambda cpu: cpu._bit(5, "D"), 2),  # BIT 5, D
        0x6B: (lambda cpu: cpu._bit(5, "E"), 2),  # BIT 5, E
        0x6C: (lambda cpu: cpu._bit(5, "H"), 2),  # BIT 5, H
        0x6D: (lambda cpu: cpu._bit(5, "L"), 2),  # BIT 5, L
        0x6E: (lambda cpu: cpu._bit_hl(5), 3),  # BIT 5, (HL)
        0x6F: (lambda cpu: cpu._bit(5, "A"), 2),  # BIT 5, A

        0x70: (lambda cpu: cpu._bit(6, "B"), 2),  # BIT 6, B
        0x71: (lambda cpu: cpu._bit(6, "C"), 2),  # BIT 6, C
        0x72: (lambda cpu: cpu._bit(6, "D"), 2),  # BIT 6, D
        0x73: (lambda cpu: cpu._bit(6, "E"), 2),  # BIT 6, E
        0x74: (lambda cpu: cpu._bit(6, "H"), 2),  # BIT 6, H
        0x75: (lambda cpu: cpu._bit(6, "L"), 2),  # BIT 6, L
        0x76: (lambda cpu: cpu._bit_hl(6), 3),  # BIT 6, (HL)
        0x77: (lambda cpu: cpu._bit(6, "A"), 2),  # BIT 6, A
        0x78: (lambda cpu: cpu._bit(7, "B"), 2),  # BIT 7, B
        0x79: (lambda cpu: cpu._bit(7, "C"), 2),  # BIT 7, C
        0x7A: (lambda cpu: cpu._bit(7, "D"), 2),  # BIT 7, D
        0x7B: (lambda cpu: cpu._bit(7, "E"), 2),  # BIT 7, E
        0x7C: (lambda cpu: cpu._bit(7, "H"), 2),  # BIT 7, H
        0x7D: (lambda cpu: cpu._bit(7, "L"), 2),  # BIT 7, L
        0x7E: (lambda cpu: cpu._bit_hl(7), 3),  # BIT 7, (HL)
        0x7F: (lambda cpu: cpu._bit(7, "A"), 2),  # BIT 7, A

        0x80: (lambda cpu: cpu._res(0, "B"), 2),  # RES 0, B
        0x81: (lambda cpu: cpu._res(0, "C"), 2),  # RES 0, C
        0x82: (lambda cpu: cpu._res(0, "D"), 2),  # RES 0, D
        0x83: (lambda cpu: cpu._res(0, "E"), 2),  # RES 0, E
        0x84: (lambda cpu: cpu._res(0, "H"), 2),  # RES 0, H
        0x85: (lambda cpu: cpu._res(0, "L"), 2),  # RES 0, L
        0x86: (lambda cpu: cpu._res_hl(0), 4),  # RES 0, (HL)
        0x87: (lambda cpu: cpu._res(0, "A"), 2),  # RES 0, A
        0x88: (lambda cpu: cpu._res(1, "B"), 2),  # RES 1, B
        0x89: (lambda cpu: cpu._res(1, "C"), 2),  # RES 1, C
        0x8A: (lambda cpu: cpu._res(1, "D"), 2),  # RES 1, D
        0x8B: (lambda cpu: cpu._res(1, "E"), 2),  # RES 1, E
        0x8C: (lambda cpu: cpu._res(1, "H"), 2),  # RES 1, H
        0x8D: (lambda cpu: cpu._res(1, "L"), 2),  # RES 1, L
        0x8E: (lambda cpu: cpu._res_hl(1), 4),  # RES 1, (HL)
        0x8F: (lambda cpu: cpu._res(1, "A"), 2),  # RES 1, A

        0x90: (lambda cpu: cpu._res(2, "B"), 2),  # RES 2, B
        0x91: (lambda cpu: cpu._res(2, "C"), 2),  # RES 2, C
        0x92: (lambda cpu: cpu._res(2, "D"), 2),  # RES 2, D
        0x93: (lambda cpu: cpu._res(2, "E"), 2),  # RES 2, E
        0x94: (lambda cpu: cpu._res(2, "H"), 2),  # RES 2, H
        0x95: (lambda cpu: cpu._res(2, "L"), 2),  # RES 2, L
        0x96: (lambda cpu: cpu._res_hl(2), 4),  # RES 2, (HL)
        0x97: (lambda cpu: cpu._res(2, "A"), 2),  # RES 2, A
        0x98: (lambda cpu: cpu._res(3, "B"), 2),  # RES 3, B
        0x99: (lambda cpu: cpu._res(3, "C"), 2),  # RES 3, C
        0x9A: (lambda cpu: cpu._res(3, "D"), 2),  # RES 3, D
        0x9B: (lambda cpu: cpu._res(3, "E"), 2),  # RES 3, E
        0x9C: (lambda cpu: cpu._res(3, "H"), 2),  # RES 3, H
        0x9D: (lambda cpu: cpu._res(3, "L"), 2),  # RES 3, L
        0x9E: (lambda cpu: cpu._res_hl(3), 4),  # RES 3, (HL)
        0x9F: (lambda cpu: cpu._res(3, "A"), 2),  # RES 3, A

        0xA0: (lambda cpu: cpu._res(4, "B"), 2),  # RES 4, B
        0xA1: (lambda cpu: cpu._res(4, "C"), 2),  # RES 4, C
        0xA2: (lambda cpu: cpu._res(4, "D"), 2),  # RES 4, D
        0xA3: (lambda cpu: cpu._res(4, "E"), 2),  # RES 4, E
        0xA4: (lambda cpu: cpu._res(4, "H"), 2),  # RES 4, H
        0xA5: (lambda cpu: cpu._res(4, "L"), 2),  # RES 4, L
        0xA6: (lambda cpu: cpu._res_hl(4), 4),  # RES 4, (HL)
        0xA7: (lambda cpu: cpu._res(4, "A"), 2),  # RES 4, A
        0xA8: (lambda cpu: cpu._res(5, "B"), 2),  # RES 5, B
        0xA9: (lambda cpu: cpu._res(5, "C"), 2),  # RES 5, C
        0xAA: (lambda cpu: cpu._res(5, "D"), 2),  # RES 5, D
        0xAB: (lambda cpu: cpu._res(5, "E"), 2),  # RES 5, E
        0xAC: (lambda cpu: cpu._res(5, "H"), 2),  # RES 5, H
        0xAD: (lambda cpu: cpu._res(5, "L"), 2),  # RES 5, L
        0xAE: (lambda cpu: cpu._res_hl(5), 4),  # RES 5, (HL)
        0xAF: (lambda cpu: cpu._res(5, "A"), 2),  # RES 5, A

        0xB0: (lambda cpu: cpu._res(6, "B"), 2),  # RES 6, B
        0xB1: (lambda cpu: cpu._res(6, "C"), 2),  # RES 6, C
        0xB2: (lambda cpu: cpu._res(6, "D"), 2),  # RES 6, D
        0xB3: (lambda cpu: cpu._res(6, "E"), 2),  # RES 6, E
        0xB4: (lambda cpu: cpu._res(6, "H"), 2),  # RES 6, H
        0xB5: (lambda cpu: cpu._res(6, "L"), 2),  # RES 6, L
        0xB6: (lambda cpu: cpu._res_hl(6), 4),  # RES 6, (HL)
        0xB7: (lambda cpu: cpu._res(6, "A"), 2),  # RES 6, A
        0xB8: (lambda cpu: cpu._res(7, "B"), 2),  # RES 7, B
        0xB9: (lambda cpu: cpu._res(7, "C"), 2),  # RES 7, C
        0xBA: (lambda cpu: cpu._res(7, "D"), 2),  # RES 7, D
        0xBB: (lambda cpu: cpu._res(7, "E"), 2),  # RES 7, E
        0xBC: (lambda cpu: cpu._res(7, "H"), 2),  # RES 7, H
        0xBD: (lambda cpu: cpu._res(7, "L"), 2),  # RES 7, L
        0xBE: (lambda cpu: cpu._res_hl(7), 4),  # RES 7, (HL)
        0xBF: (lambda cpu: cpu._res(7, "A"), 2),  # RES 7, A

        0xC0: (lambda cpu: cpu._set(0, "B"), 2),  # SET 0, B
        0xC1: (lambda cpu: cpu._set(0, "C"), 2),  # SET 0, C
        0xC2: (lambda cpu: cpu._set(0, "D"), 2),  # SET 0, D
        0xC3: (lambda cpu: cpu._set(0, "E"), 2),  # SET 0, E
        0xC4: (lambda cpu: cpu._set(0, "H"), 2),  # SET 0, H
        0xC5: (lambda cpu: cpu._set(0, "L"), 2),  # SET 0, L
        0xC6: (lambda cpu: cpu._set_hl(0), 4),  # SET 0, (HL)
        0xC7: (lambda cpu: cpu._set(0, "A"), 2),  # SET 0, A
        0xC8: (lambda cpu: cpu._set(1, "B"), 2),  # SET 1, B
        0xC9: (lambda cpu: cpu._set(1, "C"), 2),  # SET 1, C
        0xCA: (lambda cpu: cpu._set(1, "D"), 2),  # SET 1, D
        0xCB: (lambda cpu: cpu._set(1, "E"), 2),  # SET 1, E
        0xCC: (lambda cpu: cpu._set(1, "H"), 2),  # SET 1, H
        0xCD: (lambda cpu: cpu._set(1, "L"), 2),  # SET 1, L
        0xCE: (lambda cpu: cpu._set_hl(1), 4),  # SET 1, (HL)
        0xCF: (lambda cpu: cpu._set(1, "A"), 1),  # SET 1, A

        0xD0: (lambda cpu: cpu._set(2, "B"), 2),  # SET 2, B
        0xD1: (lambda cpu: cpu._set(2, "C"), 2),  # SET 2, C
        0xD2: (lambda cpu: cpu._set(2, "D"), 2),  # SET 2, D
        0xD3: (lambda cpu: cpu._set(2, "E"), 2),  # SET 2, E
        0xD4: (lambda cpu: cpu._set(2, "H"), 2),  # SET 2, H
        0xD5: (lambda cpu: cpu._set(2, "L"), 2),  # SET 2, L
        0xD6: (lambda cpu: cpu._set_hl(2), 4),  # SET 2, (HL)
        0xD7: (lambda cpu: cpu._set(2, "A"), 2),  # SET 2, A
        0xD8: (lambda cpu: cpu._set(3, "B"), 2),  # SET 3, B
        0xD9: (lambda cpu: cpu._set(3, "C"), 2),  # SET 3, C
        0xDA: (lambda cpu: cpu._set(3, "D"), 2),  # SET 3, D
        0xDB: (lambda cpu: cpu._set(3, "E"), 2),  # SET 3, E
        0xDC: (lambda cpu: cpu._set(3, "H"), 2),  # SET 3, H
        0xDD: (lambda cpu: cpu._set(3, "L"), 2),  # SET 3, L
        0xDE: (lambda cpu: cpu._set_hl(3), 4),  # SET 3, (HL)
        0xDF: (lambda cpu: cpu._set(3, "A"), 2),  # SET 3, A

        0xE0: (lambda cpu: cpu._set(4, "B"), 2),  # SET 4, B
        0xE1: (lambda cpu: cpu._set(4, "C"), 2),  # SET 4, C
        0xE2: (lambda cpu: cpu._set(4, "D"), 2),  # SET 4, D
        0xE3: (lambda cpu: cpu._set(4, "E"), 2),  # SET 4, E
        0xE4: (lambda cpu: cpu._set(4, "H"), 2),  # SET 4, H
        0xE5: (lambda cpu: cpu._set(4, "L"), 2),  # SET 4, L
        0xE6: (lambda cpu: cpu._set_hl(4), 4),  # SET 4, (HL)
        0xE7: (lambda cpu: cpu._set(4, "A"), 2),  # SET 4, A
        0xE8: (lambda cpu: cpu._set(5, "B"), 2),  # SET 5, B
        0xE9: (lambda cpu: cpu._set(5, "C"), 2),  # SET 5, C
        0xEA: (lambda cpu: cpu._set(5, "D"), 2),  # SET 5, D
        0xEB: (lambda cpu: cpu._set(5, "E"), 2),  # SET 5, E
        0xEC: (lambda cpu: cpu._set(5, "H"), 2),  # SET 5, H
        0xED: (lambda cpu: cpu._set(5, "L"), 2),  # SET 5, L
        0xEE: (lambda cpu: cpu._set_hl(5), 4),  # SET 5, (HL)
        0xEF: (lambda cpu: cpu._set(5, "A"), 2),  # SET 5, A

        0xF0: (lambda cpu: cpu._set(6, "B"), 2),  # SET 6, B
        0xF1: (lambda cpu: cpu._set(6, "C"), 2),  # SET 6, C
        0xF2: (lambda cpu: cpu._set(6, "D"), 2),  # SET 6, D
        0xF3: (lambda cpu: cpu._set(6, "E"), 2),  # SET 6, E
        0xF4: (lambda cpu: cpu._set(6, "H"), 2),  # SET 6, H
        0xF5: (lambda cpu: cpu._set(6, "L"), 2),  # SET 6, L
        0xF6: (lambda cpu: cpu._set_hl(6), 4),  # SET 6, (HL)
        0xF7: (lambda cpu: cpu._set(6, "A"), 2),  # SET 6, A
        0xF8: (lambda cpu: cpu._set(7, "B"), 2),  # SET 7, B
        0xF9: (lambda cpu: cpu._set(7, "C"), 2),  # SET 7, C
        0xFA: (lambda cpu: cpu._set(7, "D"), 2),  # SET 7, D
        0xFB: (lambda cpu: cpu._set(7, "E"), 2),  # SET 7, E
        0xFC: (lambda cpu: cpu._set(7, "H"), 2),  # SET 7, H
        0xFD: (lambda cpu: cpu._set(7, "L"), 2),  # SET 7, L
        0xFE: (lambda cpu: cpu._set_hl(7), 4),  # SET 7, (HL)
        0xFF: (lambda cpu: cpu._set(7, "A"), 2),  # SET 7, A
    }

    # DEBUGGING
    def save_log_to_file(self, filename="log.txt"):
        # Temporarily redirect stdout to a StringIO object
        old_stdout = sys.stdout
        sys.stdout = log_output = io.StringIO()

        # Call the debug function to generate the log
        self.debug()

        # Restore the original stdout
        sys.stdout = old_stdout

        # Get the captured log output as a string
        log_content = log_output.getvalue()

        # Write the log content to the file
        with open(filename, "a") as file:
            file.write(log_content)

    def debug(self):
        reg_names = ['A', 'F', 'B', 'C', 'D', 'E', 'H', 'L']
        regs = [
            f"{name}:{hex(self.registers[name])[2:].zfill(2).upper()}" for name in reg_names]

        pc_hex = hex(self.PC)[2:].zfill(4).upper()
        sp_hex = hex(self.SP)[2:].zfill(4).upper()
        opcode_bytes = [hex(self.m.read_byte(self.PC + i))
                        [2:].zfill(2).upper() for i in range(4)]

        debug_output = f"{' '.join(regs)} SP:{sp_hex} PC:{pc_hex} PCMEM:{','.join(opcode_bytes)}"

        print(debug_output)

    def debug_timer_registers(self):
        print("DIV: 0x{:02X}".format(self.m.DIV))
        print("TIMA: 0x{:02X}".format(self.m.TIMA))
        print("TMA: 0x{:02X}".format(self.m.TMA))
        print("TAC: 0x{:02X}".format(self.m.TAC))
        print("tima_counter: {}".format(self.tima_counter))
        print()

    # TIMERS

    def update_timer(self, cycles):
        self.update_divider_register(cycles)
        self.update_tima(cycles)

    def update_divider_register(self, cycles):
        div_cycles = 256
        self.div_counter -= cycles

        while self.div_counter <= 0:
            self.div_counter += div_cycles
            self.m.DIV = (self.m.DIV + 1) & 0xFF

    def get_tima_cycles(self):
        freqs = {0: 1024, 1: 16, 2: 64, 3: 256}
        return freqs[self.m.TAC & 0x03]

    def update_tima(self, cycles):
        if not (self.m.TAC & 0x04):  # Check if the timer is enabled
            return

        tima_cycles = self.get_tima_cycles()
        self.tima_counter -= cycles

        while self.tima_counter <= 0:
            self.tima_counter += tima_cycles

            if self.m.TIMA >= 0xFF:
                self.m.TIMA = self.m.TMA
                self.request_interrupt(2)  # Request Timer Interrupt (bit 2)
            else:
                self.m.TIMA += 1

    # INTERRUPTS

    def handle_interrupts(self):
        pending_interrupts = self.m.IF & self.m.IE

        # exit halted state if interrupt is pending
        if self.halted and pending_interrupts:
            self.halted = False

        # handle interrupts only if IME is set
        if self.IME and pending_interrupts:
            self.IME = False
            self.halted = False
            self._push_pc()

            for i in range(5):
                if (pending_interrupts >> i) & 0x1:
                    self.m.IF &= ~(1 << i)
                    # print("Interrupt triggered: ", i)
                    # print("PC before: ", hex(self.PC))
                    # print("Going to: ", hex(0x0040 + (0x08 * i)))
                    self.PC = 0x0040 + (0x08 * i)
                    self.total_cycles += 5
                    break

    def request_interrupt(self, interrupt_num):
        self.m.IF |= 1 << interrupt_num

    def calc_clock_speed(self, start_time, end_time):
        elapsed_time = end_time - start_time
        clock_speed = self.total_cycles / elapsed_time
        return clock_speed

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

    def step(self):
        opcode = self.m.read_byte(self.PC)

        cycles = self.opcodes[opcode][1] if opcode != 0xCB else self.cb_opcodes[self.m.read_byte(
            self.PC + 1)][1]

        if opcode == 0xCB:
            self._cb()
        else:
            self.execute_opcode(opcode)

        self.update_timer(cycles) 
        return cycles
    
    def execute_next_instruction(self):
        opcode = self.m.read_byte(self.PC)

        cycles = self.opcodes[opcode][1] if opcode != 0xCB else self.cb_opcodes[self.m.read_byte(
            self.PC + 1)][1]

        if opcode == 0xCB:
            self._cb()
        else:
            self.execute_opcode(opcode)

        self.update_timer(cycles)

    def execute_opcode(self, opcode):
        try:
            # self.save_log_to_file()
            # if self.debug_mode:
            #     self.debug()
            if opcode in self.opcodes:
                opcode_func, cycles = self.opcodes[opcode]
                opcode_func(self)
                self.total_cycles += cycles
            else:
                raise ValueError(f"Unknown opcode: {hex(opcode)}")
        except ValueError as e:
            print(f"Error: {e}")

    def _cb(self):
        try:
            opcode = self.m.read_byte(self.PC + 1)
            # self.save_log_to_file()
            # if self.debug_mode:
            #     self.debug()
            if opcode in self.cb_opcodes:
                opcode_func, cycles = self.cb_opcodes[opcode]
                opcode_func(self)
                self.total_cycles = cycles
                self.PC += 1
            else:
                raise ValueError(f"Unknown opcode: {hex(opcode)}")
        except ValueError as e:
            print(f"Error: {e}")

    ### Instructions ###

    def _nop(self):
        self.PC += 1

    def _halt(self):  # TODO: implement further
        self.halted = True

    def _stop(self):  # TODO: implement once input is implemented
        self.m.DIV = 0x00  # must begin ticking once stop ends
        self.m.read_byte(self.PC)
        self.PC += 1

    # rotate functions

    def _rlca(self):
        carry = (self.registers["A"] & 0x80) >> 7
        self.registers["A"] = ((self.registers["A"] << 1) & 0xFF) | carry
        self.set_flag("Z", False)
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.set_flag("C", carry == 1)
        self.PC += 1

    def _rrca(self):
        carry = self.registers["A"] & 0x01
        self.registers["A"] = (self.registers["A"] >> 1) | (carry << 7)
        self.set_flag("Z", False)
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.set_flag("C", carry == 1)
        self.PC += 1

    def _rla(self):
        carry = (self.registers["A"] & 0x80) >> 7
        self.registers["A"] = ((self.registers["A"] << 1)
                               & 0xFF) | self.get_flag("C")
        self.set_flag("Z", False)
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.set_flag("C", carry == 1)
        self.PC += 1

    def _rra(self):
        carry = self.registers["A"] & 0x01
        self.registers["A"] = (self.registers["A"] >> 1) | (
            self.get_flag("C") << 7)

        self.set_flag("Z", False)
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.set_flag("C", carry != 0)
        self.PC += 1

    def _ccf(self):
        self.set_flag("C", not self.get_flag("C"))
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.PC += 1

    def _scf(self):
        self.registers["F"] = (self.registers["F"] & 0x90) | 0x10
        self.PC += 1

    def _cpl(self):
        self.registers["A"] = (~self.registers["A"]) & 0xFF
        self.set_flag("N", True)
        self.set_flag("H", True)
        self.PC += 1

    # LD functions
    def _ld_r_r(self, reg1, reg2):
        self.registers[reg1] = self.registers[reg2]
        self.PC += 1

    def _ld_r_n(self, reg1):
        self.registers[reg1] = self.m.read_byte(self.PC + 1)
        self.PC += 2

    def _ld_rr_nn(self, reg1, reg2):
        nn = self.m.read_word(self.PC + 1)
        self.PC += 3
        self.registers[reg1] = (nn >> 8) & 0xFF
        self.registers[reg2] = nn & 0xFF

    def _ld_r_nn(self):
        nn = self.m.read_word(self.PC + 1)
        # if self.PC == 0xC370:
        #     print("nn: ", hex(nn))
        #     print("Value at nn: ", hex(self.m.read_byte(nn)))
        self.PC += 3
        self.registers["A"] = self.m.read_byte(nn)

    def _ld_nn_r(self):
        self.m.write_byte(self.m.read_word(self.PC + 1), self.registers["A"])
        self.PC += 3

    def _ld_nn_sp(self):
        nn = self.m.read_word(self.PC + 1)
        self.PC += 3
        self.m.write_word(nn, self.SP)

    def _ld_sp_nn(self):
        self.SP = self.m.read_word(self.PC + 1)
        self.PC += 3

    def _ld_sp_hl(self):
        self.SP = self.registers["H"] << 8 | self.registers["L"]
        self.PC += 1

    def _ld_hl_sp_n(self):
        n = self.m.read_byte(self.PC + 1)
        n = n - 256 if n > 127 else n  # convert to signed
        result = (self.SP + n) & 0xFFFF
        self.registers["H"] = (result >> 8) & 0xFF
        self.registers["L"] = result & 0xFF
        self.PC += 2
        self.set_flag("Z", False)
        self.set_flag("N", False)
        self.set_flag("H", (self.SP ^ n ^ (result & 0xFFFF)) & 0x10)
        self.set_flag("C", (self.SP ^ n ^ (result & 0xFFFF)) & 0x100)

    def _ld_r_16(self, reg1, reg2, reg3):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = self.m.read_byte(address)
        self.PC += 1

    def _ld_16_r(self, reg1, reg2, reg3):
        address = self.registers[reg1] << 8 | self.registers[reg2]
        self.m.write_byte(address, self.registers[reg3])
        self.PC += 1

    def _ld_n_to_16(self, reg1, reg2):
        address = self.registers[reg1] << 8 | self.registers[reg2]
        self.m.write_byte(address, self.m.read_byte(self.PC + 1))
        self.PC += 2

    def _ldi_to_16(self, reg1, reg2, reg3):
        address = self.registers[reg1] << 8 | self.registers[reg2]
        self.m.write_byte(address, self.registers[reg3])
        self._inc_16(reg1, reg2)

    def _ldi_from_16(self, reg1, reg2, reg3):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = self.m.read_byte(address)
        self._inc_16(reg2, reg3)  # inc alredy increments PC

    def _ldd_to_16(self, reg1, reg2, reg3):
        address = self.registers[reg1] << 8 | self.registers[reg2]
        self.m.write_byte(address, self.registers[reg3])
        self._dec_16(reg1, reg2)

    def _ldd_from_16(self, reg1, reg2, reg3):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = self.m.read_byte(address)
        self._dec_16(reg2, reg3)

    # JR functions
    def _jr_n(self):
        offset = self.m.read_byte(self.PC + 1)
        # Convert to signed if the value is greater than 127
        offset = offset - 256 if offset > 127 else offset

        # Increment the PC by the size of the instruction (2 bytes)
        self.PC += 2
        self.PC += offset  # Add the offset to the PC

    def _jr_nz_n(self):
        offset = self.m.read_byte(self.PC + 1)
        if not self.get_flag("Z"):
            # self.PC += self.m.read_byte(self.PC) + 1
            offset = offset - 256 if offset > 127 else offset  # convert to signed
            self.PC += 2 + offset
        else:
            self.PC += 2

    def _jr_z_n(self):
        offset = self.m.read_byte(self.PC + 1)
        if self.get_flag("Z"):
            offset = offset - 256 if offset > 127 else offset  # convert to signed
            self.PC += 2 + offset
        else:
            self.PC += 2

    def _jr_nc_n(self):
        offset = self.m.read_byte(self.PC + 1)
        if not self.get_flag("C"):
            offset = offset - 256 if offset > 127 else offset  # convert to signed
            self.PC += 2 + offset
        else:
            self.PC += 2

    def _jr_c_n(self):
        offset = self.m.read_byte(self.PC + 1)
        if self.get_flag("C"):
            offset = offset - 256 if offset > 127 else offset  # convert to signed
            self.PC += 2 + offset
        else:
            self.PC += 2

    def _daa(self):
        a = self.registers["A"]
        adjust = 0
        carry = False

        if not self.get_flag("N"):
            if self.get_flag("H") or (a & 0x0F) > 0x09:
                adjust |= 0x06
            if self.get_flag("C") or a > 0x99:
                adjust |= 0x60
                carry = True
            a = (a + adjust) & 0xFF
        else:
            if self.get_flag("H"):
                adjust |= 0x06
            if self.get_flag("C"):
                adjust |= 0x60
                carry = True
            a = (a - adjust) & 0xFF

        self.registers["A"] = a

        self.set_flag("Z", a == 0)
        self.set_flag("H", False)
        self.set_flag("C", carry)
        self.PC += 1

    def _di(self):
        self.IME = False
        self.PC += 1

    def _ei(self):
        self.set_IME_next = True
        self.PC += 1

    # INC/DEC functions

    def _inc(self, reg):
        self.registers[reg] = (self.registers[reg] + 1) & 0xFF
        self.inc_flag(self.registers[reg])
        self.PC += 1

    def _inc_16(self, reg1, reg2):
        reg16 = ((self.registers[reg1] << 8 |
                  self.registers[reg2]) + 1) & 0xFFFF
        self.registers[reg1] = (reg16 >> 8) & 0xFF
        self.registers[reg2] = reg16 & 0xFF
        self.PC += 1

    def _inc_hl(self):
        address = (self.registers["H"] << 8) | self.registers["L"]
        val = self.m.read_byte(address)
        result = (val + 1) & 0xFF
        self.m.write_byte(address, result)

        # Update flags
        self.set_flag("Z", result == 0)
        self.set_flag("N", False)
        self.set_flag("H", (val & 0xF) + 1 > 0xF)

        self.PC += 1

    def _inc_sp(self):
        self.SP = (self.SP + 1) & 0xFFFF
        self.PC += 1

    def _dec_sp(self):
        self.SP = (self.SP - 1) & 0xFFFF
        self.PC += 1

    def _dec(self, reg):
        self.registers[reg] = (self.registers[reg] - 1) & 0xff
        self.dec_flag(self.registers[reg])
        self.PC += 1

    def _dec_16(self, reg1, reg2):
        reg16 = ((self.registers[reg1] << 8 |
                  self.registers[reg2]) - 1) & 0xFFFF
        self.registers[reg1] = (reg16 >> 8) & 0xFF
        self.registers[reg2] = reg16 & 0xFF
        self.PC += 1

    def _dec_hl(self):
        address = self.registers["H"] << 8 | self.registers["L"]
        value = self.m.read_byte(address)
        result = (value - 1) & 0xFF
        self.m.write_byte(address, result)

        self.set_flag("Z", result == 0)
        self.set_flag("N", True)
        self.set_flag("H", (value & 0x0F) == 0x00)  # Check for half-carry

        self.PC += 1

    # ADD functions

    def _add(self, reg1, reg2):
        og_val = self.registers[reg1]
        add_val = self.registers[reg2]
        result = self.registers[reg1] + add_val
        self.registers[reg1] = result & 0xFF
        self.add_flags(og_val, add_val, result)
        self.PC += 1

    def _add_n(self, reg1):
        og_val = self.registers[reg1]
        add_val = self.m.read_byte(self.PC + 1)
        result = self.registers[reg1] + add_val
        self.registers[reg1] = result & 0xFF

        self.PC += 2
        self.add_flags(og_val, add_val, result)

    def _add_sp_n(self):
        add_val = self.m.read_byte(self.PC + 1)
        add_val = add_val - 256 if add_val > 127 else add_val  # convert to signed
        original_sp = self.SP
        result = self.SP + add_val
        self.SP = result & 0xFFFF
        self.PC += 2
        self.set_flag("Z", False)
        self.set_flag("N", False)
        self.set_flag("H", (original_sp ^ add_val ^
                      (result & 0xFFFF)) & 0x10)
        self.set_flag("C", (original_sp ^ add_val ^
                      (result & 0xFFFF)) & 0x100)

    def _add_16_16(self, reg1, reg2, reg3, reg4):
        og_val1 = self.registers[reg1] << 8 | self.registers[reg2]
        og_val2 = self.registers[reg3] << 8 | self.registers[reg4]
        result = (og_val1 + og_val2) & 0xFFFF

        self.registers[reg1] = (result >> 8) & 0xFF
        self.registers[reg2] = result & 0xFF
        self.PC += 1
        self.ADD_16_16_Flags(og_val1, og_val2)

    def _add_16_sp(self, reg1, reg2):
        og_val1 = self.registers[reg1] << 8 | self.registers[reg2]
        og_val2 = self.SP
        result = (og_val1 + og_val2) & 0xFFFF

        self.registers[reg1] = (result >> 8) & 0xFF
        self.registers[reg2] = result & 0xFF
        self.PC += 1
        self.ADD_16_16_Flags(og_val1, og_val2)

    def _add_a_hl(self):
        og_val = self.registers["A"]
        add_val = self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"])
        result = self.registers["A"] + add_val
        self.registers["A"] = result & 0xFF
        self.PC += 1
        self.add_flags(og_val, add_val, result)

    def ADD_16_16_Flags(self, og_val1, og_val2):
        self.set_flag("N", False)
        self.set_flag("H", (og_val1 & 0xFFF) + (og_val2 & 0xFFF)
                      > 0xFFF)  # Check if half-carry occurred
        # Check if carry occurred
        self.set_flag("C", (og_val1 + og_val2) > 0xFFFF)

    # ADC functions

    def _adc(self, reg1, reg2):
        og_val = self.registers[reg1]
        add_val = self.registers[reg2]
        result = self.registers[reg1] + add_val + self.get_flag("C")
        self.registers[reg1] = result & 0xFF
        self.add_flags_c(og_val, add_val, result)
        self.PC += 1

    def _adc_from_16(self, reg1, reg2, reg3):
        og_val = self.registers[reg1]
        address = self.registers[reg2] << 8 | self.registers[reg3]
        add_val = self.m.read_byte(address)
        result = self.registers[reg1] + add_val + self.get_flag("C")
        self.registers[reg1] = result & 0xFF
        self.add_flags_c(og_val, add_val, result)
        self.PC += 1

    def _adc_n(self, reg):
        og_val = self.registers[reg]
        add_val = self.m.read_byte(self.PC + 1)
        result = self.registers[reg] + add_val + self.get_flag("C")
        self.registers[reg] = result & 0xFF
        self.add_flags_c(og_val, add_val, result)
        self.PC += 2

    def add_flags_c(self, og_val, add_val, result):
        self.set_flag("Z", (result & 0xFF) == 0)
        self.set_flag("N", False)
        self.set_flag("H", (og_val & 0xF) + (add_val & 0xF) +
                      self.get_flag("C") > 0xF)
        self.set_flag("C", result > 0xFF)

    # SUB functions

    def _sub(self, reg1, reg2):
        og_val = self.registers[reg1]
        sub_val = self.registers[reg2]
        result = og_val - sub_val
        self.registers[reg1] = result & 0xFF
        self.sub_flags(og_val, sub_val, result)
        self.PC += 1

    def _sub_from_16(self, reg1, reg2, reg3):
        og_val = self.registers[reg1]
        sub_val = self.m.read_byte(
            self.registers[reg2] << 8 | self.registers[reg3])
        result = og_val - sub_val
        self.registers[reg1] = result & 0xFF
        self.sub_flags(og_val, sub_val, result)
        self.PC += 1

    def _sub_n(self, reg1):
        original_val = self.registers[reg1]
        sub_val = self.m.read_byte(self.PC + 1)
        result = original_val - sub_val
        self.registers[reg1] = result & 0xFF
        self.PC += 2
        self.sub_flags(original_val, sub_val, result)

    # SBC functions

    def _sbc(self, reg1, reg2):
        og_val = self.registers[reg1]
        sub_val = self.registers[reg2]
        result = og_val - sub_val - self.get_flag("C")
        self.registers[reg1] = result & 0xFF
        self.sub_flags_c(og_val, sub_val, result)
        self.PC += 1

    def _sbc_from_16(self, reg1, reg2, reg3):
        og_val = self.registers[reg1]
        sub_val = self.m.read_byte(
            self.registers[reg2] << 8 | self.registers[reg3])
        result = og_val - sub_val - self.get_flag("C")
        self.registers[reg1] = result & 0xFF
        self.sub_flags_c(og_val, sub_val, result)
        self.PC += 1

    def _sbc_n(self, reg):
        og_val = self.registers[reg]
        sub_val = self.m.read_byte(self.PC + 1)
        result = og_val - sub_val - self.get_flag("C")
        self.registers[reg] = result & 0xFF
        self.PC += 2
        self.sub_flags_c(og_val, sub_val, result)

    def sub_flags_c(self, og_val, sub_val, result):
        masked_result = result & 0xFF
        self.set_flag("Z", masked_result == 0)
        self.set_flag("N", True)
        self.set_flag("H", (og_val & 0xF) - (sub_val & 0xF) -
                      self.get_flag("C") < 0)
        self.set_flag("C", result < 0)

    # AND functions
    def _and(self, reg1, reg2):
        self.registers[reg1] = self.registers[reg1] & self.registers[reg2]
        self.and_flags(self.registers[reg1])
        self.PC += 1

    def _and_from_16(self, reg1, reg2, reg3):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = self.registers[reg1] & self.m.read_byte(address)
        self.and_flags(self.registers[reg1])
        self.PC += 1

    def _and_n(self, reg):
        self.registers[reg] = self.registers[reg] & self.m.read_byte(
            self.PC + 1)
        self.PC += 2
        self.and_flags(self.registers[reg])

    # OR functions
    def _or(self, reg1, reg2):
        self.registers[reg1] = self.registers[reg1] | self.registers[reg2]
        self.or_flags(self.registers[reg1])
        self.PC += 1

    def _or_from_16(self, reg1, reg2, reg3):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = self.registers[reg1] | self.m.read_byte(address)
        self.or_flags(self.registers[reg1])
        self.PC += 1

    def _or_n(self, reg):
        self.registers[reg] = self.registers[reg] | self.m.read_byte(
            self.PC + 1)
        self.PC += 2
        self.or_flags(self.registers[reg])

    # XOR functions
    def _xor(self, reg1, reg2):
        self.registers[reg1] = self.registers[reg1] ^ self.registers[reg2]
        self.or_flags(self.registers[reg1])
        self.PC += 1

    def _xor_from_16(self, reg1, reg2, reg3):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = self.registers[reg1] ^ self.m.read_byte(address)
        self.or_flags(self.registers[reg1])
        self.PC += 1

    def _xor_n(self, reg):
        self.registers[reg] = self.registers[reg] ^ self.m.read_byte(
            self.PC + 1)
        self.PC += 2
        self.or_flags(self.registers[reg])

    # CP functions
    def _cp(self, reg):
        self.cp_flags(self.registers[reg])
        self.PC += 1

    def _cp_from_16(self, reg1, reg2):
        address = self.registers[reg1] << 8 | self.registers[reg2]
        self.cp_flags(self.m.read_byte(address))
        self.PC += 1

    def _cp_n(self):
        self.cp_flags(self.m.read_byte(self.PC + 1))
        self.PC += 2

    # RET functions
    def _ret_nz(self):
        if not self.get_flag("Z"):
            self.PC = self.m.read_word(self.SP)
            self.SP += 2
        else:
            self.PC += 1

    def _ret_z(self):
        if self.get_flag("Z"):
            self.PC = self.m.read_word(self.SP)
            self.SP += 2
        else:
            self.PC += 1

    def _ret_nc(self):
        if not self.get_flag("C"):
            self.PC = self.m.read_word(self.SP)
            self.SP += 2
        else:
            self.PC += 1

    def _ret_c(self):
        if self.get_flag("C"):
            self.PC = self.m.read_word(self.SP)
            self.SP += 2
        else:
            self.PC += 1

    def _ret(self):
        low = self.m.read_byte(self.SP)
        high = self.m.read_byte(self.SP + 1)
        self.PC = (high << 8) | low
        self.SP += 2

    # RETI functions

    def _reti(self):
        self._ret()
        self.IME = True

    # PUSH/POP functions

    def _pop(self, reg1, reg2):
        self.registers[reg1] = self.m.read_byte(self.SP + 1)
        self.registers[reg2] = self.m.read_byte(self.SP)

        if reg2 == "F":
            self.registers["F"] &= 0xF0  # clear lower 4 bits

        self.SP += 2
        self.PC += 1

    def _push(self, reg1, reg2):
        self.SP -= 2
        self.m.write_byte(self.SP, self.registers[reg2])
        self.m.write_byte(self.SP + 1, self.registers[reg1])
        self.PC += 1

    def _push_pc(self):
        self.SP -= 2
        self.m.write_byte(self.SP, self.PC & 0xFF)
        self.m.write_byte(self.SP + 1, self.PC >> 8)

    # JP functions

    def _jp_nz(self):
        if not self.get_flag("Z"):
            self.PC = self.m.read_word(self.PC + 1)
        else:
            self.PC += 3

    def _jp_nn(self):
        self.PC = self.m.read_word(self.PC + 1)

    def _jp_z_nn(self):
        if self.get_flag("Z"):
            self.PC = self.m.read_word(self.PC + 1)
        else:
            self.PC += 3

    def _jp_nc_nn(self):
        if not self.get_flag("C"):
            self.PC = self.m.read_word(self.PC + 1)
        else:
            self.PC += 3

    def _jp_c_nn(self):
        if self.get_flag("C"):
            self.PC = self.m.read_word(self.PC + 1)
        else:
            self.PC += 3

    def _jp_hl(self):
        self.PC = self.registers["H"] << 8 | self.registers["L"]

    # CALL functions

    def _call_nz(self):
        if not self.get_flag("Z"):
            self.SP -= 2
            self.m.write_word(self.SP, self.PC + 3)
            self.PC = self.m.read_word(self.PC + 1)
        else:
            self.PC += 3

    def _call_z_nn(self):
        if self.get_flag("Z"):
            self.SP -= 2
            self.m.write_word(self.SP, self.PC + 3)
            self.PC = self.m.read_word(self.PC + 1)
        else:
            self.PC += 3

    def _call_nn(self):
        self.SP -= 2
        self.m.write_word(self.SP, self.PC + 3)
        self.PC = self.m.read_word(self.PC + 1)

    def _call_nc_nn(self):
        if not self.get_flag("C"):
            self.SP -= 2
            self.m.write_word(self.SP, self.PC + 3)
            self.PC = self.m.read_word(self.PC + 1)
        else:
            self.PC += 3

    def _call_c_nn(self):
        if self.get_flag("C"):
            self.SP -= 2
            self.m.write_word(self.SP, self.PC + 3)
            self.PC = self.m.read_word(self.PC + 1)
        else:
            self.PC += 3

    # RST functions

    def _rst(self, addr):
        self.SP -= 2
        self.m.write_word(self.SP, self.PC + 1)
        self.PC = addr

    # LDH functions

    def _ldh_n_a(self):
        self.m.write_byte(
            0xFF00 + self.m.read_byte(self.PC + 1), self.registers["A"])
        self.PC += 2

    def _ldh_a_n(self):
        self.registers["A"] = self.m.read_byte(
            0xFF00 + self.m.read_byte(self.PC + 1))
        self.PC += 2

    def _ldh_c_a(self):
        self.m.write_byte(0xFF00 + self.registers["C"], self.registers["A"])
        self.PC += 1

    def _ldh_a_c(self):
        self.registers["A"] = self.m.read_byte(0xFF00 + self.registers["C"])
        self.PC += 1

    def _illegal(self):
        print("Illegal instruction at 0x{:04X}".format(self.PC))
        self.PC += 1

    # xCB prefixed functions

    # RLC/RRC functions
    def _rlc(self, reg):
        highest_bit = (self.registers[reg] & 0x80) >> 7
        self.registers[reg] = ((self.registers[reg] << 1) & 0xFF) | highest_bit
        self.rl_rr_flags(self.registers[reg])
        self.set_flag("C", highest_bit == 1)
        self.PC += 1

    def _rlc_hl(self):
        address = (self.registers["H"] << 8) | self.registers["L"]
        val = self.m.read_byte(address)
        carry = (val & 0x80) >> 7

        result = ((val << 1) & 0xFF) | carry
        self.m.write_byte(address, result)

        # Update flags
        self.set_flag("Z", result == 0)
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.set_flag("C", carry != 0)

        self.PC += 1

    def _rrc(self, reg):
        self.set_flag("C", self.registers[reg] & 0x01)
        self.registers[reg] = (self.registers[reg] >> 1) | (
            self.get_flag("C") << 7)
        self.rl_rr_flags(self.registers[reg])
        self.PC += 1

    def _rrc_hl(self):
        self.set_flag("C", self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]) & 0x01)
        self.m.write_byte(self.registers["H"] << 8 | self.registers["L"], (self.m.read_byte(self.registers["H"] << 8 | self.registers["L"]) >> 1) | (
            self.get_flag("C") << 7))
        self.rl_rr_flags(self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]))
        self.PC += 1

    # RL/RR functions
    def _rl(self, reg):
        carry = self.get_flag("C")
        self.set_flag("C", self.registers[reg] & 0x80)
        self.registers[reg] = ((self.registers[reg] << 1) & 0xFF) | carry
        self.rl_rr_flags(self.registers[reg])
        self.PC += 1

    def _rl_hl(self):
        carry = self.get_flag("C")
        self.set_flag("C", self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]) & 0x80)
        self.m.write_byte(self.registers["H"] << 8 | self.registers["L"], ((self.m.read_byte(self.registers["H"] << 8 | self.registers["L"]) << 1)
                                                                           & 0xFF) | carry)
        self.rl_rr_flags(self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]))
        self.PC += 1

    def _rr(self, reg):
        carry = self.get_flag("C")
        self.set_flag("C", self.registers[reg] & 0x01)
        self.registers[reg] = (self.registers[reg] >> 1) | (carry << 7)
        self.rl_rr_flags(self.registers[reg])
        self.PC += 1

    def _rr_hl(self):
        carry = self.get_flag("C")
        self.set_flag("C", self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]) & 0x01)
        self.m.write_byte(self.registers["H"] << 8 | self.registers["L"], (self.m.read_byte(self.registers["H"] << 8 | self.registers["L"]) >> 1) | (
            carry << 7))
        self.rl_rr_flags(self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]))
        self.PC += 1

    # SLA/SRA functions
    def _sla(self, reg):
        self.set_flag("C", self.registers[reg] & 0x80)
        self.registers[reg] = (self.registers[reg] << 1) & 0xFF
        self.rl_rr_flags(self.registers[reg])
        self.PC += 1

    def _sla_hl(self):
        self.set_flag("C", self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]) & 0x80)
        self.m.write_byte(self.registers["H"] << 8 | self.registers["L"], (self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]) << 1) & 0xFF)
        self.rl_rr_flags(self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]))
        self.PC += 1

    def _sra(self, reg):
        self.set_flag("C", self.registers[reg] & 0x01)
        self.registers[reg] = (self.registers[reg] >> 1) | (
            self.registers[reg] & 0x80)
        self.rl_rr_flags(self.registers[reg])
        self.PC += 1

    def _sra_hl(self):
        self.set_flag("C", self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]) & 0x01)
        self.m.write_byte(self.registers["H"] << 8 | self.registers["L"], (self.m.read_byte(self.registers["H"] << 8 | self.registers["L"]) >> 1) | (
            self.m.read_byte(self.registers["H"] << 8 | self.registers["L"]) & 0x80))
        self.rl_rr_flags(self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]))
        self.PC += 1

    # SWAP functions
    def _swap(self, reg):
        self.registers[reg] = ((self.registers[reg] & 0xF) << 4) | (
            (self.registers[reg] & 0xF0) >> 4)
        self.or_flags(self.registers[reg])
        self.PC += 1

    def _swap_hl(self):
        self.m.write_byte(self.registers["H"] << 8 | self.registers["L"], ((self.m.read_byte(self.registers["H"] << 8 | self.registers["L"]) & 0xF) << 4) | (
            (self.m.read_byte(self.registers["H"] << 8 | self.registers["L"]) & 0xF0) >> 4))
        self.or_flags(self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]))
        self.PC += 1

    # SRL functions
    def _srl(self, reg):
        self.set_flag("C", self.registers[reg] & 0x01)
        self.registers[reg] = self.registers[reg] >> 1
        self.rl_rr_flags(self.registers[reg])
        self.PC += 1

    def _srl_hl(self):
        self.set_flag("C", self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]) & 0x01)
        self.m.write_byte(self.registers["H"] << 8 | self.registers["L"], self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]) >> 1)
        self.rl_rr_flags(self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]))
        self.PC += 1

    # BIT functions
    def _bit(self, bit, reg):
        self.set_flag("Z", not (self.registers[reg] & (1 << bit)))
        self.set_flag("N", False)
        self.set_flag("H", True)
        self.PC += 1

    def _bit_hl(self, bit):
        self.set_flag("Z", not (self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]) & (1 << bit)))
        self.set_flag("N", False)
        self.set_flag("H", True)
        self.PC += 1

    # RES functions
    def _res(self, bit, reg):
        self.registers[reg] &= ~(1 << bit)
        self.PC += 1

    def _res_hl(self, bit):
        hl_address = (self.registers["H"] << 8) | self.registers["L"]
        value = self.m.read_byte(hl_address)
        value &= ~(1 << bit)
        self.m.write_byte(hl_address, value)
        self.PC += 1

    # SET functions
    def _set(self, bit, reg):
        self.registers[reg] |= (1 << bit)
        self.PC += 1

    def _set_hl(self, bit):
        self.m.write_byte(self.registers["H"] << 8 | self.registers["L"], self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]) | (1 << bit))
        self.PC += 1

    def _def_hl(self, bit):
        self.m.write_byte(self.registers["H"] << 8 | self.registers["L"], self.m.read_byte(
            self.registers["H"] << 8 | self.registers["L"]) | (1 << bit))
        self.PC += 1

    # Flag set implementations

    def add_flags(self, og_val, add_val, result):
        self.set_flag("Z", (result & 0xFF) == 0)
        self.set_flag("N", False)
        self.set_flag("H", ((og_val & 0xF) + (add_val & 0xF)) > 0xF)
        self.set_flag("C", result > 0xFF)

    def sub_flags(self, og_val, sub_val, result):
        self.set_flag("Z", result == 0)
        self.set_flag("N", True)
        self.set_flag("H", (og_val & 0xF) < (sub_val & 0xF))
        self.set_flag("C", og_val < sub_val)

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

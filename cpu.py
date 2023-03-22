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

        # interrupts
        self.IME: bool = False  # flag
        self.IF: int = 0x00  # interrupt request
        self.IE: int = 0x00  # interrupt enable

        self.check_cb: bool = False

        # shorten register names
        # self.A = self.registers["A"]
        # self.F = self.registers["F"]
        # self.B = self.registers["B"]
        # self.C = self.registers["C"]
        # self.D = self.registers["D"]
        # self.E = self.registers["E"]
        # self.H = self.registers["H"]
        # self.L = self.registers["L"]

    """
    16 means 16bit
    no number assums 8bit
    nn is the pc
    n is the pc + 1
    """
    opcodes = {
        0x00: lambda cpu: cpu._nop(),
        0x01: lambda cpu, m: cpu._ld_nn("B", "C", m),  # LD BC, nn
        0x02: lambda cpu, m: cpu._ld_to_16("B", "C", "A", m),  # LD (BC), A
        0x03: lambda cpu: cpu._inc_16("B", "C"),  # INC BC
        0x04: lambda cpu: cpu._inc("B"),  # INC B
        0x05: lambda cpu: cpu._dec("B"),  # DEC B
        0x06: lambda cpu, m: cpu._ld_n("B", m),  # LD B, n
        0x07: lambda cpu: cpu._rlca(),  # RLCA # IMPLEMENT
        0x08: lambda cpu, m: cpu._ld_nn_sp(m),  # LD (nn), SP
        0x09: lambda cpu: cpu._add_16_16("H", "L", "B", "C"),  # ADD HL, BC
        0x0A: lambda cpu, m: cpu._ld_from_16("A", "B", "C", m),  # LD A, (BC)
        0x0B: lambda cpu: cpu._dec_16("B", "C"),  # DEC BC
        0x0C: lambda cpu: cpu._inc("C"),  # INC C
        0x0D: lambda cpu: cpu._dec("C"),  # DEC C
        0x0E: lambda cpu, m: cpu._ld_n("C", m),  # LD C, n

        0x0F: lambda cpu: cpu._rrca(),  # RRCA # IMPLEMENT
        0x10: lambda cpu, m: cpu._stop(m),  # STOP # IMPLEMENT

        0x11: lambda cpu, m: cpu._ld_nn("D", "E", m),  # LD DE, nn
        0x12: lambda cpu, m: cpu._ld_to_16("D", "E", "A", m),  # LD (DE), A
        0x13: lambda cpu: cpu._inc_16("D", "E"),  # INC DE
        0x14: lambda cpu: cpu._inc("D"),  # INC D
        0x15: lambda cpu: cpu._dec("D"),  # DEC D
        0x16: lambda cpu, m: cpu._ld_n("D", m),  # LD D, n

        0x17: lambda cpu: cpu._rla(),  # RLA # IMPLEMENT
        0x18: lambda cpu, m: cpu._jr_n(m),  # JR n

        0x19: lambda cpu: cpu._add_16_16("H", "L", "D", "E"),  # ADD HL, DE
        0x1A: lambda cpu, m: cpu._ld_from_16("A", "D", "E", m),  # LD A, (DE)
        0x1B: lambda cpu: cpu._dec_16("D", "E"),  # DEC DE
        0x1C: lambda cpu: cpu._inc("E"),  # INC E
        0x1D: lambda cpu: cpu._dec("E"),  # DEC E
        0x1E: lambda cpu, m: cpu._ld_n("E", m),  # LD E, n

        0x1F: lambda cpu: cpu._rra(),  # RRA # IMPLEMENT
        0x20: lambda cpu, m: cpu._jr_nz_n(m),  # JR NZ, n

        0x21: lambda cpu, m: cpu._ld_nn("H", "L", m),  # LD HL, nn
        0x22: lambda cpu, m: cpu._ldi_to_16("H", "L", "A", m),  # LDI (HL), A
        0x23: lambda cpu: cpu._inc_16("H", "L"),  # INC HL
        0x24: lambda cpu: cpu._inc("H"),  # INC H
        0x25: lambda cpu: cpu._dec("H"),  # DEC H
        0x26: lambda cpu, m: cpu._ld_n("H", m),  # LD H, n
        0x27: lambda cpu: cpu._daa(),  # DAA # IMPLEMENT
        0x28: lambda cpu, m: cpu._jr_z_n(m),  # JR Z, n
        0x29: lambda cpu: cpu._add_16_16("H", "L", "H", "L"),  # ADD HL, HL
        0x2A: lambda cpu, m: cpu._ldi_from_16("A", "H", "L", m),  # LDI A, (HL)
        0x2B: lambda cpu: cpu._dec_16("H", "L"),  # DEC HL
        0x2C: lambda cpu: cpu._inc("L"),  # INC L
        0x2D: lambda cpu: cpu._dec("L"),  # DEC L
        0x2E: lambda cpu, m: cpu._ld_n("L", m),  # LD L, n

        0x2F: lambda cpu: cpu._cpl(),  # CPL # IMPLEMENT

        0x30: lambda cpu, m: cpu._jr_nc_n(m),  # JR NC, n
        0x31: lambda cpu, m: cpu._ld_sp_nn(m),  # LD SP, nn
        0x32: lambda cpu, m: cpu._ldd_to_16("H", "L", "A", m),  # LDD (HL), A
        0x33: lambda cpu: cpu._inc_pntrs("SP"),  # INC SP
        0x34: lambda cpu, m: cpu._inc_16("H", "L", m),  # INC (HL)
        0x35: lambda cpu, m: cpu._dec_16("H", "L", m),  # DEC (HL)
        0x36: lambda cpu, m: cpu._ld_n_to_16("H", "L", m),  # LD (HL), n

        0x37: lambda cpu: cpu._scf(),  # SCF # IMPLEMENT

        0x38: lambda cpu, m: cpu._jr_c_n(m),  # JR C, n
        0x39: lambda cpu: cpu._add_16_sp("H", "L"),  # ADD HL, SP
        0x3A: lambda cpu, m: cpu._ldd_from_16("A", "H", "L", m),  # LDD A, (HL)
        0x3B: lambda cpu: cpu._dec_pntrs("SP"),  # DEC SP
        0x3C: lambda cpu: cpu._inc("A"),  # INC A
        0x3D: lambda cpu: cpu._dec("A"),  # DEC A
        0x3E: lambda cpu, m: cpu._ld_n("A", m),  # LD A, n

        0x3F: lambda cpu: cpu._ccf(),  # CCF # IMPLEMENT

        0x40: lambda cpu, m: cpu._ld("B", "B", m),  # LD B, B
        0x41: lambda cpu, m: cpu._ld("B", "C", m),  # LD B, C
        0x42: lambda cpu, m: cpu._ld("B", "D", m),  # LD B, D
        0x43: lambda cpu, m: cpu._ld("B", "E", m),  # LD B, E
        0x44: lambda cpu, m: cpu._ld("B", "H", m),  # LD B, H
        0x45: lambda cpu, m: cpu._ld("B", "L", m),  # LD B, L
        0x46: lambda cpu, m: cpu._ld_from_16("B", "H", "L", m),  # LD B, (HL)
        0x47: lambda cpu, m: cpu._ld("B", "A", m),  # LD B, A
        0x48: lambda cpu, m: cpu._ld("C", "B", m),  # LD C, B
        0x49: lambda cpu, m: cpu._ld("C", "C", m),  # LD C, C
        0x4A: lambda cpu, m: cpu._ld("C", "D", m),  # LD C, D
        0x4B: lambda cpu, m: cpu._ld("C", "E", m),  # LD C, E
        0x4C: lambda cpu, m: cpu._ld("C", "H", m),  # LD C, H
        0x4D: lambda cpu, m: cpu._ld("C", "L", m),  # LD C, L
        0x4E: lambda cpu, m: cpu._ld_from_16("C", "H", "L", m),  # LD C, (HL)
        0x4F: lambda cpu, m: cpu._ld("C", "A", m),  # LD C, A
        0x50: lambda cpu, m: cpu._ld("D", "B", m),  # LD D, B
        0x51: lambda cpu, m: cpu._ld("D", "C", m),  # LD D, C
        0x52: lambda cpu, m: cpu._ld("D", "D", m),  # LD D, D
        0x53: lambda cpu, m: cpu._ld("D", "E", m),  # LD D, E
        0x54: lambda cpu, m: cpu._ld("D", "H", m),  # LD D, H
        0x55: lambda cpu, m: cpu._ld("D", "L", m),  # LD D, L
        0x56: lambda cpu, m: cpu._ld_from_16("D", "H", "L", m),  # LD D, (HL)
        0x57: lambda cpu, m: cpu._ld("D", "A", m),  # LD D, A
        0x58: lambda cpu, m: cpu._ld("E", "B", m),  # LD E, B
        0x59: lambda cpu, m: cpu._ld("E", "C", m),  # LD E, C
        0x5A: lambda cpu, m: cpu._ld("E", "D", m),  # LD E, D
        0x5B: lambda cpu, m: cpu._ld("E", "E", m),  # LD E, E
        0x5C: lambda cpu, m: cpu._ld("E", "H", m),  # LD E, H
        0x5D: lambda cpu, m: cpu._ld("E", "L", m),  # LD E, L
        0x5E: lambda cpu, m: cpu._ld_from_16("E", "H", "L", m),  # LD E, (HL)
        0x5F: lambda cpu, m: cpu._ld("E", "A", m),  # LD E, A
        0x60: lambda cpu, m: cpu._ld("H", "B", m),  # LD H, B
        0x61: lambda cpu, m: cpu._ld("H", "C", m),  # LD H, C
        0x62: lambda cpu, m: cpu._ld("H", "D", m),  # LD H, D
        0x63: lambda cpu, m: cpu._ld("H", "E", m),  # LD H, E
        0x64: lambda cpu, m: cpu._ld("H", "H", m),  # LD H, H
        0x65: lambda cpu, m: cpu._ld("H", "L", m),  # LD H, L
        0x66: lambda cpu, m: cpu._ld_from_16("H", "H", "L", m),  # LD H, (HL)
        0x67: lambda cpu, m: cpu._ld("H", "A", m),  # LD H, A
        0x68: lambda cpu, m: cpu._ld("L", "B", m),  # LD L, B
        0x69: lambda cpu, m: cpu._ld("L", "C", m),  # LD L, C
        0x6A: lambda cpu, m: cpu._ld("L", "D", m),  # LD L, D
        0x6B: lambda cpu, m: cpu._ld("L", "E", m),  # LD L, E
        0x6C: lambda cpu, m: cpu._ld("L", "H", m),  # LD L, H
        0x6D: lambda cpu, m: cpu._ld("L", "L", m),  # LD L, L
        0x6E: lambda cpu, m: cpu._ld_from_16("L", "H", "L", m),  # LD L, (HL)
        0x6F: lambda cpu, m: cpu._ld("L", "A", m),  # LD L, A
        0x70: lambda cpu, m: cpu._ld_to_16("H", "L", "B", m),  # LD (HL), B
        0x71: lambda cpu, m: cpu._ld_to_16("H", "L", "C", m),  # LD (HL), C
        0x72: lambda cpu, m: cpu._ld_to_16("H", "L", "D", m),  # LD (HL), D
        0x73: lambda cpu, m: cpu._ld_to_16("H", "L", "E", m),  # LD (HL), E
        0x74: lambda cpu, m: cpu._ld_to_16("H", "L", "H", m),  # LD (HL), H
        0x75: lambda cpu, m: cpu._ld_to_16("H", "L", "L", m),  # LD (HL), L

        0x76: lambda cpu: cpu._halt(),  # HALT (TODO: implement)

        0x77: lambda cpu, m: cpu._ld_to_16("H", "L", "A", m),  # LD (HL), A
        0x78: lambda cpu, m: cpu._ld("A", "B", m),  # LD A, B
        0x79: lambda cpu, m: cpu._ld("A", "C", m),  # LD A, C
        0x7A: lambda cpu, m: cpu._ld("A", "D", m),  # LD A, D
        0x7B: lambda cpu, m: cpu._ld("A", "E", m),  # LD A, E
        0x7C: lambda cpu, m: cpu._ld("A", "H", m),  # LD A, H
        0x7D: lambda cpu, m: cpu._ld("A", "L", m),  # LD A, L
        0x7E: lambda cpu, m: cpu._ld_from_16("A", "H", "L", m),  # LD A, (HL)
        0x7F: lambda cpu, m: cpu._ld("A", "A", m),  # LD A, A
        0x80: lambda cpu: cpu._add("A", "B"),  # ADD A, B
        0x81: lambda cpu: cpu._add("A", "C"),  # ADD A, C
        0x82: lambda cpu: cpu._add("A", "D"),  # ADD A, D
        0x83: lambda cpu: cpu._add("A", "E"),  # ADD A, E
        0x84: lambda cpu: cpu._add("A", "H"),  # ADD A, H
        0x85: lambda cpu: cpu._add("A", "L"),  # ADD A, L
        0x86: lambda cpu, m: cpu._add_from_16("A", "H", "L", m),  # ADD A, (HL)
        0x87: lambda cpu: cpu._add("A", "A"),  # ADD A, A

        0x88: lambda cpu: cpu._adc("A", "B"),  # ADC A, B
        0x89: lambda cpu: cpu._adc("A", "C"),  # ADC A, C
        0x8A: lambda cpu: cpu._adc("A", "D"),  # ADC A, D
        0x8B: lambda cpu: cpu._adc("A", "E"),  # ADC A, E
        0x8C: lambda cpu: cpu._adc("A", "H"),  # ADC A, H
        0x8D: lambda cpu: cpu._adc("A", "L"),  # ADC A, L
        0x8E: lambda cpu, m: cpu._adc_from_16("A", "H", "L", m),  # ADC A, (HL)
        0x8F: lambda cpu: cpu._adc("A", "A"),  # ADC A, A

        0x90: lambda cpu: cpu._sub("A", "B"),  # SUB A, B
        0x91: lambda cpu: cpu._sub("A", "C"),  # SUB A, C
        0x92: lambda cpu: cpu._sub("A", "D"),  # SUB A, D
        0x93: lambda cpu: cpu._sub("A", "E"),  # SUB A, E
        0x94: lambda cpu: cpu._sub("A", "H"),  # SUB A, H
        0x95: lambda cpu: cpu._sub("A", "L"),  # SUB A, L
        0x96: lambda cpu, m: cpu._sub_from_16("A", "H", "L", m),  # SUB A, (HL)
        0x97: lambda cpu: cpu._sub("A", "A"),  # SUB A, A

        0x98: lambda cpu: cpu._sbc("A", "B"),  # SBC A, B
        0x99: lambda cpu: cpu._sbc("A", "C"),  # SBC A, C
        0x9A: lambda cpu: cpu._sbc("A", "D"),  # SBC A, D
        0x9B: lambda cpu: cpu._sbc("A", "E"),  # SBC A, E
        0x9C: lambda cpu: cpu._sbc("A", "H"),  # SBC A, H
        0x9D: lambda cpu: cpu._sbc("A", "L"),  # SBC A, L
        0x9E: lambda cpu, m: cpu._sbc_from_16("A", "H", "L", m),  # SBC A, (HL)
        0x9F: lambda cpu: cpu._sbc("A", "A"),  # SBC A, A

        0xA0: lambda cpu: cpu._and("A", "B"),  # AND A, B
        0xA1: lambda cpu: cpu._and("A", "C"),  # AND A, C
        0xA2: lambda cpu: cpu._and("A", "D"),  # AND A, D
        0xA3: lambda cpu: cpu._and("A", "E"),  # AND A, E
        0xA4: lambda cpu: cpu._and("A", "H"),  # AND A, H
        0xA5: lambda cpu: cpu._and("A", "L"),  # AND A, L
        0xA6: lambda cpu, m: cpu._and_from_16("A", "H", "L", m),  # AND A, (HL)
        0xA7: lambda cpu: cpu._and("A", "A"),  # AND A, A

        0xA8: lambda cpu: cpu._xor("A", "B"),  # XOR A, B
        0xA9: lambda cpu: cpu._xor("A", "C"),  # XOR A, C
        0xAA: lambda cpu: cpu._xor("A", "D"),  # XOR A, D
        0xAB: lambda cpu: cpu._xor("A", "E"),  # XOR A, E
        0xAC: lambda cpu: cpu._xor("A", "H"),  # XOR A, H
        0xAD: lambda cpu: cpu._xor("A", "L"),  # XOR A, L
        0xAE: lambda cpu, m: cpu._xor_from_16("A", "H", "L", m),  # XOR A, (HL)
        0xAF: lambda cpu: cpu._xor("A", "A"),  # XOR A, A

        0xB0: lambda cpu: cpu._or("A", "B"),  # OR A, B
        0xB1: lambda cpu: cpu._or("A", "C"),  # OR A, C
        0xB2: lambda cpu: cpu._or("A", "D"),  # OR A, D
        0xB3: lambda cpu: cpu._or("A", "E"),  # OR A, E
        0xB4: lambda cpu: cpu._or("A", "H"),  # OR A, H
        0xB5: lambda cpu: cpu._or("A", "L"),  # OR A, L
        0xB6: lambda cpu, m: cpu._or_from_16("A", "H", "L", m),  # OR A, (HL)
        0xB7: lambda cpu: cpu._or("A", "A"),  # OR A, A

        0xB8: lambda cpu: cpu._cp("B"),  # CP A, B
        0xB9: lambda cpu: cpu._cp("C"),  # CP A, C
        0xBA: lambda cpu: cpu._cp("D"),  # CP A, D
        0xBB: lambda cpu: cpu._cp("E"),  # CP A, E
        0xBC: lambda cpu: cpu._cp("H"),  # CP A, H
        0xBD: lambda cpu: cpu._cp("L"),  # CP A, L
        0xBE: lambda cpu, m: cpu._cp_from_16("H", "L", m),  # CP A, (HL)
        0xBF: lambda cpu: cpu._cp("A"),  # CP A, A

        0xC0: lambda cpu, m: cpu._ret_nz(m),  # RET NZ
        0xC1: lambda cpu, m: cpu._pop("B", "C", m),  # POP BC
        0xC2: lambda cpu, m: cpu._jp_nz(m),  # JP NZ, nn
        0xC3: lambda cpu, m: cpu._jp_nn(m),  # JP nn
        0xC4: lambda cpu, m: cpu._call_nz(m),  # CALL NZ, nn
        0xC5: lambda cpu, m: cpu._push("B", "C", m),  # PUSH BC
        0xC6: lambda cpu, m: cpu._add_n("A", m),  # ADD A, n
        0xC7: lambda cpu, m: cpu._rst(0x00, m),  # RST 00H
        0xC8: lambda cpu, m: cpu._ret_z(m),  # RET Z
        0xC9: lambda cpu, m: cpu._ret(m),  # RET
        0xCA: lambda cpu, m: cpu._jp_z_nn(m),  # JP Z, nn
        0xCB: lambda cpu, m: cpu._cb(m),  # CB prefix
        0xCC: lambda cpu, m: cpu._call_z_nn(m),  # CALL Z, nn
        0xCD: lambda cpu, m: cpu._call_nn(m),  # CALL nn
        0xCE: lambda cpu, m: cpu._adc_n("A", m),  # ADC A, n
        0xCF: lambda cpu, m: cpu._rst(0x08, m),  # RST 08H

        0xD0: lambda cpu, m: cpu._ret_nc(m),  # RET NC
        0xD1: lambda cpu, m: cpu._pop("D", "E", m),  # POP DE
        0xD2: lambda cpu, m: cpu._jp_nc_nn(m),  # JP NC, nn
        0xD3: lambda cpu: cpu._illegal(),  # Illegal opcode
        0xD4: lambda cpu, m: cpu._call_nc_nn(m),  # CALL NC, nn
        0xD5: lambda cpu, m: cpu._push("D", "E", m),  # PUSH DE
        0xD6: lambda cpu, m: cpu._sub_n("A", m),  # SUB A, n
        0xD7: lambda cpu, m: cpu._rst(0x10, m),  # RST 10H
        0xD8: lambda cpu, m: cpu._ret_c(m),  # RET C
        0xD9: lambda cpu: cpu._reti(),  # RETI
        0xDA: lambda cpu, m: cpu._jp_c_nn(m),  # JP C, nn
        0xDB: lambda cpu: cpu._illegal(),  # Illegal opcode
        0xDC: lambda cpu, m: cpu._call_c_nn(m),  # CALL C, nn
        0xDD: lambda cpu: cpu._illegal(),  # Illegal opcode
        0xDE: lambda cpu, m: cpu._sbc_n("A", m),  # SBC A, n
        0xDF: lambda cpu, m: cpu._rst(0x18, m),  # RST 18H

        0xE0: lambda cpu, m: cpu._ldh_n_a(m),  # LDH (n), A
        0xE1: lambda cpu, m: cpu._pop("H", "L", m),  # POP HL
        0xE2: lambda cpu, m: cpu._ldh_c_a(m),  # LDH (C), A
        0xE3: lambda cpu: cpu._illegal(),  # Illegal opcode
        0xE4: lambda cpu: cpu._illegal(),  # Illegal opcode
        0xE5: lambda cpu, m: cpu._push("H", "L", m),  # PUSH HL
        0xE6: lambda cpu, m: cpu._and_n("A", m),  # AND A, n
        0xE7: lambda cpu, m: cpu._rst(0x20, m),  # RST 20H
        0xE8: lambda cpu, m: cpu._add_sp_n(m),  # ADD SP, n
        0xE9: lambda cpu: cpu._jp_hl(),  # JP (HL)
        0xEA: lambda cpu, m: cpu._ld_nn_a(m),  # LD (nn), A
        0xEB: lambda cpu: cpu._illegal(),  # Illegal opcode
        0xEC: lambda cpu: cpu._illegal(),  # Illegal opcode
        0xED: lambda cpu: cpu._illegal(),  # Illegal opcode
        0xEE: lambda cpu, m: cpu._xor_n("A", m),  # XOR A, n
        0xEF: lambda cpu, m: cpu._rst(0x28, m),  # RST 28H

        0xF0: lambda cpu, m: cpu._ldh_a_n(m),  # LDH A, (n)
        0xF1: lambda cpu, m: cpu._pop("A", "F", m),  # POP AF
        0xF2: lambda cpu, m: cpu._ldh_a_c(m),  # LDH A, (C)
        0xF3: lambda cpu: cpu._di(),  # DI
        0xF4: lambda cpu: cpu._illegal(),  # Illegal opcode
        0xF5: lambda cpu, m: cpu._push("A", "F", m),  # PUSH AF
        0xF6: lambda cpu, m: cpu._or_n("A", m),  # OR A, n
        0xF7: lambda cpu, m: cpu._rst(0x30, m),  # RST 30H
        0xF8: lambda cpu, m: cpu._ld_hl_sp_n(m),  # LD HL, SP+n
        0xF9: lambda cpu: cpu._ld_sp_hl(),  # LD SP, HL
        0xFA: lambda cpu, m: cpu._ld_a_nn(m),  # LD A, (nn)
        0xFB: lambda cpu: cpu._ei(),  # EI
        0xFC: lambda cpu: cpu._illegal(),  # Illegal opcode
        0xFD: lambda cpu: cpu._illegal(),  # Illegal opcode
        0xFE: lambda cpu, m: cpu._cp_n(m),  # CP A, n
        0xFF: lambda cpu, m: cpu._rst(0x38, m),  # RST 38H
    }

    cb_opcodes = {
        0x00: lambda cpu: cpu._rlc(cpu.B),
        0x01: lambda cpu, m: cpu._test_only(cpu.A, m)  # REMOVE
    }

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

    def execute_opcode(self, opcode, m):
        try:
            print("Executing opcode: ", hex(opcode))
            if opcode in self.opcodes:
                opcode_func = self.opcodes[opcode]
                num_args = len(inspect.signature(opcode_func).parameters)

                if num_args == 1:
                    opcode_func(self)
                elif num_args == 2:
                    opcode_func(self, m)
                else:
                    raise ValueError(
                        f"Unexpected number of arguments for opcode {hex(opcode)}: {num_args}")
            else:
                raise ValueError(f"Unknown opcode: {hex(opcode)}")
        except ValueError as e:
            print(f"Error: {e}")

    def _cb(self, opcode, m):
        try:
            print("Executing CB prefixed opcode: ", hex(opcode))
            if opcode in self.cb_opcodes:
                opcode_func = self.cb_opcodes[opcode]
                num_args = len(inspect.signature(opcode_func).parameters)

                if num_args == 1:
                    opcode_func(self)
                elif num_args == 2:
                    opcode_func(self, m)
                else:
                    raise ValueError(
                        f"Unexpected number of arguments for opcode {hex(opcode)}: {num_args}")
            else:
                raise ValueError(f"Unknown opcode: {hex(opcode)}")
        except ValueError as e:
            print(f"Error: {e}")

    ### Instructions ###

    def _nop(self):
        self.PC += 1

    def _halt(self):  # TODO: implement further
        self.PC += 1

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

    def _scf(self):
        self.registers["F"] = (self.registers["F"] & 0x90) | 0x10
        self.PC += 1

    def _cpl(self):
        self.registers["A"] = ~self.registers["A"]
        self.registers["F"] = (self.registers["F"] & 0x10) | 0x60
        self.PC += 1

    # LD functions
    def _ld(self, reg1, reg2, m):
        self.registers[reg1] = m.read_byte(self.registers[reg2])

    def _ld_n(self, reg1, m):
        self.registers[reg1] = m.read_byte(self.PC)
        self.PC += 1

    def _ld_nn(self, reg1, reg2, m):
        nn = m.read_word(self.PC)
        self.PC += 2
        self.registers[reg1] = (nn >> 8) & 0xFF
        self.registers[reg2] = nn & 0xFFa

    def _ld_a_nn(self, m):
        nn = m.read_word(self.PC)
        self.PC += 2
        self.registers["A"] = m.read_byte(nn)

    def _ld_nn_a(self, m):
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

    def _ld_from_16(self, reg1, reg2, reg3, m):
        address = self.registers[reg2] << 8 | self.registers[reg3]
        self.registers[reg1] = m.read_byte(address)

    def _ld_to_16(self, reg1, reg2, reg3, m):
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

    def _inc(self, reg1):
        self.registers[reg1] = (self.registers[reg1] + 1) & 0xFF
        self.inc_flag(self.registers[reg1])

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

    def _dec(self, reg1):
        self.registers[reg1] = (self.registers[reg1] - 1) & 0xFF
        self.dec_flag(self.registers[reg1])

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
        reg16 = (self.registers[reg1] << 8 | self.registers[reg2]) + \
            self.SP & 0xFFFF
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

    def inc_flag(self, reg):
        self.set_flag("Z", reg == 0)
        self.set_flag("N", False)
        self.set_flag("H", (reg & 0xF) == 0x0)

    def dec_flag(self, reg):
        self.set_flag("Z", reg == 0)
        self.set_flag("N", True)
        self.set_flag("H", (reg & 0xF) == 0xF)

    # for AND opcodes (same flags)

    def and_flags(self, value):
        self.set_flag("Z", value == 0)
        self.set_flag("N", False)
        self.set_flag("H", True)
        self.set_flag("C", False)

    # for OR opcodes (same flags)
    def or_flags(self, value):
        self.set_flag("Z", value == 0)
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.set_flag("C", False)

    # CP opcodes (same flags)
    def cp_flags(self, value):
        self.set_flag("Z", self.registers["A"] == value)
        self.set_flag("N", True)
        self.set_flag("H", (self.registers["A"] & 0xF) < (value & 0xF))
        self.set_flag("C", self.registers["A"] < value)
        self.set_flag("C", self.registers["A"] < value)

    def rot_flags(self, value):
        self.set_flag("Z", value == 0)
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.set_flag("C", value & 0x01)
        self.set_flag("C", value & 0x01)

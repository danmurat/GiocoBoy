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
            "PC": 0x100,
            "SP": 0xFFFE
        }
        self.flags = {
            "Z": 0x80,
            "N": 0x40,
            "H": 0x20,
            "C": 0x10
        }

        # interrupts
        self.IME = False  # flag
        self.IF = 0x00  # interrupt request
        self.IE = 0x00  # interrupt enable

        # shorten register names
        self.A = self.registers["A"]
        self.F = self.registers["F"]
        self.B = self.registers["B"]
        self.C = self.registers["C"]
        self.D = self.registers["D"]
        self.E = self.registers["E"]
        self.H = self.registers["H"]
        self.L = self.registers["L"]
        self.PC = self.registers["PC"]
        self.SP = self.registers["SP"]

    # Helper methods for flag manipulation
    def set_flag(self, flag, value):
        if flag in self.flags:
            if value:
                self.F |= self.flags[flag]
            else:
                self.F &= ~self.flags[flag]
        else:
            raise ValueError("Invalid flag")

    def get_flag(self, flag):
        if flag in self.flags:
            return (self.F & self.flags[flag]) != 0
        else:
            raise ValueError("Invalid flag")

    def execute_next_instruction(self, m):
        opcode = m.read_word(self.PC)
        self.PC += 2
        self.execute_instruction(opcode, m)

    def execute_instruction(self, opcode, m):
        match opcode:  # Python 3.10
            case 0x00:  # NOP
                pass

            case 0x01:  # LD BC, nn
                self.C = m.read_word(self.PC)
                self.PC += 2

            case 0x02:  # LD (BC), A
                m.write_word(self.B << 8 | self.C, self.A)

            case 0x03:  # INC BC
                self.C += 1

                if self.C > 0xFF:
                    self.C = 0
                self.B += 1

                if self.B > 0xFF:
                    self.B = 0

            case 0x04:  # INC B
                # ensure result stays in 8 bit range
                self.B = (self.B + 1) & 0xFF
                self.F = (self.F & 0xCF) | (0x20 if (self.B & 0x0F)
                                            == 0x00 else 0) | (0 if self.B else 0x80)
                self.set_flag("Z", self.B == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.B & 0x0F) == 0x00)

            case 0x05:  # DEC B
                self.B = (self.B - 1) & 0xFF
                self.set_flag("Z", self.B == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.B & 0x0F) == 0x0F)

            case 0x06:  # LD B, n
                self.B = m.read_byte(self.PC)

            case 0x07:  # RLCA
                self.A = (self.A << 1) | (self.A >> 7)
                self.set_flag("Z", False)
                self.set_flag("N", False)
                self.set_flag("H", False)
                self.set_flag("C", self.A > 0x7F)  # Set C flag to bit 7 of A

            case 0x08:  # LD (nn), SP
                m.write_word(m.read_word(self.PC), self.SP)
                self.PC += 2

            case 0x09:  # ADD HL, BC
                hl = (self.H << 8) | self.L
                bc = (self.B << 8) | self.C
                result = hl + bc

                self.H = (result >> 8) & 0xFF
                self.L = result & 0xFF

                self.set_flag("N", False)
                self.set_flag("H", (hl & 0xFFF) + (bc & 0xFFF) > 0xFFF)
                self.set_flag("C", result > 0xFFFF)

            case 0x0A:  # LD A, (BC)
                self.A = m.read_byte(self.B << 8 | self.C)

            case 0x0B:  # DEC BC
                self.B = (self.B - 1) & 0xFF
                self.C = (self.C - 1) & 0xFF

            case 0x0C:  # INC C
                self.C = (self.C + 1) & 0xFF
                self.set_flag("Z", self.C == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.C & 0x0F) == 0x00)

            case 0x0D:  # DEC C
                self.C = (self.C - 1) & 0xFF
                self.set_flag("Z", self.C == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.C & 0x0F) == 0x0F)

            case 0x0E:  # LD C, n
                self.C = m.read_byte(self.PC)
                self.PC += 1

            case 0x0F:  # RRCA
                self.A = (self.A >> 1) | (self.A << 7)
                self.set_flag("Z", False)
                self.set_flag("N", False)
                self.set_flag("H", False)
                self.set_flag("C", self.A > 0x7F)

            case 0x10:  # STOP
                pass  # Implement STOP behavior

            case 0x11:  # LD DE, nn
                self.E = m.read_byte(self.PC)
                self.D = m.read_byte(self.PC)
                self.PC += 2

            case 0x12:  # LD (DE), A
                m.write_byte(self.D << 8 | self.E, self.A)

            case 0x13:  # INC DE
                value = (self.D << 8) + self.E + 1
                self.D = (value >> 8) & 0xFF
                self.E = value & 0xFF

            case 0x14:  # INC D
                self.D = (self.D + 1) & 0xFF
                self.set_flag("Z", self.D == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.D & 0x0F) == 0x00)

            case 0x15:  # DEC D
                self.D = (self.D - 1) & 0xFF
                self.set_flag("Z", self.D == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.D & 0x0F) == 0x0F)

            case 0x16:  # LD D, n
                self.D = m.read_byte(self.PC)
                self.PC += 1

            case 0x17:  # RLA
                self.A = (self.A << 1) | (self.A >> 7)
                self.set_flag("Z", False)
                self.set_flag("N", False)
                self.set_flag("H", False)
                self.set_flag("C", self.A > 0x7F)

            case 0x18:  # JR n
                offset = m.read_byte(self.PC)
                self.PC += 1
                self.PC += offset

            case 0x19:  # ADD HL, DE
                self.H += self.D
                self.L += self.E
                self.set_flag("N", False)
                self.set_flag("H", (self.L & 0xF) < (self.E & 0xF))
                self.set_flag("C", (self.L & 0xFF) < (self.E & 0xFF))

            case 0x1A:  # LD A, (DE)
                self.A = m.read_byte(self.D << 8 | self.E)

            case 0x1B:  # DEC DE
                value = (self.D << 8) + self.E - 1
                self.D = (value >> 8) & 0xFF
                self.E = value & 0xFF

            case 0x1C:  # INC E
                self.E = (self.E + 1) & 0xFF
                self.set_flag("Z", self.E == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.E & 0x0F) == 0x00)

            case 0x1D:  # DEC E
                self.E = (self.E - 1) & 0xFF
                self.set_flag("Z", self.E == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.E & 0x0F) == 0x0F)

            case 0x1E:  # LD E, n
                self.E = m.read_byte(self.PC)
                self.PC += 1

            case 0x1F:  # RRA
                carry = self.A & 1
                self.A = ((self.A >> 1) | (carry << 7)) & 0xFF

            case 0x20:  # JR NZ, n
                offset = m.read_byte(self.PC)
                self.PC += 1
                if not self.F & 0x80:
                    self.PC += offset

            case 0x21:  # LD HL, nn
                self.L = m.read_byte(self.PC)
                self.PC += 1
                self.H = m.read_byte(self.PC)
                self.PC += 1

            case 0x22:  # LD (HL+), A
                m.write_byte(self.H << 8 | self.L, self.A)
                self.L += 1

            case 0x23:  # INC HL
                value = (self.H << 8) + self.L + 1
                self.H = (value >> 8) & 0xFF
                self.L = value & 0xFF

            case 0x24:  # INC H
                self.H = (self.H + 1) & 0xFF
                self.set_flag("Z", self.H == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.H & 0x0F) == 0x00)

            case 0x25:  # DEC H
                self.H = (self.H - 1) & 0xFF
                self.set_flag("Z", self.H == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.H & 0x0F) == 0x0F)

            case 0x26:  # LD H, n
                self.H = m.read_byte(self.PC)
                self.PC += 1

            case 0x27:  # DAA
                pass  # Implement DAA behavior

            case 0x28:  # JR Z, n
                offset = m.read_byte(self.PC)
                self.PC += 1
                if self.F & 0x80:
                    self.PC += offset

            case 0x29:  # ADD HL, HL
                hl = ((self.H << 8) | self.L) << 1

                self.set_flag("N", False)
                self.set_flag("H", hl > 0x0FFF)
                self.set_flag("C", hl > 0xFFFF)

                hl = hl & 0xFFFF
                self.H = (hl >> 8) & 0xFF
                self.L = hl & 0xFF

            case 0x2A:  # LD A, (HL+)
                self.A = m.read_byte(self.H << 8 | self.L)
                self.L = (self.L + 1) & 0xFF
                if self.L == 0:
                    self.H = (self.H + 1) & 0xFF

            case 0x2B:  # DEC HL
                value = (self.H << 8) + self.L - 1
                self.H = (value >> 8) & 0xFF
                self.L = value & 0xFF

            case 0x2C:  # INC L
                self.L = (self.L + 1) & 0xFF
                self.set_flag("Z", self.L == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.L & 0x0F) == 0x00)

            case 0x2D:  # DEC L
                self.L = (self.L - 1) & 0xFF
                self.set_flag("Z", self.L == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.L & 0x0F) == 0x0F)

            case 0x2E:  # LD L, n
                self.L = m.read_byte(self.PC)
                self.PC += 1

            case 0x2F:  # CPL
                self.A = ~self.A & 0xFF
                self.set_flag("N", True)
                self.set_flag("H", True)

            case 0x30:  # JR NC, n
                offset = m.read_byte(self.PC)
                self.PC += 1
                if not self.F & 0x10:
                    self.PC += offset

            case 0x31:  # LD SP, nn
                self.SP = m.read_word(self.PC)
                self.PC += 2

            case 0x32:  # LD (HL-), A
                hl = (self.H << 8) | self.L
                m.write_byte(hl, self.A)

                hl = (hl - 1) & 0xFFFF
                self.L = (hl >> 8) & 0xFF
                self.H = hl & 0xFF

            case 0x33:  # INC SP
                self.SP = (self.SP + 1) & 0xFFFF

            case 0x34:  # INC (HL)
                hl = (self.H << 8) | self.L
                value = m.read_byte(hl)
                value = (value + 1) & 0xFF
                m.write_byte(hl, value)

                self.set_flag("Z", value == 0)
                self.set_flag("N", False)
                self.set_flag("H", (value & 0x0F) == 0x00)

            case 0x35:  # DEC (HL)
                hl = (self.H << 8) | self.L
                value = m.read_byte(hl)
                value = (value - 1) & 0xFF
                m.write_byte(hl, value)

                self.set_flag("Z", value == 0)
                self.set_flag("N", True)
                self.set_flag("H", (value & 0x0F) == 0x0F)

            case 0x36:  # LD (HL), n
                hl = (self.H << 8) | self.L
                value = m.read_byte(self.PC)
                m.write_byte(hl, value)
                self.PC += 1

            case 0x37:  # SCF
                self.set_flag("N", False)
                self.set_flag("H", False)
                self.set_flag("C", True)

            case 0x38:  # JR C, n
                offset = m.read_byte(self.PC)
                self.PC += 1
                if self.F & 0x10:
                    self.PC += offset

            case 0x39:  # ADD HL, SP
                hl = (self.H << 8) | self.L
                hl = (hl + self.SP)

                self.set_flag("N", False)
                self.set_flag("H", (hl & 0x0FFF) < (self.SP & 0x0FFF))
                self.set_flag("C", hl < self.SP)

                hl = hl & 0xFFFF
                self.H = (hl >> 8) & 0xFF
                self.L = hl & 0xFF

            case 0x3A:  # LD A, (HL-)
                self.A = m.read_byte(self.H << 8 | self.L)
                self.L = (self.L - 1) & 0xFF
                if self.L == 0xFF:
                    self.H = (self.H - 1) & 0xFF

            case 0x3B:  # DEC SP
                self.SP = (self.SP - 1) & 0xFFFF

            case 0x3C:  # INC A
                self.A = (self.A + 1) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0x0F) == 0x00)

            case 0x3D:  # DEC A
                self.A = (self.A - 1) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0x0F) == 0x0F)

            case 0x3E:  # LD A, n
                self.A = m.read_byte(self.PC)
                self.PC += 1

            case 0x3F:  # CCF
                self.set_flag("N", False)
                self.set_flag("H", False)
                self.set_flag("C", not self.F & 0x10)

            case 0x40:  # LD B, B
                pass

            case 0x41:  # LD B, C
                self.B = self.C

            case 0x42:  # LD B, D
                self.B = self.D

            case 0x43:  # LD B, E
                self.B = self.E

            case 0x44:  # LD B, H
                self.B = self.H

            case 0x45:  # LD B, L
                self.B = self.L

            case 0x46:  # LD B, (HL)
                self.B = m.read_byte(self.H << 8 | self.L)

            case 0x47:  # LD B, A
                self.B = self.A

            case 0x48:  # LD C, B
                self.C = self.B

            case 0x49:  # LD C, C
                pass

            case 0x4A:  # LD C, D
                self.C = self.D

            case 0x4B:  # LD C, E
                self.C = self.E

            case 0x4C:  # LD C, H
                self.C = self.H

            case 0x4D:  # LD C, L
                self.C = self.L

            case 0x4E:  # LD C, (HL)
                self.C = m.read_byte(self.H << 8 | self.L)

            case 0x4F:  # LD C, A
                self.C = self.A

            case 0x50:  # LD D, B
                self.D = self.B

            case 0x51:  # LD D, C
                self.D = self.C

            case 0x52:  # LD D, D
                pass

            case 0x53:  # LD D, E
                self.D = self.E

            case 0x54:  # LD D, H
                self.D = self.H

            case 0x55:  # LD D, L
                self.D = self.L

            case 0x56:  # LD D, (HL)
                self.D = m.read_byte(self.H << 8 | self.L)

            case 0x57:  # LD D, A
                self.D = self.A

            case 0x58:  # LD E, B
                self.E = self.B

            case 0x59:  # LD E, C
                self.E = self.C

            case 0x5A:  # LD E, D
                self.E = self.D

            case 0x5B:  # LD E, E
                pass

            case 0x5C:  # LD E, H
                self.E = self.H

            case 0x5D:  # LD E, L
                self.E = self.L

            case 0x5E:  # LD E, (HL)
                self.E = m.read_byte(self.H << 8 | self.L)

            case 0x5F:  # LD E, A
                self.E = self.A

            case 0x60:  # LD H, B
                self.H = self.B

            case 0x61:  # LD H, C
                self.H = self.C

            case 0x62:  # LD H, D
                self.H = self.D

            case 0x63:  # LD H, E
                self.H = self.E

            case 0x64:  # LD H, H
                pass

            case 0x65:  # LD H, L
                self.H = self.L

            case 0x66:  # LD H, (HL)
                self.H = m.read_byte(self.H << 8 | self.L)

            case 0x67:  # LD H, A
                self.H = self.A

            case 0x68:  # LD L, B
                self.L = self.B

            case 0x69:  # LD L, C
                self.L = self.C

            case 0x6A:  # LD L, D
                self.L = self.D

            case 0x6B:  # LD L, E
                self.L = self.E

            case 0x6C:  # LD L, H
                self.L = self.H

            case 0x6D:  # LD L, L
                pass

            case 0x6E:  # LD L, (HL)
                self.L = m.read_byte(self.H << 8 | self.L)

            case 0x6F:  # LD L, A
                self.L = self.A

            case 0x70:  # LD (HL), B
                m.write_byte(self.H << 8 | self.L, self.B)

            case 0x71:  # LD (HL), C
                m.write_byte(self.H << 8 | self.L, self.C)

            case 0x72:  # LD (HL), D
                m.write_byte(self.H << 8 | self.L, self.D)

            case 0x73:  # LD (HL), E
                m.write_byte(self.H << 8 | self.L, self.E)

            case 0x74:  # LD (HL), H
                m.write_byte(self.H << 8 | self.L, self.H)

            case 0x75:  # LD (HL), L
                m.write_byte(self.H << 8 | self.L, self.L)

            case 0x76:  # HALT
                pass  # implement

            case 0x77:  # LD (HL), A
                m.write_byte(self.H << 8 | self.L, self.A)

            case 0x78:  # LD A, B
                self.A = self.B

            case 0x79:  # LD A, C
                self.A = self.C

            case 0x7A:  # LD A, D
                self.A = self.D

            case 0x7B:  # LD A, E
                self.A = self.E

            case 0x7C:  # LD A, H
                self.A = self.H

            case 0x7D:  # LD A, L
                self.A = self.L

            case 0x7E:  # LD A, (HL)
                self.A = m.read_byte(self.H << 8 | self.L)

            case 0x7F:  # LD A, A
                pass

            case 0x80:  # ADD A, B
                self.A = (self.A + self.B) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) + (self.B & 0xF) > 0xF)
                self.set_flag("C", self.A + self.B > 0xFF)

            case 0x81:  # ADD A, C
                self.A = (self.A + self.C) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) + (self.C & 0xF) > 0xF)
                self.set_flag("C", self.A + self.C > 0xFF)

            case 0x82:  # ADD A, D
                self.A = (self.A + self.D) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) + (self.D & 0xF) > 0xF)
                self.set_flag("C", self.A + self.D > 0xFF)

            case 0x83:  # ADD A, E
                self.A = (self.A + self.E) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) + (self.E & 0xF) > 0xF)
                self.set_flag("C", self.A + self.E > 0xFF)

            case 0x84:  # ADD A, H
                self.A = (self.A + self.H) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) + (self.H & 0xF) > 0xF)
                self.set_flag("C", self.A + self.H > 0xFF)

            case 0x85:  # ADD A, L
                self.A = (self.A + self.L) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) + (self.L & 0xF) > 0xF)
                self.set_flag("C", self.A + self.L > 0xFF)

            case 0x86:  # ADD A, (HL)
                self.A = (self.A + m.read_byte(self.H << 8 | self.L)) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) +
                              (m.read_byte(self.H << 8 | self.L) & 0xF) > 0xF)
                self.set_flag(
                    "C", self.A + m.read_byte(self.H << 8 | self.L) > 0xFF)

            case 0x87:  # ADD A, A
                self.A = (self.A + self.A) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) + (self.A & 0xF) > 0xF)
                self.set_flag("C", self.A + self.A > 0xFF)

            case 0x88:  # ADC A, B
                self.A = (self.A + self.B + self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) +
                              (self.B & 0xF) + self.get_flag("C") > 0xF)
                self.set_flag("C", self.A + self.B +
                              self.get_flag("C") > 0xFF)

            case 0x89:  # ADC A, C
                self.A = (self.A + self.C + self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) +
                              (self.C & 0xF) + self.get_flag("C") > 0xF)
                self.set_flag("C", self.A + self.C +
                              self.get_flag("C") > 0xFF)

            case 0x8A:  # ADC A, D
                self.A = (self.A + self.D + self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) +
                              (self.D & 0xF) + self.get_flag("C") > 0xF)
                self.set_flag("C", self.A + self.D +
                              self.get_flag("C") > 0xFF)

            case 0x8B:  # ADC A, E
                self.A = (self.A + self.E + self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) +
                              (self.E & 0xF) + self.get_flag("C") > 0xF)
                self.set_flag("C", self.A + self.E +
                              self.get_flag("C") > 0xFF)

            case 0x8C:  # ADC A, H
                self.A = (self.A + self.H + self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) +
                              (self.H & 0xF) + self.get_flag("C") > 0xF)
                self.set_flag("C", self.A + self.H +
                              self.get_flag("C") > 0xFF)

            case 0x8D:  # ADC A, L
                self.A = (self.A + self.L + self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) +
                              (self.L & 0xF) + self.get_flag("C") > 0xF)
                self.set_flag("C", self.A + self.L +
                              self.get_flag("C") > 0xFF)

            case 0x8E:  # ADC A, (HL)
                self.A = (self.A + m.read_byte(self.H << 8 | self.L) +
                          self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) + (m.read_byte(self.H <<
                              8 | self.L) & 0xF) + self.get_flag("C") > 0xF)
                self.set_flag("C", self.A + m.read_byte(self.H << 8 |
                              self.L) + self.get_flag("C") > 0xFF)

            case 0x8F:  # ADC A, A
                self.A = (self.A + self.A + self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) +
                              (self.A & 0xF) + self.get_flag("C") > 0xF)
                self.set_flag("C", self.A + self.A +
                              self.get_flag("C") > 0xFF)

            case 0x90:  # SUB B
                self.A = (self.A - self.B) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) - (self.B & 0xF) < 0)
                self.set_flag("C", self.A - self.B < 0)

            case 0x91:  # SUB C
                self.A = (self.A - self.C) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) - (self.C & 0xF) < 0)
                self.set_flag("C", self.A - self.C < 0)

            case 0x92:  # SUB D
                self.A = (self.A - self.D) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) - (self.D & 0xF) < 0)
                self.set_flag("C", self.A - self.D < 0)

            case 0x93:  # SUB E
                self.A = (self.A - self.E) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) - (self.E & 0xF) < 0)
                self.set_flag("C", self.A - self.E < 0)

            case 0x94:  # SUB H
                self.A = (self.A - self.H) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) - (self.H & 0xF) < 0)
                self.set_flag("C", self.A - self.H < 0)

            case 0x95:  # SUB L
                self.A = (self.A - self.L) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) - (self.L & 0xF) < 0)
                self.set_flag("C", self.A - self.L < 0)

            case 0x96:  # SUB (HL)
                self.A = (self.A - m.read_byte(self.H << 8 | self.L)) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) -
                              (m.read_byte(self.H << 8 | self.L) & 0xF) < 0)
                self.set_flag(
                    "C", self.A - m.read_byte(self.H << 8 | self.L) < 0)

            case 0x97:  # SUB A
                self.A = (self.A - self.A) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) - (self.A & 0xF) < 0)
                self.set_flag("C", self.A - self.A < 0)

            case 0x98:  # SBC A, B
                self.A = (self.A - self.B - self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) -
                              (self.B & 0xF) - self.get_flag("C") < 0)
                self.set_flag("C", self.A - self.B - self.get_flag("C") < 0)

            case 0x99:  # SBC A, C
                self.A = (self.A - self.C - self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) -
                              (self.C & 0xF) - self.get_flag("C") < 0)
                self.set_flag("C", self.A - self.C - self.get_flag("C") < 0)

            case 0x9A:  # SBC A, D
                self.A = (self.A - self.D - self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) -
                              (self.D & 0xF) - self.get_flag("C") < 0)
                self.set_flag("C", self.A - self.D - self.get_flag("C") < 0)

            case 0x9B:  # SBC A, E
                self.A = (self.A - self.E - self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) -
                              (self.E & 0xF) - self.get_flag("C") < 0)
                self.set_flag("C", self.A - self.E - self.get_flag("C") < 0)

            case 0x9C:  # SBC A, H
                self.A = (self.A - self.H - self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) -
                              (self.H & 0xF) - self.get_flag("C") < 0)
                self.set_flag("C", self.A - self.H - self.get_flag("C") < 0)

            case 0x9D:  # SBC A, L
                self.A = (self.A - self.L - self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) -
                              (self.L & 0xF) - self.get_flag("C") < 0)
                self.set_flag("C", self.A - self.L - self.get_flag("C") < 0)

            case 0x9E:  # SBC A, (HL)
                self.A = (self.A - m.read_byte(self.H << 8 | self.L) -
                          self.get_flag("C")) & 0xFF

                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) - (m.read_byte(self.H <<
                              8 | self.L) & 0xF) - self.get_flag("C") < 0)
                self.set_flag("C", self.A - m.read_byte(self.H << 8 |
                              self.L) - self.get_flag("C") < 0)

            case 0x9F:  # SBC A, A
                self.A = (self.A - self.A - self.get_flag("C")) & 0xFF
                self.AND_flags(self.A)

            case 0xA0:  # AND B
                self.A = self.A & self.B
                self.AND_flags(self.A)

            case 0xA1:  # AND C
                self.A = self.A & self.C
                self.AND_flags(self.A)

            case 0xA2:  # AND D
                self.A = self.A & self.D
                self.AND_flags(self.A)

            case 0xA3:  # AND E
                self.A = self.A & self.E
                self.AND_flags(self.A)

            case 0xA4:  # AND H
                self.A = self.A & self.H
                self.AND_flags(self.A)

            case 0xA5:  # AND L
                self.A = self.A & self.L
                self.AND_flags(self.A)

            case 0xA6:  # AND (HL)
                self.A = self.A & m.read_byte(self.H << 8 | self.L)
                self.AND_flags(self.A)

            case 0xA7:  # AND A
                self.A = self.A & self.A
                self.AND_flags(self.A)

            case 0xA8:  # XOR B
                self.A = self.A ^ self.B
                self.OR_flags(self.A)

            case 0xA9:  # XOR C
                self.A = self.A ^ self.C
                self.OR_flags(self.A)

            case 0xAA:  # XOR D
                self.A = self.A ^ self.D
                self.OR_flags(self.A)

            case 0xAB:  # XOR E
                self.A = self.A ^ self.E
                self.OR_flags(self.A)

            case 0xAC:  # XOR H
                self.A = self.A ^ self.H
                self.OR_flags(self.A)

            case 0xAD:  # XOR L
                self.A = self.A ^ self.L
                self.OR_flags(self.A)

            case 0xAE:  # XOR (HL)
                self.A = self.A ^ m.read_byte(self.H << 8 | self.L)
                self.OR_flags(self.A)

            case 0xAF:  # XOR A
                self.A = self.A ^ self.A
                self.OR_flags(self.A)

            case 0xB0:  # OR B
                self.A = self.A | self.B
                self.OR_flags(self.A)

            case 0xB1:  # OR C
                self.A = self.A | self.C
                self.OR_flags(self.A)

            case 0xB2:  # OR D
                self.A = self.A | self.D
                self.OR_flags(self.A)

            case 0xB3:  # OR E
                self.A = self.A | self.E
                self.OR_flags(self.A)

            case 0xB4:  # OR H
                self.A = self.A | self.H
                self.OR_flags(self.A)

            case 0xB5:  # OR L
                self.A = self.A | self.L
                self.OR_flags(self.A)

            case 0xB6:  # OR (HL)
                self.A = self.A | m.read_byte(self.H << 8 | self.L)
                self.OR_flags(self.A)

            case 0xB7:  # OR A
                self.A = self.A | self.A
                self.OR_flags(self.A)

            case 0xB8:  # CP B
                self.CP_flags(self.B)

            case 0xB9:  # CP C
                self.CP_flags(self.C)

            case 0xBA:  # CP D
                self.CP_flags(self.D)

            case 0xBB:  # CP E
                self.CP_flags(self.E)

            case 0xBC:  # CP H
                self.CP_flags(self.H)

            case 0xBD:  # CP L
                self.CP_flags(self.L)

            case 0xBE:  # CP (HL)
                self.CP_flags(m.read_byte(self.H << 8 | self.L))

            case 0xBF:  # CP A
                self.CP_flags(self.A)

            case 0xC0:  # RET NZ
                if not self.get_flag("Z"):
                    self.PC = m.read_word(self.SP)
                    self.SP += 2

            case 0xC1:  # POP BC
                self.C = m.read_byte(self.SP)
                self.B = m.read_byte((self.SP + 1) & 0xFFFF)
                self.SP += 2

            case 0xC2:  # JP NZ, a16
                if not self.get_flag("Z"):
                    self.PC = m.read_byte(
                        self.PC + 1) | (m.read_byte(self.PC + 2) << 8)
                else:
                    self.PC += 2

            case 0xC3:  # JP a16
                self.PC = m.read_byte(
                    self.PC + 1) | (m.read_byte(self.PC + 2) << 8)

            case 0xC4:  # CALL NZ, a16
                if not self.get_flag("Z"):
                    self.SP -= 2
                    m.write_byte(self.SP, self.PC >> 8)
                    m.write_byte(self.SP + 1, self.PC & 0xFF)
                    self.PC = m.read_byte(
                        self.PC + 1) | (m.read_byte(self.PC + 2) << 8)
                else:
                    self.PC += 2

            case 0xC5:  # PUSH BC
                self.SP -= 2
                m.write_byte(self.SP, self.B)
                m.write_byte(self.SP + 1, self.C)

            case 0xC6:  # ADD A, d8
                self.A = self.A + m.read_byte(self.PC + 1)
                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) < (
                    m.read_byte(self.PC + 1) & 0xF))
                self.set_flag("C", self.A > 0xFF)
                self.A &= 0xFF
                self.PC += 1

            case 0xC7:  # RST 00H
                self.SP -= 2
                m.write_byte(self.SP, self.PC & 0xFF)
                m.write_byte(self.SP + 1, self.PC >> 8)
                self.PC = 0x00

            case 0xC8:  # RET Z
                if self.get_flag("Z"):
                    self.PC = m.read_byte(self.SP | (
                        m.read_byte(self.SP + 1) << 8))
                    self.SP += 2

            case 0xC9:  # RET
                self.PC = m.read_word(self.SP)
                self.SP += 2

            case 0xCA:  # JP Z, a16
                if self.get_flag("Z"):
                    self.PC = m.read_word(self.PC + 1)
                else:
                    self.PC += 2

            case 0xCB:  # CB prefix
                self.PC += 1
                opcode = m.read_byte(self.PC)
                self.execute_cb(opcode)

            case 0xCC:  # CALL Z, a16
                if self.get_flag("Z"):
                    self.SP -= 2
                    m.write_word(self.SP, self.PC + 2)
                    self.PC = m.read_word(self.PC + 1)
                else:
                    self.PC += 2

            case 0xCD:  # CALL a16
                self.SP -= 2
                m.write_word(self.SP, self.PC + 2)
                self.PC = m.read_word(self.PC + 1)

            case 0xCE:  # ADC A, d8
                self.A = self.A + m.read_byte(self.PC + 1) + self.get_flag("C")
                self.set_flag("Z", self.A == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.A & 0xF) < (
                    m.read_byte(self.PC + 1) & 0xF))
                self.set_flag("C", self.A > 0xFF)

            case 0xCF:  # RST 08H
                self.SP -= 2
                m.write_word(self.SP, self.PC)
                self.PC = 0x08

            case 0xD0:  # RET NC
                if not self.get_flag("C"):
                    self.PC = m.read_word(self.SP)
                    self.SP += 2

            case 0xD1:  # POP DE
                self.E = m.read_byte(self.SP)
                self.D = m.read_byte(self.SP + 1)
                self.SP += 2

            case 0xD2:  # JP NC, a16
                if not self.get_flag("C"):
                    self.PC = m.read_word(self.PC + 1)
                else:
                    self.PC += 2

            case 0xD3:  # Illegal opcode
                print("Illegal opcode: 0x%02X" % opcode)

            case 0xD4:  # CALL NC, a16
                if not self.get_flag("C"):
                    self.SP -= 2
                    m.write_word(self.SP, self.PC + 2)
                    self.PC = m.read_word(self.PC + 1)
                else:
                    self.PC += 2

            case 0xD5:  # PUSH DE
                self.SP -= 2
                m.write_byte(self.SP, self.D)
                m.write_byte(self.SP + 1, self.E)

            case 0xD6:  # SUB d8
                self.A = self.A - m.read_byte(self.PC + 1)
                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) > (
                    m.read_byte(self.PC + 1) & 0xF))
                self.set_flag("C", self.A < 0)
                self.A &= 0xFF
                self.PC += 1

            case 0xD7:  # RST 10H
                self.SP -= 2
                m.write_word(self.SP, self.PC)
                self.PC = 0x10

            case 0xD8:  # RET C
                if self.get_flag("C"):
                    self.PC = m.read_word(self.SP)
                    self.SP += 2

            case 0xD9:  # RETI
                self.PC = m.read_word(self.SP)
                self.SP += 2
                self.IME = True

            case 0xDA:  # JP C, a16
                if self.get_flag("C"):
                    self.PC = m.read_word(self.PC)
                else:
                    self.PC += 2

            case 0xDB:  # Illegal opcode
                print("Illegal opcode: 0x%02X" % opcode)

            case 0xDC:  # CALL C, a16
                if self.get_flag("C"):
                    self.SP -= 2
                    m.write_word(self.SP, self.PC + 2)
                    self.PC = m.read_word(self.PC)
                else:
                    self.PC += 2

            case 0xDD:  # Illegal opcode
                print("Illegal opcode: 0x%02X" % opcode)

            case 0xDE:  # SBC A, d8
                self.A = self.A - m.read_byte(self.PC + 1) - self.get_flag("C")
                self.set_flag("Z", self.A == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.A & 0xF) > (
                    m.read_byte(self.PC + 1) & 0xF))
                self.set_flag("C", self.A < 0)

            case 0xDF:  # RST 18H
                self.SP -= 2
                m.write_word(self.SP, self.PC)
                self.PC = 0x18

            case 0xE0:  # LDH (a8), A
                m.write_byte(0xFF00 + m.read_byte(self.PC + 1), self.A)
                self.PC += 1

            case 0xE1:  # POP HL
                self.L = m.read_byte(self.SP)
                self.H = m.read_byte(self.SP + 1)
                self.SP += 2

            case 0xE2:  # LD (C), A
                m.write_byte(0xFF00 + self.C, self.A)

            case 0xE3:  # Illegal opcode
                print("Illegal opcode: 0x%02X" % opcode)

            case 0xE4:  # Illegal opcode
                print("Illegal opcode: 0x%02X" % opcode)

            case 0xE5:  # PUSH HL
                self.SP -= 2
                m.write_byte(self.SP, self.H)
                m.write_byte(self.SP + 1, self.L)

            case 0xE6:  # AND d8
                self.A &= m.read_byte(self.PC + 1)
                self.AND_flags(self.A)
                self.PC += 2

            case 0xE7:  # RST 20H
                self.SP -= 2
                m.write_word(self.SP, self.PC)
                self.PC = 0x20

            case 0xE8:  # ADD SP, r8
                self.SP += m.read_byte(self.PC + 1)
                self.set_flag("Z", False)
                self.set_flag("N", False)
                self.set_flag("H", (self.SP & 0xF) < (
                    self.SP - m.read_byte(self.PC) & 0xF))
                self.set_flag("C", (self.SP & 0xFF) < (
                    self.SP - m.read_byte(self.PC + 1) & 0xFF))
                self.PC += 2

            case 0xE9:  # JP (HL)
                self.PC = self.H << 8 | self.L

            case 0xEA:  # LD (a16), A
                m.write_byte(m.read_word(self.PC), self.A)
                self.PC += 2

            case 0xEB:  # Illegal opcode
                print("Illegal opcode: 0x%02X" % opcode)

            case 0xEC:  # Illegal opcode
                print("Illegal opcode: 0x%02X" % opcode)

            case 0xED:  # Illegal opcode
                print("Illegal opcode: 0x%02X" % opcode)

            case 0xEE:  # XOR d8
                self.A ^= m.read_byte(self.PC + 1)
                self.OR_flags(self.A)

            case 0xEF:  # RST 28H
                self.SP -= 2
                m.write_word(self.SP, self.PC)
                self.PC = 0x28

            case 0xF0:  # LDH A, (a8)
                self.A = m.read_byte(0xFF00 + m.read_byte(self.PC + 1))
                self.PC += 1

            case 0xF1:  # POP AF
                self.F = m.read_byte(self.SP)
                self.A = m.read_byte(self.SP + 1)
                self.SP += 2

            case 0xF2:  # LD A, (C)
                self.A = m.read_byte(0xFF00 + self.C)

            case 0xF3:  # DI
                self.IME = False

            case 0xF4:  # Illegal opcode
                print("Illegal opcode: 0x%02X" % opcode)

            case 0xF5:  # PUSH AF
                self.SP -= 2
                m.write_byte(self.SP, self.F)
                m.write_byte(self.SP + 1, self.A)

            case 0xF6:  # OR d8
                self.A |= m.read_byte(self.PC + 1)
                self.OR_flags(self.A)
                self.PC += 2

            case 0xF7:  # RST 30H
                self.SP -= 2
                m.write_word(self.SP, self.PC)
                self.PC = 0x30

            case 0xF8:  # LD HL, SP+r8
                self.H = (self.SP + m.read_byte(self.PC + 1)) >> 8
                self.L = (self.SP + m.read_byte(self.PC + 1)) & 0xFF
                self.set_flag("Z", False)
                self.set_flag("N", False)
                self.set_flag("H", (self.SP & 0xF) < (
                    self.SP + m.read_byte(self.PC + 1) & 0xF))
                self.set_flag("C", (self.SP & 0xFF) < (
                    self.SP + m.read_byte(self.PC + 1) & 0xFF))
                self.PC += 2

            case 0xF9:  # LD SP, HL
                self.SP = self.H << 8 | self.L

            case 0xFA:  # LD A, (a16)
                self.A = m.read_byte(m.read_word(self.PC + 1))
                self.PC += 2

            case 0xFB:  # EI
                self.IME = True

            case 0xFC:  # Illegal opcode
                print("Illegal opcode: 0x%02X" % opcode)

            case 0xFD:  # Illegal opcode
                print("Illegal opcode: 0x%02X" % opcode)

            case 0xFE:  # CP d8
                self.CP_flags(m.read_byte(self.PC + 1))
                self.PC += 2

            case 0xFF:  # RST 38H
                self.SP -= 2
                m.write_word(self.SP, self.PC)
                self.PC = 0x38

            case _:
                print("Unknown opcode: 0x%02X" % opcode)

    # for AND opcodes (same flags)

    def AND_flags(self, value):
        self.set_flag("Z", value == 0)
        self.set_flag("N", False)
        self.set_flag("H", True)
        self.set_flag("C", False)

    # for OR opcodes (same flags)
    def OR_flags(self, value):
        self.set_flag("Z", value == 0)
        self.set_flag("N", False)
        self.set_flag("H", False)
        self.set_flag("C", False)

    # CP opcodes (same flags)
    def CP_flags(self, value):
        self.set_flag("Z", self.A == value)
        self.set_flag("N", True)
        self.set_flag("H", (self.A & 0xF) < (value & 0xF))
        self.set_flag("C", self.A < value)

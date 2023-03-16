class CPU: # 8bit
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
            "SP": 0
        }
        self.flags = {
            "Z": 0x80,
            "N": 0x40,
            "H": 0x20,
            "C": 0x10
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

    def execute_next_instruction(self, memory):
        opcode = memory.read_byte(self.registers["PC"])
        self.registers["PC"] += 1
        self.execute_instruction(opcode, memory)

    def execute_instruction(self, opcode, memory):
        match opcode: # Python 3.10
            case 0x00: # NOP
                pass

            case 0x01: # LD BC, nn
                self.registers["C"] = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1
                self.registers["B"] = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1

            case 0x02: # LD (BC), A
                memory.write_byte(self.registers["B"] << 8 | self.registers["C"], self.registers["A"])

            case 0x03: # INC BC
                self.registers["C"] += 1

                if self.registers["C"] > 0xFF:
                    self.registers["C"] = 0
                self.registers["B"] += 1

                if self.registers["B"] > 0xFF:
                    self.registers["B"] = 0

            case 0x04: # INC B
                self.registers["B"] = (self.registers["B"] + 1) & 0xFF # ensure result stays in 8 bit range
                self.registers["F"] = (self.registers["F"] & 0xCF) | (0x20 if (self.registers["B"] & 0x0F) == 0x00 else 0) | (0 if self.registers["B"] else 0x80)
                self.set_flag("Z", self.registers["B"] == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.registers["B"] & 0x0F) == 0x00)

            case 0x05: # DEC B
                self.registers["B"] = (self.registers["B"] - 1) & 0xFF
                self.set_flag("Z", self.registers["B"] == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.registers["B"] & 0x0F) == 0x0F)

            case 0x06: # LD B, n
                self.registers["B"] = memory.read_byte(self.registers["PC"])

            case 0x07: # RLCA
                self.registers["A"] = (self.registers["A"] << 1) | (self.registers["A"] >> 7)
                self.set_flag("Z", False)
                self.set_flag("N", False)
                self.set_flag("H", False)
                self.set_flag("C", self.registers["A"] > 0x7F) # Set C flag to bit 7 of A

            case 0x08: # LD (nn), SP
                addr = memory.read_byte(self.registers["PC"])

            case 0x09: # ADD HL, BC
                hl = (self.registers["H"] << 8) | self.registers["L"]
                bc = (self.registers["B"] << 8) | self.registers["C"]
                result = hl + bc

                self.registers["H"] = (result >> 8) & 0xFF
                self.registers["L"] = result & 0xFF

                self.set_flag("N", False)
                self.set_flag("H", (hl & 0xFFF) + (bc & 0xFFF) > 0xFFF)
                self.set_flag("C", result > 0xFFFF)

            case 0x0A: # LD A, (BC)
                self.registers["A"] = memory.read_byte(self.registers["B"] << 8 | self.registers["C"])

            case 0x0B: # DEC BC
                self.registers["B"] = (self.registers["B"] - 1) & 0xFF
                self.registers["C"] = (self.registers["C"] - 1) & 0xFF

            case 0x0C: # INC C
                self.registers["C"] = (self.registers["C"] + 1) & 0xFF
                self.set_flag("Z", self.registers["C"] == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.registers["C"] & 0x0F) == 0x00)

            case 0x0D: # DEC C
                self.registers["C"] = (self.registers["C"] - 1) & 0xFF
                self.set_flag("Z", self.registers["C"] == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.registers["C"] & 0x0F) == 0x0F)

            case 0x0E: # LD C, n
                self.registers["C"] = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1

            case 0x0F: # RRCA
                self.registers["A"] = (self.registers["A"] >> 1) | (self.registers["A"] << 7)
                self.set_flag("Z", False)
                self.set_flag("N", False)
                self.set_flag("H", False)
                self.set_flag("C", self.registers["A"] > 0x7F)

            case 0x10: # STOP
                pass  # Implement STOP behavior

            case 0x11: # LD DE, nn
                self.registers["E"] = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1
                self.registers["D"] = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1

            case 0x12: # LD (DE), A
                memory.write_byte(self.registers["D"] << 8 | self.registers["E"], self.registers["A"])

            case 0x13: # INC DE
                value = (self.registers["D"] << 8) + self.registers["E"] + 1
                self.registers["D"] = (value >> 8) & 0xFF
                self.registers["E"] = value & 0xFF

            case 0x14: # INC D
                self.registers["D"] = (self.registers["D"] + 1) & 0xFF
                self.set_flag("Z", self.registers["D"] == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.registers["D"] & 0x0F) == 0x00)

            case 0x15: # DEC D
                self.registers["D"] = (self.registers["D"] - 1) & 0xFF
                self.set_flag("Z", self.registers["D"] == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.registers["D"] & 0x0F) == 0x0F)

            case 0x16: # LD D, n
                self.registers["D"] = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1

            case 0x17: # RLA
                self.registers["A"] = (self.registers["A"] << 1) | (self.registers["A"] >> 7)
                self.set_flag("Z", False)
                self.set_flag("N", False)
                self.set_flag("H", False)
                self.set_flag("C", self.registers["A"] > 0x7F)

            case 0x18: # JR n
                offset = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1
                self.registers["PC"] += offset

            case 0x19: # ADD HL, DE
                self.registers["H"] += self.registers["D"]
                self.registers["L"] += self.registers["E"]
                self.set_flag("N", False)
                self.set_flag("H", (self.registers["L"] & 0xF) < (self.registers["E"] & 0xF))
                self.set_flag("C", (self.registers["L"] & 0xFF) < (self.registers["E"] & 0xFF))

            case 0x1A: # LD A, (DE)
                self.registers["A"] = memory.read_byte(self.registers["D"] << 8 | self.registers["E"])

            case 0x1B: # DEC DE
                value = (self.registers["D"] << 8) + self.registers["E"] - 1
                self.registers["D"] = (value >> 8) & 0xFF
                self.registers["E"] = value & 0xFF

            case 0x1C: # INC E
                self.registers["E"] = (self.registers["E"] + 1) & 0xFF
                self.set_flag("Z", self.registers["E"] == 0)
                self.set_flag("N", False)
                self.set_flag("H", (self.registers["E"] & 0x0F) == 0x00)

            case 0x1D: # DEC E
                self.registers["E"] = (self.registers["E"] - 1) & 0xFF
                self.set_flag("Z", self.registers["E"] == 0)
                self.set_flag("N", True)
                self.set_flag("H", (self.registers["E"] & 0x0F) == 0x0F)

            case 0x1E: # LD E, n
                self.registers["E"] = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1

            case 0x1F: # RRA
                carry = self.registers["A"] & 1
                self.registers["A"] = ((self.registers["A"] >> 1) | (carry << 7)) & 0xFF

            case 0x20: # JR NZ, n
                offset = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1
                if not self.registers["F"] & 0x80:
                    self.registers["PC"] += offset

            case 0x21: # LD HL, nn
                self.registers["L"] = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1
                self.registers["H"] = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1

            case 0x22: # LD (HL+), A
                memory.write_byte(self.registers["H"] << 8 | self.registers["L"], self.registers["A"])
                self.registers["L"] += 1

            case 0x23: # INC HL
                value = (self.registers["H"] << 8) + self.registers["L"] + 1
                self.registers["H"] = (value >> 8) & 0xFF
                self.registers["L"] = value & 0xFF

            case 0x24: # INC H
                self.registers["H"] += 1

            case 0x25: # DEC H
                self.registers["H"] -= 1

            case 0x26: # LD H, n
                self.registers["H"] = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1

            case 0x27: # DAA
                pass  # Implement DAA behavior

            case 0x28: # JR Z, n
                offset = memory.read_byte(self.registers["PC"])
                self.registers["PC"] += 1
                if self.registers["F"] & 0x80:
                    self.registers["PC"] += offset

            case 0x29: # ADD HL, HL
                self.registers["H"] += self.registers["H"]
                self.registers["L"] += self.registers["L"]

            case 0x2A: # LD A, (HL+)
                self.registers["A"] = memory.read_byte(self.registers["H"] << 8 | self.registers["L"])
                self.registers["L"] = (self.registers["L"] + 1) & 0xFF
                if self.registers["L"] == 0:
                    self.registers["H"] = (self.registers["H"] + 1) & 0xFF


        # else:
        #     raise Exception(f"Unknown opcode: {hex(opcode)}")

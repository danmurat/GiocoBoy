# opcode implementations for the GAMEBOY CPU

class Instructions:
    def __init__(self):
        pass

    # cpu instructions
    def INC(self, reg):
        reg = (reg + 1) & 0xFF # ensure 8 bit range

    def DEC(self, reg):
        reg = (reg - 1) & 0xFF

    def ADD(self, reg1, reg2):
        return reg1 + reg2

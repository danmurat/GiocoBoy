class Memory:
    def __init__(self):
        self.memory = [0] * 65536 # 64KB of memory

    def load_rom(self, rom_data):
        for i, byte in enumerate(rom_data):
            self.memory[i] = byte

    def read_byte(self, addr):
        if 0 <= addr < len(self.memory):
            return self.memory[addr]
        else:
            print(f"Attempted to read an invalid memory address: {hex(addr)}")
            return 0

    # essentially LD instruction
    def write_byte(self, addr, value):
        self.memory[addr] = value

    def read_word(self, addr):
        return self.read_byte(addr) | (self.read_byte(addr + 1) << 8)

    def write_word(self, addr, value):
        self.write_byte(addr, value & 0xFF)
        self.write_byte(addr + 1, (value >> 8) & 0xFF)

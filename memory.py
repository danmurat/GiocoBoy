class Memory:
    def __init__(self):
        self.memory = [0] * 65536  # 64KB memory

    def load_rom(self, rom_data):
        for i, byte in enumerate(rom_data):
            self.memory[i] = byte

    def read_byte(self, addr):
        if 0 <= addr < len(self.memory):
            return self.memory[addr]
        else:
            print(f"Attempted to read an invalid memory address: {hex(addr)}")
            return 0

    def write_byte(self, addr, value):
        self.memory[addr] = value

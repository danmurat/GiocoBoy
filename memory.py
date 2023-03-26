class Memory:
    def __init__(self):
        self.memory = [0] * 65536  # 64KB of memory
        self.rom_data = []
        self.current_bank = 1

        self.rom_bank0 = self.memory[0x0000:0x3FFF]
        self.cart_header = self.memory[0x0100:0x014F]  # info on cart
        self.rom_bank1 = self.memory[0x4000:0x7FFF]
        self.vram = self.memory[0x8000:0x9FFF]
        self.eram = self.memory[0xA000:0xBFFF]
        self.wram = self.memory[0xC000:0xDFFF]
        self.echo_ram = self.memory[0xE000:0xFDFF]
        self.oam = self.memory[0xFE00:0xFE9F]
        self.unusable = self.memory[0xFEA0:0xFEFF]
        self.io = self.memory[0xFF00:0xFF7F]
        self.hram = self.memory[0xFF80:0xFFFF]

    def load_rom(self, rom_data):
        self.rom_data = rom_data
        self.rom_bank0 = self.rom_data[0x0000:0x3FFF]
        self.rom_bank1 = self.rom_data[0x4000:0x7FFF]

    def read_byte(self, addr):
        if 0x0000 <= addr <= 0x3FFF:  # ROM Bank 0 (fixed)
            return self.rom_data[addr]
        elif 0x4000 <= addr <= 0x7FFF:  # ROM Bank 1 (switchable)
            rom_offset = (self.current_bank - 1) * 0x4000
            return self.rom_data[rom_offset + (addr - 0x4000)]
        elif 0x8000 <= addr <= 0x9FFF:  # VRAM
            return self.vram[addr - 0x8000]
        elif 0xA000 <= addr <= 0xBFFF:  # External RAM
            return self.eram[addr - 0xA000]
        elif 0xC000 <= addr <= 0xDFFF:  # Work RAM
            return self.wram[addr - 0xC000]
        elif 0xE000 <= addr <= 0xFDFF:  # Echo RAM
            return self.echo_ram[addr - 0xE000]
        elif 0xFE00 <= addr <= 0xFE9F:  # OAM
            return self.oam[addr - 0xFE00]
        elif 0xFF00 <= addr <= 0xFF7F:  # IO
            return self.io[addr - 0xFF00]
        elif 0xFF80 <= addr <= 0xFFFF:  # HRAM
            return self.hram[addr - 0xFF80]
        else:
            print(f"Attempted to read an invalid memory address: {hex(addr)}")
            return 0

    def write_byte(self, addr, value):
        if 0x0000 <= addr <= 0x1FFF:  # RAM Enable / MBC control
            pass  # handle MBC control or RAM enable here
        elif 0x2000 <= addr <= 0x3FFF:  # ROM Bank switching
            self.current_bank = value
        elif 0x4000 <= addr <= 0x5FFF:  # RAM Bank switching / MBC control
            pass  # handle MBC control or RAM bank switching here
        elif 0x6000 <= addr <= 0x7FFF:  # MBC control
            pass  # handle MBC control here
        elif 0x8000 <= addr <= 0x9FFF:  # VRAM
            self.vram[addr - 0x8000] = value
        elif 0xA000 <= addr <= 0xBFFF:  # External RAM
            self.eram[addr - 0xA000] = value
        elif 0xC000 <= addr <= 0xDFFF:  # Work RAM
            self.wram[addr - 0xC000] = value
        elif 0xE000 <= addr <= 0xFDFF:  # Echo RAM
            self.echo_ram[addr - 0xE000] = value
        elif 0xFE00 <= addr <= 0xFE9F:  # OAM
            self.oam[addr - 0xFE00] = value
        elif 0xFF00 <= addr <= 0xFF7F:  # IO
            self.io[addr - 0xFF00] = value
        elif 0xFF80 <= addr <= 0xFFFF:
            self.hram[addr - 0xFF80] = value
        else:
            print(
                f"Attempted to write an invalid memory address: {hex(addr)}")

    def read_word(self, addr):
        return self.read_byte(addr) | (self.read_byte(addr + 1) << 8)

    def write_word(self, addr, value):
        self.write_byte(addr, value & 0xFF)
        self.write_byte(addr + 1, (value >> 8) & 0xFF)

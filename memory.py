class Memory:
    def __init__(self, cpu):
        # when we need to access cpu registers (passed from main.py)
        self.cpu = cpu
        self.rom_data = []
        self.current_bank = 1

        # ROM Bank 0 (fixed) and ROM Bank 1 (switchable)

        # memory map
        self.cart_header = [0] * 0x50  # info on cart

        self.vram = [0] * 0x2000  # 8kb video ram
        self.eram = [0] * 0x2000  # 8kb external ram
        self.wram = [0] * 0x2000  # 8kb work ram

        self.echo_ram = [0] * 0x1E00  # area is prohibited
        self.oam = [0] * 0xA0  # 160b of sprite attribute table

        self.unusable = [0] * 0x60

        self.io = [0] * 0x80  # 128 bytes of io registers
        self.hram = [0] * 0x80  # 128 bytes of high ram

        self.IF = 0x00  # interrupt flag
        self.IE = 0x00  # interrupt enable

        # TIMERS
        self.DIV = 0x00
        self.TIMA = 0x00
        self.TMA = 0x00
        self.TAC = 0x00

    def load_rom(self, rom_data):
        self.rom_data = rom_data

    def print_rom_data(self):
        print("ROM Data:")
        for i in range(0, len(self.rom_data), 16):
            print(f"{hex(i)[2:].zfill(4)}: " + " ".join([hex(x)[2:].zfill(2)
                  for x in self.rom_data[i:i + 16]]))

    def print_memory_range(self, start, end):
        print(f"Memory range: {hex(start)} - {hex(end)}")
        for i in range(start, end + 1, 16):
            print(f"{hex(i)[2:].zfill(4)}: " + " ".join([hex(self.read_byte(x))
                  [2:].zfill(2) for x in range(i, i + 16)]))

    def read_byte(self, addr):
        if 0x0000 <= addr <= 0x3FFF:  # ROM Bank 0 (fixed)
            return self.rom_data[addr]
        elif 0x4000 <= addr <= 0x7FFF:  # ROM Bank 1 (switchable)
            rom_offset = self.current_bank * 0x4000
            data = self.rom_data[rom_offset + (addr - 0x4000)]
            return data
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
            if addr == 0xFF04:
                return self.DIV
            elif addr == 0xFF05:
                return self.TIMA
            elif addr == 0xFF06:
                return self.TMA
            elif addr == 0xFF07:
                return self.TAC
            elif addr == 0xFF0F:
                return self.IF
            elif addr == 0xFF44:
                return 0x90  # gameboy doctor
            return self.io[addr - 0xFF00]
        elif 0xFF80 <= addr <= 0xFFFF:  # HRAM
            if addr == 0xFFFF:  # IE
                return self.IE
            return self.hram[addr - 0xFF80]
        else:
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
            if addr == 0xFF04:
                self.DIV = 0x00
            elif addr == 0xFF05:
                self.TIMA = value
            elif addr == 0xFF06:
                self.TMA = value
            elif addr == 0xFF07:
                self.TAC = value & 0x07
                freqs = {0: 1024, 1: 16, 2: 64, 3: 256}
                self.cpu.tima_counter = freqs[self.TAC & 0x03]
            elif addr == 0xFF0F:
                self.IF = value
            else:
                self.io[addr - 0xFF00] = value
        elif 0xFF80 <= addr <= 0xFFFF:
            if addr == 0xFFFF:
                self.IE = value
            else:
                self.hram[addr - 0xFF80] = value
        else:
            pass
            # print(
            #     f"Attempted to write an invalid memory address: {hex(addr)}")

    def read_word(self, addr):
        return self.read_byte(addr) | (self.read_byte(addr + 1) << 8)

    def write_word(self, addr, value):
        self.write_byte(addr, value & 0xFF)
        self.write_byte(addr + 1, (value >> 8) & 0xFF)

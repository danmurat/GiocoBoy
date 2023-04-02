import sys
import time

from cpu import CPU
from memory import Memory

# Debug mode


class GameBoyEmulator:
    def __init__(self, rom_path):
        self.memory = Memory()
        self.cpu = CPU(self.memory)
        self.load_rom(rom_path)

    def load_rom(self, rom_path):
        with open(rom_path, 'rb') as f:
            rom_data = f.read()
        self.memory.load_rom(rom_data)
        # self.memory.print_memory_range(0x100, 0xFFFF)  # loaded values from rom

    def run(self):
        start = time.perf_counter()
        n = 0
        while n < 1270000:
            self.cpu.execute_next_instruction()
            self.cpu.handle_interrupts()
            if self.memory.read_byte(0xFF02) == 0x81:
                print(chr(self.memory.read_byte(0xFF01)), end='', flush=True)
                # reset serial transfer
                self.memory.write_byte(0xFF02, 0x00)
                # cycles = self.cpu.execute_next_instruction(self.memory)
                # self.cpu.cycles += cycles

            # if n == 16518:  # 0x0C24C is written with 9b instead of ba
            #     self.memory.print_memory_range(0xd790, 0xd820)

            # if 16484 <= n <= 16500:
            #     self.cpu.debug_mode = True
            # if 2360 <= n <= 2376:
            #     self.cpu.debug_mode = True
            # else:
            #     self.cpu.debug_mode = False
            n += 1
        # while True:
        #     self.cpu.execute_next_instruction()
        #     self.cpu.handle_interrupts()
        #     if self.memory.read_byte(0xFF02) == 0x81:
        #         print(chr(self.memory.read_byte(0xFF01)), end='', flush=True)
        #         self.memory.write_byte(0xFF02, 0x00)

        end = time.perf_counter()
        clock_speed = self.cpu.calc_clock_speed(start, end)
        print(f"Clock speed: {clock_speed:.2f} Hz")


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python gameboy_emulator.py [path_to_rom]")
        sys.exit(1)

    rom_path = sys.argv[1]
    emulator = GameBoyEmulator(rom_path)
    emulator.run()
    # cProfile.run('emulator.run()', "profiling_results.prof")

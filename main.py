import sys
import time

from cpu import CPU
from memory import Memory


class GameBoyEmulator:
    def __init__(self, rom_path):
        self.cpu = CPU()
        self.memory = Memory(self.cpu)
        self.cpu.m = self.memory
        self.load_rom(rom_path)

    def load_rom(self, rom_path):
        with open(rom_path, 'rb') as f:
            rom_data = f.read()
        self.memory.load_rom(rom_data)
        # self.memory.print_memory_range(0x100, 0xFFFF)  # loaded values from rom

    def run(self):
        start = time.perf_counter()
        n = 0
        while n < 500000:
            if not self.cpu.halted:
                self.cpu.execute_next_instruction()
                self.cpu.handle_interrupts()
                if self.cpu.set_IME_next:
                    self.cpu.IME = True
                    self.cpu.set_IME_next = False

            if self.memory.read_byte(0xFF02) == 0x81:
                print(chr(self.memory.read_byte(0xFF01)), end='', flush=True)
                self.memory.write_byte(0xFF02, 0x00)
            n += 1

        end = time.perf_counter()
        clock_speed = self.cpu.calc_clock_speed(start, end)
        print(f"Clock speed: {clock_speed:.2f} Hz")


def debug_timer(emulator):
    # Set up registers and memory as per the test ROM
    emulator.memory.write_byte(0xFF07, 0x05)  # TAC
    emulator.memory.write_byte(0xFF05, 0x00)  # TIMA
    emulator.memory.write_byte(0xFF0F, 0x00)  # IF

    cycle_delay = 500
    cycle_count = 0

    # Run the emulator for the first delay
    while cycle_count < cycle_delay:
        cycles = emulator.cpu.step()  # Execute one instruction and get the cycles it took
        emulator.cpu.update_tima(cycles)  # Update the TIMA with the cycles
        cycle_count += cycles

    IF_value = emulator.memory.read_byte(0xFF0F)  # Read the IF register
    print(f"IF register after first delay: {hex(IF_value)}")

    # Run the emulator for the second delay
    cycle_count = 0
    while cycle_count < cycle_delay:
        cycles = emulator.cpu.step()
        emulator.cpu.update_tima(cycles)
        cycle_count += cycles

    IF_value = emulator.memory.read_byte(0xFF0F)
    print(f"IF register after second delay: {hex(IF_value)}")

    # Run the emulator for the third delay
    cycle_count = 0
    while cycle_count < cycle_delay:
        cycles = emulator.cpu.step()
        emulator.cpu.update_tima(cycles)
        cycle_count += cycles

    IF_value = emulator.memory.read_byte(0xFF0F)
    print(f"IF register after third delay: {hex(IF_value)}")


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python gameboy_emulator.py [path_to_rom]")
        sys.exit(1)

    rom_path = sys.argv[1]
    emulator = GameBoyEmulator(rom_path)
    # debug_timer(emulator)
    emulator.run()
    # cProfile.run('emulator.run()', "profiling_results.prof")

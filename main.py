import sys

from cpu import CPU
from memory import Memory

# The code above sets up a basic emulator structure with a CPU and memory class.
# The next step would be to implement the opcode execution logic in the execute_instruction
# method of the CPU class. There are around 500 opcodes for the Game Boy's CPU
# (the Sharp LR35902), which makes the task quite extensive.


class GameBoyEmulator:
    def __init__(self, rom_path):
        self.cpu = CPU()
        self.memory = Memory()
        self.load_rom(rom_path)

    def load_rom(self, rom_path):
        with open(rom_path, 'rb') as f:
            rom_data = f.read()
        self.memory.load_rom(rom_data)

    def run(self):
        while True:
            self.cpu.execute_next_instruction(self.memory)
            # cycles = self.cpu.execute_next_instruction(self.memory)
            # self.cpu.cycles += cycles


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python gameboy_emulator.py [path_to_rom]")
        sys.exit(1)

    rom_path = sys.argv[1]
    emulator = GameBoyEmulator(rom_path)
    emulator.run()

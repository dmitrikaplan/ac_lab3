from klvm.Exception import ISACommandNotFound
from klvm.ISA import ISA
from klvm.Memory import memory


def get_memory_size() -> int:
    return len(memory)


class DataPath:
    class ALU:
        @staticmethod
        def compute(cpu, action, arguments: list[int], registers: list[int]):
            action(cpu, arguments, registers)

    # 0 - REG1, 1 - REG2, 2 - REG3, 3 - REG4, 4 - IP, 5 - SP, 6 - PEP, 7 - REG5
    registers = [0, 0, 0, 0, 0, get_memory_size() - 1, 0, 0]
    alu = ALU()


class ControlUnit:

    def __init__(self, data_path: DataPath):
        self.data_path = data_path

    def run(self):
        while is_runnable:
            isa = self.decoder(read_in_memory(self.get_ip()))
            arguments: list[int] = []
            for i in range(1, isa.number_of_arguments + 1):
                self.next_ip()
                arguments.append(read_in_memory(self.get_ip()))

            self.next_ip()
            data_path.alu.compute(self, action=isa.action, arguments=arguments, registers=data_path.registers)

    def next_ip(self):
        self.data_path.registers[4] += 1

    def get_ip(self) -> int:
        return self.data_path.registers[4]

    @staticmethod
    def decoder(data: int) -> ISA:
        for command in list(ISA):
            if command.get_code() == data:
                return command
        raise ISACommandNotFound()


is_runnable: bool = True
data_path = DataPath()
control_unit = ControlUnit(data_path)


def write_to_memory(index: int, value: int):
    memory[index] = value


def read_in_memory(index: int) -> int:
    return memory[index]


def init_memory(l: list[int]):
    for index in range(0, len(l)):
        memory[index] = l[index]

    data_path.registers[6] = len(l)


def start(mem: list[int]):
    init_memory(mem)
    control_unit.run()

from klvm import CPU
from klvm.Converter import Type

external_input_device_1 = 0x10000
external_input_device_2 = 0x10001
external_input_device_3 = 0x10002
external_output_device_1 = 0x10003
external_output_device_2 = 0x10004


input_file = ""


class Console:
    queue: list[int] = []
    index = 0

    @staticmethod
    def print():
        data = CPU.read_in_memory(external_output_device_1)
        index_of_type = CPU.read_in_memory(external_output_device_2)
        mapper = Type.get_mapper_by_index(index_of_type)
        type_ = Type.get_type_by_index(index_of_type)

        if type_ == Type.NUMBER or type_ == Type.BOOLEAN:
            print(mapper(data))
        else:
            line = mapper(data)
            i = 0
            while i < len(line):
                if line[i] == "\\" and i + 1 < len(line) and line[i + 1] == "n":
                    print()
                    i += 1
                else:
                    print(line[i], end="")
                i += 1

    def read(self):
        with open(input_file, encoding="utf-8") as file:
            line = "".join(file.readlines())
            print(line)
            type_ = Type.define_type(line)
            self.queue = type_.reverse_mapper(line)
            CPU.write_to_memory(external_input_device_1, type_.index)

    def has_next(self) -> bool:
        return self.index < len(self.queue)

    def next(self):
        CPU.write_to_memory(external_input_device_1, self.queue[self.index])
        self.index += 1

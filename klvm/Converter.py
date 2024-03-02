import enum

from Exception import RuntimeException


def to_sign_int(unsigned_int: int) -> int:
    binary_number = int_to_binary(unsigned_int, 64)
    int_number = int(binary_number, 2)

    if binary_number[0] == '1':
        int_number = int_number - (1 << len(binary_number))

    return int_number


def to_unsigned_int(sign_int: int) -> int:
    return sign_int if sign_int >= 0 else sign_int & 0xffffffffffffffff


def boolean_to_binary(value: str) -> int:
    return 1 if value == 'true' else 0


def int_to_string(value: int) -> str:
    binary = int_to_binary(value)
    start = 0
    finish = 8
    string = []
    while finish <= len(binary):
        byte = int(binary[start:finish], 2)

        if byte == 0:
            return "".join(string)

        string.append(chr(byte))

        start += 8
        finish += 8

    return "".join(string)


def int_to_binary(value: int, size: int = 64) -> str:
    mask = (1 << size) - 1
    if value < 0:
        value = ((abs(value) ^ mask) + 1)
        return bin(value & mask)[2:]

    binary_representation = bin(value & mask)[2:]
    return (size - len(binary_representation)) * '0' + bin(value & mask)[2:]


def string_to_int(value: str) -> list[int]:
    result = []
    arr = bytearray(value, 'utf-8')
    start = 0
    finish = 8
    flag = True
    while flag:
        result.append(bytes_to_int(arr[start: finish]))
        if finish > len(arr):
            flag = False
        start += 8
        finish += 8

    return result


def bytes_to_int(bytes_) -> int:
    result = 0
    i = 56
    for byte in bytes_:
        result += byte * 2 ** i
        i -= 8

    return result


class Type(enum.Enum):
    NUMBER = {
        'index': 0,
        'mapper': lambda value: to_sign_int(value),
        'reverse-mapper': lambda value: [to_unsigned_int(int(value))] if is_number(value) else None
    },
    BOOLEAN = {
        'index': 1,
        'mapper': lambda value: 'false' if value == 0 else 'true',
        'reverse-mapper': lambda value: [boolean_to_binary(value)] if is_boolean(value) else None
    }
    STRING = {
        'index': 2,
        'mapper': lambda value: int_to_string(value),
        'reverse-mapper': lambda value: string_to_int(value) if is_string(value) else None
    }
    UNIT = {
        'index': 2,
        'mapper': lambda value: int_to_string(value),
        'reverse-mapper': lambda value: None
    }

    def __init__(self, vals):
        self.index = vals['index']
        self.mapper = vals['mapper']
        self.reverse_mapper = vals['reverse-mapper']

    def get_index(self):
        return self.index

    def get_mapper(self):
        return self.mapper

    def get_reverse_mapper(self):
        return self.reverse_mapper

    @staticmethod
    def get_mapper_by_index(index_of_type: int):
        for type in list(Type):
            if type.get_index() == index_of_type:
                return type.get_mapper()

    @staticmethod
    def get_type_by_index(index_of_type: int):
        for type in list(Type):
            if type.get_index() == index_of_type:
                return type

    @staticmethod
    def define_type(value: str):
        if is_number(value):
            return Type.NUMBER

        if is_boolean(value):
            return Type.BOOLEAN

        if is_string(value):
            return Type.STRING
        else:
            raise RuntimeException('Невозможно определить тип переменной!')


def is_number(text: str) -> bool:
    start_index = 1 if (text[0] == '-') else 0
    for i in range(start_index, len(text)):
        if not text[i].isdigit():
            return False
    return True


def is_string(text: str) -> bool:

    symbols = ['?', '!', '>', '<', ',', '.', '\\', ' ']

    for index in range(1, len(text) - 1):
        if not text[index].isdigit() and not text[index].isalpha() and text[index] not in symbols:
            return False

    return True


def is_boolean(text: str) -> bool:
    return text == 'true' or text == 'false'


def exist_null_terminator(value: int) -> bool:
    bin = int_to_binary(value)
    start = 0
    finish = 8
    for i in range(0, 8):
        if bin[start:finish] == '0' * 8:
            return True
        start += 8
        finish += 8

    return False
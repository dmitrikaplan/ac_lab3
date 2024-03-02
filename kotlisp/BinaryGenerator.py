import re

from typeguard import typechecked

from kotlisp.Exceptions import ParsingException
from kotlisp.Utlis import is_index, is_name, define_type, random_string, to_typed_list
from kotlisp.VM import ISA, Register
from kotlisp.Variable import Type, Variable

bits = 64

binary = []


def generate_load_command(value: str, register: Register):
    if is_index(value):
        generate_load_command_for_index(value, register)
        return

    if is_name(value):
        generate_load_constant_command(Register.REG3, value)
        generate_load_command_for_registers(register, Register.REG3)
        return
    else:
        generate_load_command_for_constant(value, register)
        return


@typechecked
def generate_load_command_for_index(value: str, register: Register):
    l = re.split("[\[\]]", value)
    list_name = l[0]
    index_name = l[1]
    generate_load_constant_command(Register.REG4, list_name)
    generate_load_command(index_name, Register.REG5)
    generate_binary_command(ISA.ADD, Register.REG4, Register.REG5)
    generate_load_command_for_registers(register, Register.REG4)


@typechecked
def generate_load_constant_command(register: Register, address: str):
    binary.append(isa_to_binary(ISA.LDC))
    binary.append(register_to_binary(register))
    binary.append(address)


@typechecked
def generate_load_command_for_registers(register1: Register, register2: Register):
    generate_binary_command(ISA.LD, register1, register2)


@typechecked
def generate_default_binary_command(command: ISA):
    binary.append(isa_to_binary(command))


@typechecked
def generate_binary_command(command: ISA, *registers: Register):
    binary.append(isa_to_binary(command))
    for register in registers:
        binary.append(register_to_binary(register))


def generate_load_command_for_constant(value: str, register: Register):
    from kotlisp.KotlispTransformer import get_variables

    type_: Type = define_type(value)
    variables = get_variables()

    if type_ == Type.STRING:
        name = random_string()
        variables[name] = Variable(value, type_)
        return generate_load_command(name, register)

    if type_ == Type.BOOLEAN:
        return generate_load_constant_command(
            register=register, address=(int_to_binary(1) if (value == "true") else int_to_binary(0))
        )

    if type_ == Type.NUMBER:
        return generate_load_constant_command(register, int_to_binary(int(value)))

    else:
        raise ParsingException("Ошибка парсинга!")


def generate_binary_variable(variable: Variable):
    if variable.type == Type.NUMBER:
        binary.append(int_to_binary(int(variable.value)))

    if variable.type == Type.BOOLEAN:
        binary.append(boolean_to_binary(bool(variable.value)))

    if variable.type == Type.STRING:
        strings: list[str] = string_to_binary(variable.value)
        for string in strings:
            binary.append(string)

    if variable.type == Type.UNIT:
        binary.append(string_to_binary("Unit")[0])

    if variable.type == Type.LIST:
        list_, type_ = to_typed_list(variable.value)

        for elem in list_:
            if type_ == Type.STRING:
                replaced_elem = elem.replace('"', "")
                return generate_binary_variable(Variable(replaced_elem, Type.STRING))

            else:
                generate_binary_variable(Variable(elem, type_))


@typechecked
def int_to_binary(value: int, size: int = bits) -> str:
    mask = (1 << size) - 1
    if value < 0:
        value = (abs(value) ^ mask) + 1
        return bin(value & mask)[2:]

    binary_representation = bin(value & mask)[2:]
    return (size - len(binary_representation)) * "0" + bin(value & mask)[2:]


@typechecked
def isa_to_binary(value: ISA, size: int = bits) -> str:
    return int_to_binary(value.get_code(), size)


@typechecked
def register_to_binary(register: Register, size: int = bits) -> str:
    return int_to_binary(register.index_of_register, size)


def boolean_to_binary(value: bool, size: int = bits) -> str:
    return int_to_binary(value=1 if (bool(value)) else 0, size=size)


def string_to_binary(value: str) -> list[str]:
    list_of_symbols: list[str] = []
    result: list[str] = []
    for x in bytearray(value, "utf-8"):
        list_of_symbols.append(int_to_binary(x, 8))

    index = 0
    builder = ""

    while index < len(list_of_symbols) - (len(list_of_symbols) % 8):
        builder += list_of_symbols[index]
        index += 1

        if index % 8 == 0:
            result.append(builder)
            builder = ""

    while index < len(list_of_symbols):
        builder += list_of_symbols[index]
        index += 1

    builder += "0" * 8 * (8 - index % 8)
    result.append(builder)

    return result


def get_bin() -> list[str]:
    return binary

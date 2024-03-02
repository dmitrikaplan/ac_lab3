import enum
import logging

from klvm import CPU, IO
from klvm.Converter import to_sign_int, to_unsigned_int, exist_null_terminator, Type
from klvm.Exception import ArgumentsSizeException, ArithmeticException, RuntimeException


class ISA(enum.Enum):
    ADD = {
        'code': 0xe000000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: add(arguments, registers)
    },
    SUB = {
        'code': 0xe10000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: sub(arguments, registers)
    },
    MUL = {
        'code': 0xe200000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: mul(arguments, registers)
    },
    IDIV = {
        'code': 0xe300000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: idiv(arguments, registers)
    },  # знаковое деление
    AND = {
        'code': 0xe400000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: and_command(arguments, registers)
    },
    OR = {
        'code': 0xe500000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: or_command(arguments, registers)
    },
    XOR = {
        'code': 0xe600000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: xor(arguments, registers)
    },
    INC = {
        'code': 0xe700000000000000,
        'number_of_arguments': 1,
        'action': lambda cpu, arguments, registers: inc(arguments, registers)
    },
    NEG = {
        'code': 0xe800000000000000,
        'number_of_arguments': 1,
        'action': lambda cpu, arguments, registers: neg(arguments, registers)
    },
    EQ = {
        'code': 0xe900000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: eq(arguments, registers)
    },  # ==
    NEQ = {
        'code': 0xea00000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: neq(arguments, registers)
    }  # !=
    GR = {
        'code': 0xeb00000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: gr(arguments, registers)
    },  # >
    GREQ = {
        'code': 0xec00000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: greq(arguments, registers)
    },  # >=
    LS = {
        'code': 0xed00000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: ls(arguments, registers)
    },  # <
    LSEQ = {
        'code': 0xed00000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: lseq(arguments, registers)
    },  # <=
    PUSH = {
        'code': 0xee00000000000000,
        'number_of_arguments': 1,
        'action': lambda cpu, arguments, registers: push(arguments, registers)
    },  # PUSH a, кладет на стек значение a
    POP = {
        'code': 0xef00000000000000,
        'number_of_arguments': 0,
        'action': lambda cpu, arguments, registers: pop(arguments, registers)
    },  # POP a снимает со стека значение и кладет в a
    RET = {
        'code': 0xf000000000000000,
        'number_of_arguments': 0,
        'action': lambda cpu, arguments, registers: ret(arguments, registers)
    },  # берет адрес возврата со стека и стирает его
    HLT = {
        'code': 0xf100000000000000,
        'number_of_arguments': 0,
        'action': lambda cpu, arguments, registers: hlt(cpu, arguments, registers)
    },  # заканчивает выполнение программы
    SV = {
        'code': 0xf200000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: sv(arguments, registers)
    },  # SV a, b сохранает по адресу a значение b
    LDC = {
        'code': 0xf300000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: ldc(arguments, registers)
    },  # LDC a, b загружает в a константу b
    LD = {
        'code': 0xf400000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: ld(arguments, registers)
    },  # загрузка из памяти, LD a, b загружает в a значение по адресу b
    MOV = {
        'code': 0xf500000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: mov(arguments, registers)
    },  # MOV a, b переносит значение из a в b
    CALL = {
        'code': 0xf600000000000000,
        'number_of_arguments': 1,
        'action': lambda cpu, arguments, registers: call(cpu, arguments, registers)
    },  # CALL a, вызывает функцию по адресу a
    BRMN = {
        'code': 0xf700000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: brmn(arguments, registers)
    },  # переход если больше 0, BRMN a, b, если a <= 0 переходит на  адрес  b
    JMP = {
        'code': 0xf800000000000000,
        'number_of_arguments': 1,
        'action': lambda cpu, arguments, registers: jmp(arguments, registers)
    },  # безусловный JUMP a, переходит на  адрес  a
    BRPL = {
        'code': 0xf900000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: brpl(arguments, registers)
    },  # переходеслибольше0, BRPLa, b, еслиa > 0 переходит на адрес b
    MOD = {
        'code': 0xfa00000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: mod(arguments, registers)
    },  # выводит остаток от деления, MOD a, b -> записывает в a остаток от деления a на b
    PRT = {
        'code': 0xfb00000000000000,
        'number_of_arguments': 2,
        'action': lambda cpu, arguments, registers: prt(arguments, registers)
    },  # вывод в на внешнее устройство данные
    RDL = {
        'code': 0xfc00000000000000,
        'number_of_arguments': 1,
        'action': lambda cpu, arguments, registers: rdl(arguments, registers)
    }

    def __init__(self, vals):
        self.code = vals['code']
        self.number_of_arguments = vals['number_of_arguments']
        self.action = vals['action']

    def get_code(self):
        return self.code

    def get_number_of_arguments(self):
        return self.number_of_arguments

    def get_action(self):
        return self.action

    def exists_number_of_arguments(self, number_of_arguments: str):
        return number_of_arguments in self.number_of_arguments


def add(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.ADD.number_of_arguments:
        raise ArgumentsSizeException('add')

    arg1 = to_sign_int(registers[arguments[0]])
    arg2 = to_sign_int(registers[arguments[1]])
    result = to_unsigned_int(arg1 + arg2)

    logging.debug(f'{registers[4] - ISA.ADD.number_of_arguments} {ISA.ADD.name} ->'
                  f'{get_register_name_by_index(arguments[0])}  + {get_register_name_by_index(arguments[1])}')

    if result > 0xffffffffffffffff:
        raise ArithmeticException()

    registers[arguments[0]] = result


def sub(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.SUB.number_of_arguments:
        raise ArgumentsSizeException('sub')

    arg1 = to_sign_int(registers[arguments[0]])
    arg2 = to_sign_int(registers[arguments[1]])
    result = to_unsigned_int(arg1 - arg2)

    logging.debug(f'{registers[4] - ISA.SUB.number_of_arguments} {ISA.SUB.name} -> '
                  f'{get_register_name_by_index(arguments[0])} - {get_register_name_by_index(arguments[1])}')

    if result > 0xffffffffffffffff:
        raise ArithmeticException()

    registers[arguments[0]] = result


def mul(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.MUL.number_of_arguments:
        raise ArgumentsSizeException('mul')

    logging.debug(f'{registers[4] - ISA.MUL.number_of_arguments } {ISA.MUL.name} -> '
                  f'{get_register_name_by_index(arguments[0])} * {get_register_name_by_index(arguments[1])}')

    arg1 = to_sign_int(registers[arguments[0]])
    arg2 = to_sign_int(registers[arguments[1]])
    result = to_unsigned_int(arg1 * arg2)

    if result > 0xffffffffffffffff:
        raise ArithmeticException()

    registers[arguments[0]] = result


def idiv(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.IDIV.number_of_arguments:
        raise ArgumentsSizeException('idiv')

    arg1 = to_sign_int(registers[arguments[0]])
    arg2 = to_sign_int(registers[arguments[1]])
    result = to_unsigned_int(int(arg1 / arg2))

    logging.debug(f'{registers[4] - ISA.IDIV.number_of_arguments} {ISA.IDIV.name} -> '
                  f'{get_register_name_by_index(arguments[0])}  / {get_register_name_by_index(arguments[1])}')

    if result > 0xffffffffffffffff:
        raise ArithmeticException()

    registers[arguments[0]] = result


def and_command(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.AND.number_of_arguments:
        raise ArgumentsSizeException('and')

    logging.debug(f'{registers[4] - ISA.AND.number_of_arguments} {ISA.AND.name} -> '
                  f'{get_register_name_by_index(arguments[0])}  & {get_register_name_by_index(arguments[1])}')

    registers[arguments[0]] = registers[arguments[0]] and registers[arguments[1]]


def or_command(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.OR.number_of_arguments:
        raise ArgumentsSizeException('or')

    logging.debug(f'{registers[4] - ISA.OR.number_of_arguments} {ISA.OR.name} -> '
                  f'{get_register_name_by_index(arguments[0])}  | {get_register_name_by_index(arguments[1])}')

    registers[arguments[0]] = registers[arguments[0]] or registers[arguments[1]]


def xor(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.XOR.number_of_arguments:
        raise ArgumentsSizeException('xor')

    logging.debug(f'{registers[4] - ISA.XOR.number_of_arguments} {ISA.XOR.name} -> '
                  f'{get_register_name_by_index(arguments[0])}  ^ {get_register_name_by_index(arguments[1])}')

    registers[arguments[0]] = registers[arguments[0]] ^ registers[arguments[1]]


def inc(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.INC.number_of_arguments:
        raise ArgumentsSizeException('inc')

    logging.debug(f'{registers[4] - ISA.INC.number_of_arguments} {ISA.INC.name} -> '
                  f'{get_register_name_by_index(arguments[0])}  + 1')

    arg = to_sign_int(registers[arguments[0]])
    result = to_unsigned_int(arg + 1)

    if result > 0xffffffffffffffff:
        raise ArithmeticException()

    registers[arguments[0]] = result


def neg(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.NEG.number_of_arguments:
        raise ArgumentsSizeException('neg')

    logging.debug(f'{registers[4] - ISA.NEG.number_of_arguments} {ISA.NEG.name} -> '
                  f' -{get_register_name_by_index(arguments[0])}')

    arg = to_sign_int(registers[arguments[0]])
    result = to_unsigned_int(-arg)

    if result > 0xffffffffffffffff:
        raise ArithmeticException()

    registers[arguments[0]] = result


def eq(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.EQ.number_of_arguments:
        raise ArgumentsSizeException('eq')

    logging.debug(f'{registers[4] - ISA.EQ.number_of_arguments} {ISA.EQ.name} -> '
                  f'{get_register_name_by_index(arguments[0])} = {get_register_name_by_index(arguments[1])}')

    registers[arguments[0]] = 1 if registers[arguments[0]] == registers[arguments[1]] else 0


def neq(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.NEQ.number_of_arguments:
        raise ArgumentsSizeException('neq')

    registers[arguments[0]] = 1 if registers[arguments[0]] != registers[arguments[1]] else 0

    logging.debug(f'{registers[4] - ISA.NEQ.number_of_arguments} {ISA.NEQ.name} -> '
                  f'{get_register_name_by_index(arguments[0])} != {get_register_name_by_index(arguments[1])}')


def gr(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.GR.number_of_arguments:
        raise ArgumentsSizeException('gr')

    arg1 = to_sign_int(registers[arguments[0]])
    arg2 = to_sign_int(registers[arguments[1]])

    logging.debug(f'{registers[4] - ISA.GR.number_of_arguments} {ISA.GR.name} -> '
                  f'{get_register_name_by_index(arguments[0])} > {get_register_name_by_index(arguments[1])}')

    registers[arguments[0]] = 1 if arg1 > arg2 else 0


def greq(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.GREQ.number_of_arguments:
        raise ArgumentsSizeException('greq')

    arg1 = to_sign_int(registers[arguments[0]])
    arg2 = to_sign_int(registers[arguments[1]])

    logging.debug(f'{registers[4] - ISA.GREQ.number_of_arguments} {ISA.GREQ.name} -> '
                  f'{get_register_name_by_index(arguments[0])} >= {get_register_name_by_index(arguments[1])}')

    registers[arguments[0]] = 1 if arg1 >= arg2 else 0


def ls(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.LS.number_of_arguments:
        raise ArgumentsSizeException('ls')

    arg1 = to_sign_int(registers[arguments[0]])
    arg2 = to_sign_int(registers[arguments[1]])

    logging.debug(f'{registers[4] - ISA.LS.number_of_arguments} {ISA.LS.name} -> '
                  f'{get_register_name_by_index(arguments[0])}  < {get_register_name_by_index(arguments[1])}')

    registers[arguments[0]] = 1 if arg1 < arg2 else 0


def lseq(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.LSEQ.number_of_arguments:
        raise ArgumentsSizeException('lseq')

    arg1 = to_sign_int(registers[arguments[0]])
    arg2 = to_sign_int(registers[arguments[1]])

    registers[arguments[0]] = 1 if arg1 <= arg2 else 0

    logging.debug(f'{registers[4] - ISA.LSEQ.number_of_arguments} {ISA.LSEQ.name} -> '
                  f'{get_register_name_by_index(arguments[0])} <= {get_register_name_by_index(arguments[1])}')


def push(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.PUSH.number_of_arguments:
        raise ArgumentsSizeException('push')

    CPU.write_to_memory(registers[5], registers[arguments[0]])
    registers[5] -= 1

    logging.debug(f'{registers[4] - ISA.PUSH.number_of_arguments} {ISA.PUSH.name} -> '
                  f'{get_register_name_by_index(arguments[0])}')


def pop(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.POP.number_of_arguments:
        raise ArgumentsSizeException('pop')

    registers[5] += 1

    logging.debug(f'{registers[4] - ISA.POP.number_of_arguments} {ISA.POP.name}')



def ret(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.RET.number_of_arguments:
        raise ArgumentsSizeException('ret')

    registers[5] += 1
    registers[4] = CPU.read_in_memory(registers[5] + 1)

    logging.debug(f'{registers[4] - ISA.RET.number_of_arguments} {ISA.RET.name}')


def hlt(cpu, arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.HLT.number_of_arguments:
        raise ArgumentsSizeException('hlt')

    CPU.is_runnable = False

    logging.debug(f'{registers[4] - ISA.HLT.number_of_arguments} {ISA.HLT.name}')


def sv(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.SV.number_of_arguments:
        raise ArgumentsSizeException('sv')

    CPU.write_to_memory(registers[arguments[0]], registers[arguments[1]])
    logging.debug(f'{registers[4] - ISA.SV.number_of_arguments} {ISA.SV.name} -> '
                  f'{get_register_name_by_index(arguments[0])} {get_register_name_by_index(arguments[1])}')


def ldc(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.LDC.number_of_arguments:
        raise ArgumentsSizeException('ldc')

    registers[arguments[0]] = arguments[1]
    logging.debug(f'{registers[4] - ISA.LDC.number_of_arguments} {ISA.LDC.name} -> '
                  f'{get_register_name_by_index(arguments[0])} {arguments[1]}')


def ld(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.LD.number_of_arguments:
        raise ArgumentsSizeException('ld')

    registers[arguments[0]] = CPU.read_in_memory(registers[arguments[1]])
    logging.debug(f'{registers[4] - ISA.LD.number_of_arguments} {ISA.LD.name} -> '
                  f'{get_register_name_by_index(arguments[0])} {get_register_name_by_index(arguments[1])}')


def mov(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.MOV.number_of_arguments:
        raise ArgumentsSizeException('mov')

    registers[arguments[0]] = registers[arguments[1]]

    logging.debug(f'{registers[4] - ISA.MOV.number_of_arguments} {ISA.MOV.name} -> '
                  f'{get_register_name_by_index(arguments[0])} {get_register_name_by_index(arguments[1])}')


def call(cpu, arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.CALL.number_of_arguments:
        raise ArgumentsSizeException('call')

    ISA.PUSH.action(cpu, [4], registers)
    registers[4] = registers[arguments[0]]

    logging.debug(f'{registers[4] - ISA.CALL.number_of_arguments} {ISA.CALL.name} -> '
                  f'{get_register_name_by_index(arguments[0])}')


def brmn(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.BRMN.number_of_arguments:
        raise ArgumentsSizeException('brmn')

    arg = to_sign_int(registers[arguments[0]])

    logging.debug(f'{registers[4] - ISA.BRMN.number_of_arguments} {ISA.BRMN.name} -> '
                  f'{get_register_name_by_index(arguments[0])} {get_register_name_by_index(arguments[1])}')

    if arg <= 0:
        registers[4] = registers[arguments[1]]


def jmp(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.JMP.number_of_arguments:
        raise ArgumentsSizeException('jmp')

    logging.debug(f'{registers[4] - ISA.JMP.number_of_arguments} {ISA.JMP.name} -> '
                  f'{get_register_name_by_index(arguments[0])}')

    registers[4] = registers[arguments[0]]


def brpl(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.BRPL.number_of_arguments:
        raise ArgumentsSizeException('brpl')

    arg = to_sign_int(registers[arguments[0]])

    if arg > 0:
        registers[4] = registers[arguments[1]]

        logging.debug(f'{registers[4] - ISA.BRPL.number_of_arguments} {ISA.BRPL.name} -> '
                      f'{get_register_name_by_index(arguments[0])}')


def mod(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.MOD.number_of_arguments:
        raise ArgumentsSizeException('mod')

    arg1 = to_sign_int(registers[arguments[0]])
    arg2 = to_sign_int(registers[arguments[1]])
    result = to_unsigned_int(arg1 % arg2)

    registers[arguments[0]] = result

    logging.debug(f'{registers[4] - ISA.MOD.number_of_arguments} {ISA.MOD.name} -> '
                  f'{get_register_name_by_index(arguments[0])} % {get_register_name_by_index(arguments[1])}')




def prt(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.PRT.number_of_arguments:
        raise ArgumentsSizeException('prt')

    index_of_type = registers[arguments[0]]

    logging.debug(f'{registers[4] - ISA.PRT.number_of_arguments} {ISA.PRT.name} -> '
                  f'{get_register_name_by_index(arguments[0])} {get_register_name_by_index(arguments[1])}')

    if index_of_type == Type.STRING.index:

        start_address = registers[arguments[1]]
        line = CPU.read_in_memory(start_address)

        CPU.write_to_memory(IO.external_output_device_1, line)
        CPU.write_to_memory(IO.external_output_device_2, index_of_type)

        IO.Console.print()

        while not exist_null_terminator(line):
            start_address += 1
            line = CPU.read_in_memory(start_address)

            CPU.write_to_memory(IO.external_output_device_1, line)
            CPU.write_to_memory(IO.external_output_device_2, index_of_type)

            IO.Console.print()

    else:
        data = registers[arguments[1]]
        CPU.write_to_memory(IO.external_output_device_1, data)
        CPU.write_to_memory(IO.external_output_device_2, index_of_type)
        IO.Console.print()


def rdl(arguments: list[int], registers: list[int]):
    if len(arguments) != ISA.RDL.number_of_arguments:
        raise ArgumentsSizeException('rdl')

    logging.debug(f'{registers[4] - ISA.RDL.number_of_arguments} {ISA.RDL.name} -> '
                  f'{arguments[0]} {get_register_name_by_index(arguments[1])}')

    console = IO.Console()
    console.read()
    type_index = CPU.read_in_memory(IO.external_input_device_1)

    if type_index != registers[arguments[0]]:
        raise RuntimeException('Неожидаемый тип!')

    if type_index != Type.STRING.index:
        console.next()
        data = CPU.read_in_memory(IO.external_input_device_1)
        ldc([0, data], registers)  # в reg1 пишем значение числовой или булеан переменной
    else:
        string_address = registers[6]
        while console.has_next():
            console.next()
            data = CPU.read_in_memory(IO.external_input_device_1)
            CPU.write_to_memory(registers[6], data)
            registers[6] += 1

        ldc([0, string_address], registers)  # в reg1 кладем адрес строки


def get_register_name_by_index(index) -> str:
    if index == 0:
        return 'REG1'

    if index == 1:
        return 'REG2'

    if index == 2:
        return 'REG3'

    if index == 3:
        return 'REG4'

    if index == 4:
        return 'IP'

    if index == 5:
        return 'SP'

    if index == 6:
        return 'PEP'

    if index == 7:
        return 'REG5'
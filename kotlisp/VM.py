import enum

from kotlisp.Variable import Type

external_device = 0x10000


class ISA(enum.Enum):
    ADD = (
        {
            "code": 0xE000000000000000,
            "symbol": "+",
            "action": lambda a, b: a.type == Type.NUMBER and b.type == Type.NUMBER,
        },
    )
    SUB = (
        {
            "code": 0xE10000000000000,
            "symbol": "-",
            "action": lambda a, b: a.type == Type.NUMBER and b.type == Type.NUMBER,
        },
    )
    MUL = (
        {
            "code": 0xE200000000000000,
            "symbol": "*",
            "action": lambda a, b: a.type == Type.NUMBER and b.type == Type.NUMBER,
        },
    )
    IDIV = (
        {
            "code": 0xE300000000000000,
            "symbol": "/",
            "action": lambda a, b: a.type == Type.NUMBER and b.type == Type.NUMBER,
        },
    )  # знаковое деление
    AND = (
        {
            "code": 0xE400000000000000,
            "symbol": "&",
            "action": lambda a, b: (a.type == Type.NUMBER and b.type == Type.NUMBER)
            or (a.type == Type.BOOLEAN and b.type == Type.BOOLEAN),
        },
    )
    OR = (
        {
            "code": 0xE500000000000000,
            "symbol": "|",
            "action": lambda a, b: (a.type == Type.NUMBER and b.type == Type.NUMBER)
            or (a.type == Type.BOOLEAN and b.type == Type.BOOLEAN),
        },
    )
    XOR = (
        {
            "code": 0xE600000000000000,
            "symbol": "^",
            "action": lambda a, b: (a.type == Type.NUMBER and b.type == Type.NUMBER)
            or (a.type == Type.BOOLEAN and b.type == Type.BOOLEAN),
        },
    )
    INC = ({"code": 0xE700000000000000, "symbol": "", "action": lambda a, b: False},)
    NEG = ({"code": 0xE800000000000000, "symbol": "", "action": lambda a, b: False},)
    EQ = (
        {
            "code": 0xE900000000000000,
            "symbol": "=",
            "action": lambda a, b: (a.type == Type.NUMBER or a.type == Type.BOOLEAN or a.type == Type.LIST)
            and (a.type == b.type),
        },
    )  # ==
    NEQ = {"code": 0xEA00000000000000, "symbol": "!=", "action": lambda a, b: a.type == b.type}  # !=
    GR = (
        {
            "code": 0xEB00000000000000,
            "symbol": ">",
            "action": lambda a, b: a.type == Type.NUMBER and b.type == Type.NUMBER,
        },
    )  # >
    GREQ = (
        {
            "code": 0xEC00000000000000,
            "symbol": ">=",
            "action": lambda a, b: a.type == Type.NUMBER and b.type == Type.NUMBER,
        },
    )  # >=
    LS = (
        {
            "code": 0xED00000000000000,
            "symbol": "<",
            "action": lambda a, b: a.type == Type.NUMBER and b.type == Type.NUMBER,
        },
    )  # <
    LSEQ = (
        {
            "code": 0xED00000000000000,
            "symbol": "<=",
            "action": lambda a, b: a.type == Type.NUMBER and b.type == Type.NUMBER,
        },
    )  # <=
    PUSH = (
        {"code": 0xEE00000000000000, "symbol": "", "action": lambda a, b: False},
    )  # PUSH a, кладет на стек значение a
    POP = (
        {"code": 0xEF00000000000000, "symbol": "", "action": lambda a, b: False},
    )  # POP a снимает со стека значение и кладет в a
    RET = (
        {"code": 0xF000000000000000, "symbol": "", "action": lambda a, b: False},
    )  # берет адрес возврата со стека и стирает его
    HLT = (
        {"code": 0xF100000000000000, "symbol": "", "action": lambda a, b: False},
    )  # заканчивает выполнение программы
    SV = (
        {"code": 0xF200000000000000, "symbol": "", "action": lambda a, b: False},
    )  # SV a, b сохранает по адресу a значение b
    LDC = (
        {"code": 0xF300000000000000, "symbol": "", "action": lambda a, b: False},
    )  # LDC a, b загружает в a константу b
    LD = (
        {"code": 0xF400000000000000, "symbol": "", "action": lambda a, b: False},
    )  # загрузка из памяти, LD a, b загружает в a значение по адресу b
    MOV = (
        {"code": 0xF500000000000000, "symbol": "", "action": lambda a, b: False},
    )  # MOV a, b переносит значение из a в b
    CALL = (
        {"code": 0xF600000000000000, "symbol": "", "action": lambda a, b: False},
    )  # CALL a, вызывает функцию по адресу a
    BRMN = (
        {"code": 0xF700000000000000, "symbol": "", "action": lambda a, b: False},
    )  # переход если больше 0, BRMN a, b, если a <= 0 переходит на  адрес  b
    JMP = (
        {"code": 0xF800000000000000, "symbol": "", "action": lambda a, b: False},
    )  # безусловный JUMP a, переходит на  адрес  a
    BRPL = (
        {"code": 0xF900000000000000, "symbol": "", "action": lambda a, b: False},
    )  # переход если больше 0, BRPLa, b, еслиa > 0 переходит на адрес b
    MOD = (
        {
            "code": 0xFA00000000000000,
            "symbol": "%",
            "action": lambda a, b: a.type == Type.NUMBER and b.type == Type.NUMBER,
        },
    )  # выводит остаток от деления, MOD a, b -> записывает в a остаток от деления a на b
    PRT = (
        {"code": 0xFB00000000000000, "symbol": "", "action": lambda a, b: False},
    )  # вывод в на внешнее устройство данные
    RDL = {"code": 0xFC00000000000000, "symbol": "", "action": lambda a, b: False}

    def __init__(self, vals):
        self.code = vals["code"]
        self.symbol = vals["symbol"]
        self.action = vals["action"]

    def get_code(self):
        return self.code

    def get_symbol(self):
        return self.symbol

    def get_action(self):
        return self.action

    def exists_symbol(self, symbol: str):
        return symbol in self.symbol


def get_action_by_character(character: str):
    for value in list(ISA):
        if value.get_symbol() == character:
            return value.get_action()


def get_command_by_character(character: str) -> ISA:
    for value in list(ISA):
        if value.get_symbol() == character:
            return value


class Register(enum.Enum):
    REG1 = (0,)
    REG2 = 1
    REG3 = 2
    REG4 = 3
    IP = 4
    SP = 5
    PEP = 6
    REG5 = 7

    def __init__(self, index_of_register):
        self.index_of_register = index_of_register

    def get_index_of_register(self):
        return self.index_of_register


def get_external_device() -> int:
    return external_device

import enum

from typeguard import typechecked

from Exceptions import VariableNotFoundException


class Type(enum.Enum):
    NUMBER = {
        'default_value': '0',
        'index': 0
    },
    BOOLEAN = {
        'default_value': 'false',
        'index': 1
    }
    STRING = {
        'default_value': '',
        'index': 2
    }
    LIST = {
        'default_value': '(0)',
        'index': 3
    }
    FUNCTION = {
        'default_value': '',
        'index': 4
    }
    UNIT = {
        'default_value': 'Unit',
        'index': 2
    }

    def __init__(self, vals):
        self.default_value = vals['default_value']
        self.index = vals['index']

    def get_default_value(self):
        return self.default_value

    def get_index(self):
        return self.index


@typechecked
class Variable:
    value: str
    type: Type
    size: int

    def __init__(self, value: str, type: Type):
        self.value: str = value
        self.type: Type = type


variables: dict[str, Variable] = {}
variables_addresses: dict[str, str] = {}


def get_variables() -> dict[str, Variable]:
    return variables


def get_variables_address() -> dict[str, str]:
    return variables_addresses


def find_variable(variable_name: str) -> Variable:
    if not exists_variable(variable_name):
        raise VariableNotFoundException(variable_name)

    return variables[variable_name]


def exists_variable(variable_name: str) -> bool:
    return variable_name in variables.keys()


def get_string_size(text: str) -> int:
    return int(len(text) / 8) + 1

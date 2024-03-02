from antlr4.tree.Tree import ParseTree
from typeguard import typechecked

from kotlisp.BinaryGenerator import get_bin, generate_load_command, generate_binary_command, generate_load_constant_command, \
    int_to_binary
from kotlisp.Exceptions import ParsingException, ArithmeticException
from kotlisp.Utlis import is_symbol, to_text, is_name, is_string, is_index, to_typed_list, define_type, is_list
from kotlisp.VM import Register, get_action_by_character, ISA, get_command_by_character
from kotlisp.Variable import Variable, Type, find_variable
from kotlisp.antlr.KotlispParser import KotlispParser


def define_math_expression(math_expression: KotlispParser.Math_expressionContext) -> Variable:
    bin = get_bin()
    symbol = math_expression.getChild(0)

    if is_symbol(symbol.getText()):
        variable: Variable = define_variable(math_expression.getChild(1), bin)
        generate_load_command(math_expression.getChild(1).getText(), Register.REG1)

        variable = compute_expression(
            math_expression=math_expression,
            index_of_child=2,
            variable=variable,
            action=get_action_by_character(symbol.getText()),
            command=get_command_by_character(symbol.getText()),
            bin=bin
        )

        return variable

    else:
        if not is_list(to_text(symbol)) and not is_string(to_text(symbol)):
            generate_load_command(symbol.getText(), Register.REG1)

        return define_variable(symbol, bin)


@typechecked
def compute_expression(
        math_expression: KotlispParser.Math_expressionContext,
        index_of_child: int,
        variable: Variable,
        action,
        command: ISA,
        bin: list[str]
):
    if index_of_child > math_expression.getChildCount() - 1:
        if command in [ISA.EQ, ISA.NEQ, ISA.GR, ISA.GREQ, ISA.LS, ISA.LSEQ]:
            return Variable(Type.BOOLEAN.default_value, Type.BOOLEAN)

        return Variable(variable.type.get_default_value(), variable.type)

    next_value = math_expression.getChild(index_of_child)
    second_variable = define_variable(next_value, bin)

    if not action(variable, second_variable):
        raise ArithmeticException()

    if variable.type == Type.LIST:
        if not compare_immutable_types(command, variable, second_variable):
            return Variable('false', Type.BOOLEAN)
    else:
        generate_load_command(next_value.getText(), Register.REG2)
        generate_binary_command(command, Register.REG1, Register.REG2)

    return compute_expression(
        math_expression=math_expression,
        index_of_child=index_of_child + 1,
        variable=variable,
        action=action,
        command=command,
        bin=bin
    )


@typechecked
def define_variable(parseTree: ParseTree, bin: list[str]) -> Variable:
    text = to_text(parseTree)

    if is_name(text):
        return find_variable(text)

    if is_string(text):
        replaced_text = text.replace('"', '')
        return Variable(replaced_text, Type.STRING)

    if is_index(text):
        return validate_index(parseTree, bin)
    else:
        return Variable(text, define_type(text))


@typechecked
def validate_index(index_context: KotlispParser.IndexContext, bin: list[str]) -> Variable:
    index_variable = None
    if index_context.NUMBER() is not None:
        index_variable = define_variable(index_context.NUMBER(), bin)
    else:
        index_variable = define_variable(index_context.NAME(1), bin)

    if index_variable.type != Type.NUMBER:
        raise ParsingException('Индекс должен быть представлен числом!')

    type_: Type = to_typed_list(define_variable(index_context.NAME(0), bin).value)[1]

    return Variable(type_.default_value, type_)


def compare_immutable_types(command: ISA, string1: Variable, string2: Variable) -> bool:
    if command == ISA.EQ:
        return equals_immutable_types(string1, string2)
    else:
        return not_equals_immutable_types(string1, string2)


def equals_immutable_types(string1: Variable, string2: Variable) -> bool:
    if string1.value != string2.value:
        generate_load_constant_command(Register.REG1, int_to_binary(0))
        return False
    else:
        generate_load_constant_command(Register.REG1, int_to_binary(1))
        return True


def not_equals_immutable_types(string1: Variable, string2: Variable) -> bool:
    if string1.value == string2.value:
        generate_load_constant_command(Register.REG1, int_to_binary(0))
        return False
    else:
        generate_load_constant_command(Register.REG1, int_to_binary(1))
        return True

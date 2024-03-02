from antlr4.tree.Tree import ParseTree
from typeguard import typechecked

from BinaryGenerator import generate_default_binary_command, int_to_binary, get_bin, generate_load_command, \
    generate_binary_command, generate_load_constant_command, generate_binary_variable, string_to_binary
from Exceptions import ParsingException
from Expression import define_math_expression, define_variable
from Utlis import random_string, is_name, is_string
from VM import ISA, Register
from Variable import Variable, Type, get_variables, get_variables_address, exists_variable
from antlr.KotlispParser import KotlispParser


@typechecked
def transform_kotlisp(kotlisp_context: KotlispParser.KotlispContext) -> bytearray:
    validate_kotlisp(kotlisp_context.children)
    init_addresses()
    debug()
    replace_variable_name_to_address()

    return to_byte_array()


@typechecked
def validate_kotlisp(children: list[KotlispParser.FunContext]):
    variables = get_variables()
    variables_addresses = get_variables_address()
    bin = get_bin()

    if children[0].NAME().getText() != 'main':
        raise ParsingException('Программа должна начинаться с функции main!')

    for child in children:
        function_name = child.NAME().getText()
        variables[function_name] = Variable("function", Type.FUNCTION)

    for child in children:
        function_name = child.NAME().getText()
        variables_addresses[function_name] = int_to_binary(len(bin))

        check_s_expression(child.s_expression())

        generate_default_binary_command(command=ISA.HLT if child.NAME().getText() == "main" else ISA.RET)


@typechecked
def check_s_expression(s_expression: KotlispParser.S_expressionContext) -> Variable:
    children: list[ParseTree] = s_expression.children

    if len(children) == 2:
        return Variable(Type.UNIT.default_value, Type.UNIT)

    for index in range(1, len(children) - 2):
        validate_s_expression(children[index])

    return validate_s_expression(children[-2])


@typechecked
def validate_s_expression(parseTree: ParseTree) -> Variable:
    match parseTree:

        case parseTree if isinstance(parseTree, KotlispParser.VariableContext):
            return validate_variable(parseTree)

        case parseTree if isinstance(parseTree, KotlispParser.LoopContext):
            return validate_loop(parseTree)

        case parseTree if isinstance(parseTree, KotlispParser.ConditionalContext):
            return validate_conditional(parseTree)

        case parseTree if isinstance(parseTree, KotlispParser.Fun_callContext):
            return validate_fun_call(parseTree)

        case parseTree if isinstance(parseTree, KotlispParser.Print_Context):
            return validate_print(parseTree)

        case parseTree if isinstance(parseTree, KotlispParser.S_expressionContext):
            return check_s_expression(parseTree)

        case parseTree if isinstance(parseTree, KotlispParser.Math_expressionContext):
            return define_math_expression(parseTree)

        case parseTree if isinstance(parseTree, KotlispParser.Read_lineContext):
            return validate_readline(parseTree)


@typechecked
def validate_variable(variable_context: KotlispParser.VariableContext) -> Variable:
    variables = get_variables()
    variable_name = variable_context.getChild(1).getText()

    if exists_variable(variable_name):

        prev_variable: Variable = variables[variable_name]

        variable = check_s_expression(s_expression=variable_context.s_expression())

        if variable.type == Type.FUNCTION:
            raise ParsingException(
                f'Нельзя использовать переменную с именем {variable_name}, так как существует функция с таким же именем!')

        if variable.type == Type.STRING or variable.type == Type.LIST:
            raise ParsingException("Нельзя изменять непримитивные типы данных!")

        if variable.type != prev_variable.type:
            raise ParsingException("Нельзя менять тип переменной!")

        if variable.type == Type.UNIT:
            generate_load_constant_command(Register.REG1, string_to_binary('Unit')[0])

        generate_load_constant_command(Register.REG2, variable_name)
        generate_binary_command(ISA.SV, Register.REG2, Register.REG1)
        variables[variable_name] = variable

    else:
        variable = check_s_expression(s_expression=variable_context.s_expression())

        if variable.type == Type.UNIT:
            generate_load_constant_command(Register.REG1, string_to_binary('Unit')[0])

        variables[variable_name] = variable

        if variable.type != Type.LIST and variable.type != Type.STRING:
            generate_load_constant_command(Register.REG2, variable_name)
            generate_binary_command(ISA.SV, Register.REG2, Register.REG1)

        if variable.type == Type.STRING and variable.value == Type.STRING.default_value:
            generate_load_constant_command(Register.REG2, variable_name)
            generate_binary_command(ISA.SV, Register.REG2, Register.REG1)

    return Variable(Type.UNIT.get_default_value(), Type.UNIT)


@typechecked
def validate_loop(loop_context: KotlispParser.LoopContext) -> Variable:
    index = loop_context.NAME(0).getText()
    variables = get_variables()
    bin = get_bin()

    if exists_variable(index):
        raise ParsingException(f'Переменная с именем {index} уже была объявлена!')

    variables[index] = Variable('0', Type.NUMBER)
    counter_name = None

    if loop_context.NAME(1) is not None:
        variable = define_variable(loop_context.NAME(1), bin)

        if variable.type != Type.NUMBER:
            raise ParsingException("Переменная количества итераций должна быть представлена числом!")

        counter_name = loop_context.NAME(1).getText()

    else:
        counter_name = random_string()
        variables[counter_name] = Variable(loop_context.NUMBER().getText(), Type.NUMBER)

    if int(variables[counter_name].value) <= 0:
        return Variable(Type.UNIT.default_value, Type.UNIT)

    start_address = len(bin)
    check_s_expression(loop_context.s_expression())

    generate_load_command(index, Register.REG1)
    generate_binary_command(ISA.INC, Register.REG1)
    generate_load_constant_command(Register.REG2, index)
    generate_binary_command(ISA.SV, Register.REG2, Register.REG1)
    generate_load_command(counter_name, Register.REG2)
    generate_binary_command(ISA.SUB, Register.REG2, Register.REG1)
    generate_load_constant_command(Register.REG1, int_to_binary(start_address))
    generate_binary_command(ISA.BRPL, Register.REG2, Register.REG1)

    generate_load_constant_command(Register.REG1, index)
    generate_load_constant_command(Register.REG2, int_to_binary(0))
    generate_binary_command(ISA.SV, Register.REG1, Register.REG2)

    return Variable(Type.UNIT.default_value, Type.UNIT)


@typechecked
def validate_conditional(conditional_context: KotlispParser.ConditionalContext) -> Variable:
    result: Variable = check_s_expression(conditional_context.s_expression(0))
    variables = get_variables()
    bin = get_bin()

    if result.type != Type.NUMBER and result.type != Type.BOOLEAN:
        raise ParsingException("Условие должно быть иметь типы number или boolean!")

    address_of_branch_if_false = random_string()

    generate_load_command(address_of_branch_if_false, Register.REG2)
    generate_binary_command(ISA.BRMN, Register.REG1, Register.REG2)

    variable_if_true = check_s_expression(conditional_context.s_expression(1))

    address_of_branch_after_cond = random_string()

    generate_load_command(address_of_branch_after_cond, Register.REG2)
    generate_binary_command(ISA.JMP, Register.REG2)

    variables[address_of_branch_if_false] = Variable(str(len(bin)), Type.NUMBER)

    variable_if_else = check_s_expression(conditional_context.s_expression(2))

    variables[address_of_branch_after_cond] = Variable(str(len(bin)), Type.NUMBER)

    if variable_if_else.type != variable_if_true.type:
        raise ParsingException('Несовпадают типы возвращаемых значений!')

    if variable_if_else.type == Type.STRING or variable_if_else.type == Type.LIST:
        raise ParsingException('Невозможно вернуть из условия if иммутабельные структуры данных')

    if variable_if_else.type != Type.UNIT:
        return variable_if_else

    return Variable(Type.UNIT.default_value, Type.UNIT)


@typechecked
def validate_fun_call(fun_call_context: KotlispParser.Fun_callContext) -> Variable:
    function_name = fun_call_context.NAME().getText()
    variables = get_variables()
    bin = get_bin()
    function = variables[function_name]

    if not exists_variable(function_name) or function.type != Type.FUNCTION:
        raise ParsingException(f"Функция с именем {function_name} не найдена!")

    generate_load_constant_command(Register.REG1, function_name)
    generate_load_constant_command(Register.REG2, int_to_binary(len(bin) + 7))
    generate_binary_command(ISA.PUSH, Register.REG2)
    generate_binary_command(ISA.CALL, Register.REG1)

    return Variable(Type.UNIT.default_value, Type.UNIT)


@typechecked
def validate_print(print_context: KotlispParser.Print_Context) -> Variable:
    argument = check_s_expression(print_context.s_expression())
    if argument.type == Type.STRING and argument.value != Type.STRING.default_value:
        data = print_context.s_expression().getText()[1:-1]
        if is_string(data):
            variables = get_variables()
            variable_name = random_string()
            variables[variable_name] = Variable(data[1:-1], Type.STRING)
            generate_load_constant_command(Register.REG1, variable_name)
        else:
            generate_load_constant_command(Register.REG1, data)

    if argument.type == Type.UNIT:
        variables = get_variables()
        variable_name = random_string()
        variables[variable_name] = Variable('Unit', Type.STRING)
        generate_load_constant_command(Register.REG1, variable_name)

    generate_load_constant_command(Register.REG2, int_to_binary(argument.type.index))
    generate_binary_command(ISA.PRT, Register.REG2, Register.REG1)

    return Variable(Type.UNIT.default_value, Type.UNIT)


def validate_readline(read_line_context: KotlispParser.Read_lineContext) -> Variable:
    text = read_line_context.getText()
    l = 0
    while text[l] != '(':
        l += 1
    type_ = text[l + 1: len(text) - 1]

    if type_ == 'bool':
        generate_load_constant_command(Register.REG2, int_to_binary(Type.BOOLEAN.index))
        generate_binary_command(ISA.RDL, Register.REG2)
        return Variable(Type.BOOLEAN.default_value, Type.BOOLEAN)
    if type_ == 'int':
        generate_load_constant_command(Register.REG2, int_to_binary(Type.NUMBER.index))
        generate_binary_command(ISA.RDL, Register.REG2)
        return Variable(Type.NUMBER.default_value, Type.NUMBER)
    else:
        generate_load_constant_command(Register.REG2, int_to_binary(Type.STRING.index))
        generate_binary_command(ISA.RDL, Register.REG2)
        return Variable(Type.STRING.default_value, Type.STRING)


def init_addresses():
    variables = get_variables()
    variable_addresses = get_variables_address()
    bin = get_bin()

    for variable_name, variable_data in variables.items():
        if variable_data.type != Type.FUNCTION:
            if variable_name not in variable_addresses.keys():
                variable_addresses[variable_name] = int_to_binary(len(bin))
                generate_binary_variable(variable_data)


def replace_variable_name_to_address():
    bin = get_bin()
    variables_addresses = get_variables_address()
    for index in range(0, len(bin)):
        address = bin[index]
        if is_name(bin[index]):
            address = variables_addresses[bin[index]]

        bin[index] = address


def to_byte_array() -> bytearray:
    bin = get_bin()

    bytes_ = []

    for b in bin:
        start = 0
        finish = 7
        for i in range(1, 9):
            byte = b[start: finish + 1]
            bytes_.append(int(byte, 2))
            start += 8
            finish += 8

    return bytearray(bytes_)


def debug():
    bin = get_bin()
    index = 0
    addr = get_variables_address()
    for b in bin:
        flag = True
        if b.isdigit():
            for l in list(ISA):
                if l.get_code() == int(b, 2):
                    flag = False
                    print(hex(index), l.name)
                    continue

            for l in list(Register):
                if l.get_index_of_register() == int(b, 2):
                    flag = False
                    print(hex(index), l.name)
        if flag:
            address = '' if not b.isalpha() else addr[b]
            print(hex(index), b, address)

        index += 1

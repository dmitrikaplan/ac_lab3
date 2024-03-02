import random
import re
import string

from antlr4.tree.Tree import ParseTree

from kotlisp.Exceptions import ParsingException
from kotlisp.VM import ISA
from kotlisp.Variable import Type


def is_symbol(text: str) -> bool:
    for operator in list(ISA):
        if operator.exists_symbol(text):
            return True

    return False


def is_number(text: str) -> bool:
    start_index = 1 if (text[0] == "-") else 0
    for i in range(start_index, len(text)):
        if not text[i].isdigit():
            return False
    return True


def is_string(text: str) -> bool:
    if text[0] != '"' or text[-1] != '"':
        return False

    symbols = ["?", "!", ">", "<", ",", ".", "\\", " "]

    for index in range(1, len(text) - 1):
        if not text[index].isdigit() and not text[index].isalpha() and text[index] not in symbols:
            return False

    return True


def is_name(text: str) -> bool:
    if is_boolean(text):
        return False

    for ch in text:
        if not ch.isalpha():
            return False

    return True


def is_boolean(text: str) -> bool:
    return text == "true" or text == "false"


def is_index(text: str) -> bool:
    l = re.split("[\[\]]", text)
    return len(l) == 3 and is_name(l[0]) and (is_name(l[1]) or is_number(l[1]))


def is_list(text: str) -> bool:
    return len(text) > 6 and text[:4] == "list"


def to_typed_list(l: str) -> tuple[list[str], Type]:
    left = index_of(l, "(")
    right = index_of(l, ")")

    values = list(map(lambda x: x.strip(), l[left + 1 : right].split(",")))

    return values, define_type(values[0])


def to_text(parseTree: ParseTree) -> str:
    if parseTree.getChildCount() == 0:
        return parseTree.getText()

    i = 0
    string = ""

    child_count = parseTree.getChildCount()
    while i < child_count and parseTree.getChild(i).getText() != "(":
        string += parseTree.getChild(i).getText()
        i += 1

    if child_count - 1 <= i:
        return string

    string += parseTree.getChild(i).getText()
    i += 1
    string += parseTree.getChild(i).getText()
    i += 1

    while parseTree.getChild(i).getText() != ")":
        string += ","
        string += parseTree.getChild(i).getText()
        i += 1

    return string + ")"


def random_string(length: int = 32):
    letters = string.ascii_lowercase
    return "".join(random.choice(letters) for i in range(length))


def index_of(s: str, ch: str) -> int:
    for i in range(0, len(s)):
        if s[i] == ch:
            return i

    return -1


def define_type(value: str) -> Type:
    if is_number(value):
        return Type.NUMBER

    if is_string(value):
        return Type.STRING

    if is_boolean(value):
        return Type.BOOLEAN

    if is_index(value):
        return Type.NUMBER

    if is_list(value):
        return Type.LIST

    raise ParsingException("Ошибка определения переменной!")

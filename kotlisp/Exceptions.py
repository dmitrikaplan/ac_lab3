class InvalidFileNameException(Exception):
    def __init__(self):
        self.msg = "Неверный формат файла!"

    def get_msg(self) -> str:
        return self.msg


class ArgsSizeException(Exception):
    def __init__(self):
        self.msg = "Должен быть только 2 аргумента!"

    def get_msg(self) -> str:
        return self.msg


class ParsingException(Exception):
    def __init__(self, msg: str):
        self.msg = msg

    def get_msg(self) -> str:
        return self.msg


class VariableNotFoundException(Exception):
    def __init__(self, variable_name: str):
        self.msg = f"Переменная с именем {variable_name} не найдена!"

    def get_msg(self):
        return self.msg


class ArithmeticException(Exception):
    def __init__(self):
        self.msg = "Невозможно выполнить арифметическую операцию!"

    def get_msg(self):
        return self.msg

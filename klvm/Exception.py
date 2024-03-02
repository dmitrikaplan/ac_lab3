class ISACommandNotFound(Exception):
    def __init__(self):
        self.msg = "Команда не найдена!"

    def get_msg(self):
        return self.msg


class ArgumentsSizeException(Exception):
    def __init__(self, operation_name: str):
        self.msg = f"Неправильное количество аргументов у операции ${operation_name}"

    def get_msg(self):
        return self.msg


class ArithmeticException(Exception):
    def __init__(self):
        self.msg = f"Результат операции вышел за пределы разрядной сетки!"

    def get_msg(self):
        return self.msg


class RuntimeException(Exception):
    def __init__(self, msg: str):
        self.msg = msg

    def get_msg(self):
        return self.msg


class ArgsSizeException(Exception):
    def __init__(self):
        self.msg = "Неверное количество аргументов для программы!"

    def get_msg(self):
        return self.msg

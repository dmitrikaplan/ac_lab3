import sys

from antlr4 import FileStream, CommonTokenStream
from typeguard import typechecked

from kotlisp.Exceptions import InvalidFileNameException, VariableNotFoundException, ArithmeticException, ParsingException, \
    ArgsSizeException
from kotlisp.KotlispTransformer import transform_kotlisp
from kotlisp.antlr.KotlispLexer import KotlispLexer
from kotlisp.antlr.KotlispParser import KotlispParser


def main(source, target):
    try:
        file_stream = FileStream(source)
        lexer = KotlispLexer(file_stream)
        stream = CommonTokenStream(lexer)
        parser = KotlispParser(stream)
        kotlisp: KotlispParser.KotlispContext = parser.kotlisp()
        byte_array = transform_kotlisp(kotlisp)
        write_bin(byte_array, target)

    except InvalidFileNameException as exception:
        print(exception.msg, file=sys.stderr)
    except VariableNotFoundException as exception:
        print(exception.msg, file=sys.stderr)
    except ArithmeticException as exception:
        print(exception.msg, file=sys.stderr)
    except ParsingException as exception:
        print(exception.msg, file=sys.stderr)
    except ArgsSizeException as exception:
        print(exception.msg, file=sys.stderr)


@typechecked
def validate_file_name(file_name: str):
    if not file_name.endswith('.klp'):
        raise InvalidFileNameException()


@typechecked
def validate_args_length(args: list[str]):
    if len(args) != 3:
        raise ArgsSizeException()


def write_bin(byte_array: bytearray, file_name: str):
    with open(file_name, 'wb') as f:
        f.write(byte_array)


if __name__ == '__main__':
    validate_args_length(sys.argv)
    _, source, target = sys.argv
    validate_file_name(source)
    main(source, target)

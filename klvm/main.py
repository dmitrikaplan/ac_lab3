import sys

from klvm import IO, CPU
from klvm.Converter import bytes_to_int
from klvm.Exception import ISACommandNotFound, ArgumentsSizeException, RuntimeException, ArithmeticException, \
    ArgsSizeException


def main(target, input_stream):
    try:
        data = read_bin_file(target)
        IO.input_file = input_stream
        CPU.start(data)
    except ISACommandNotFound as exception:
        print(exception.msg, file=sys.stderr)
    except ArgumentsSizeException as exception:
        print(exception.msg, file=sys.stderr)
    except RuntimeException as exception:
        print(exception.msg, file=sys.stderr)
    except ArithmeticException as exception:
        print(exception.msg, file=sys.stderr)
    except ArgsSizeException as exception:
        print(exception.msg, file=sys.stderr)


def read_bin_file(file_name: str) -> list[int]:
    result: list[int] = []

    with open(file_name, 'rb') as f:
        byte = 1
        while byte:
            byte = list(f.read(8))
            if len(byte) != 0:
                result.append(bytes_to_int(byte))

    return result


def validate_args_size(args: list[str]):
    if len(args) != 3:
        raise ArgsSizeException()


if __name__ == '__main__':
    validate_args_size(sys.argv)
    _, bin_file, input_file = sys.argv
    main(bin_file, input_file)

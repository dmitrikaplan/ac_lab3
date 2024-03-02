import sys

import CPU
from Converter import bytes_to_int
from Exception import ArgsSizeException, ISACommandNotFound, ArgumentsSizeException, ArithmeticException, \
    RuntimeException


def main():

    try:
        validate_args_size(sys.argv)
        data = read_bin_file(sys.argv[1])
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
    if len(args) != 2:
        raise ArgsSizeException()


if __name__ == '__main__':
    main()

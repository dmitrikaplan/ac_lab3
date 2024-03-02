import contextlib
import io
import logging
import os.path
import tempfile

import pytest

import klvm.main
import kotlisp.main


@pytest.mark.golden_test("golden/*.yml")
def test_kotlisp(golden, caplog):
    caplog.set_level(logging.DEBUG)

    with tempfile.TemporaryDirectory() as temp_dir_name:
        source_code = os.path.join(temp_dir_name, 'input.klp')
        input = os.path.join(temp_dir_name, "temp.txt")
        target = os.path.join(temp_dir_name, 'target.bin')

        print(target)

        # Записываем входные данные в файлы. Данные берутся из теста.
        with open(source_code, "w", encoding="utf-8") as file:
            file.write(golden["source"])
        with open(input, "w", encoding="utf-8") as file:
            file.write(golden["stdin"])

        with contextlib.redirect_stdout(io.StringIO()) as stdout:
            kotlisp.main.main(source_code, target)
            print("============================================================")
            klvm.main.main(target, input)

        with open(target, encoding="utf-8") as file:
            bin = file.read()

        assert stdout.getvalue() == golden.out["stdout"]
        print(golden.out["stdout"])
from dataclasses import dataclass
import os
import subprocess
import sys
from typing import Optional

LANG_EXT = ".xx"
TESTS_DIR = "./tests"

@dataclass
class TestCase:
    stdin: bytes
    returncode: int
    stdout: bytes
    stderr: bytes

def loadTestCase(filepath: str) -> Optional[TestCase]:
    result = TestCase(0, 0, 0, 0)
    try:
        with open(filepath, "rb") as f:
            header = f.readline().decode()
            parsed = header.split(":")
            if(parsed[0] == "stdout"):
                result.stdout = f.read(int(parsed[1]))
            elif(parsed[0] == "stderr"):
                result.stderr = f.read(int(parsed[1]))
            else:
                pass
        return result
    except FileNotFoundError:
        return None

if __name__ == "__main__":
    exe_name, *argv = sys.argv # shift
    subcommand = "run"
    if len(argv) > 0:
        subcommand, *argv = argv
    
    if subcommand == "record":
        for entry in os.scandir(TESTS_DIR):
            if entry.is_file() and entry.path.endswith(LANG_EXT):
                print(f"file found: {entry.name}, path: {entry.path[:-len(entry.name)]}")
                command = "./compiler.exe --tokens ./tests/" + entry.name
                output = subprocess.run(command, capture_output=True)
                with open(entry.path[:-len(entry.name)] + entry.name[:-len(LANG_EXT)] + ".txt", "wb") as f:
                    header = f"stdout:{len(output.stdout)}\n"
                    f.write(str.encode(header))
                    f.write(output.stdout)
                    f.close()
    elif subcommand == "run":
        test = loadTestCase("./tests/func.txt")
        if test:
            print(f"loaded from file: {test.stdout[:20]}")
    else:
        print(f"[ERROR] Subcommand {subcommand} cont recognized")

from dataclasses import dataclass
from subprocess import CompletedProcess
import os
import subprocess
import sys
from typing import Optional

LANG_EXT = ".xx"
TESTS_DIR = "./tests"

@dataclass
class TestCase:
    stdin: bytes
    returnCode: int
    stdout: bytes
    stderr: bytes

def loadTestCase(filepath: str) -> Optional[TestCase]:
    result = TestCase(0, 0, 0, 0)
    try:
        with open(filepath, "rb") as f:
            while True:
                header = f.readline().decode()
                if header == "":
                    break
                parsed = header.split(":")
                if parsed[0] == "stdout":
                    result.stdout = f.read(int(parsed[1]))
                elif parsed[0] == "stderr":
                    result.stderr = f.read(int(parsed[1]))
                elif parsed[0] == "stdin":
                    result.stdin = f.read(int(parsed[1]))
                elif parsed[0] == "return code":
                    result.returnCode = int(parsed[1])
                else:
                    return None
            return result
    except FileNotFoundError:
        return None

def writeTestCase(path: str, data: CompletedProcess[bytes]):
    with open(path, "wb") as f:
        # exit code
        f.write(f"return code:{data.returncode}\n".encode())
        # stdout
        if len(data.stdout) > 0:
            f.write(f"stdout:{len(data.stdout)}\n".encode())
            f.write(data.stdout + "\n".encode())
        else:
            f.write(f"stdout:0\n".encode())
        # stderr
        if len(data.stderr) > 0:
            f.write(f"stderr:{len(data.stderr)}\n".encode())
            f.write(data.stderr + "\n".encode())
        else:
            f.write(f"stderr:0\n".encode())

def buildCompiler():
    out = subprocess.run("./nobuild.exe debug")
    return out.returncode

def compileFile(path: str):
    out = subprocess.run(f"./compiler.exe -o {path[:-len(LANG_EXT)]}.asm {path}")
    return out.returncode

def usage(path: str):
    print(f"{path}               - run all tests")
    print(f"{path} run all       - run all tests")
    print(f"{path} run <path>    - run test at path")
    print(f"{path} record all    - run all tests and record the output as the expected result")
    print(f"{path} record <path> - run test at path and record the output as the expected result")

def testFile(path: str):
    if compileFile(path) != 0:
        print(f"[ERROR] Failed to build file: {path}")
        exit(1)
    
    if path.find("/asm") != -1:
        # found
        expectedPath = path[:-len(LANG_EXT)] + ".test"
        asmPath      = path[:-len(LANG_EXT)] + ".asm"
        
        expected = ""
        try:
            with open(expectedPath, "rb") as f:
                expected =  f.read()
        except FileNotFoundError:
            print(f"[ERROR] Could not find file: {expectedPath}")
            exit(1)

        recieved = ""
        try:
            with open(asmPath, "rb") as f:
                recieved =  f.read()
        except FileNotFoundError:
            print(f"[ERROR] Could not find file: {expectedPath}")
            exit(1)

        if recieved == expected:
            # passed
            return True
        else:
            # failed
            return False
    elif path.find("/exe") != -1:
        # found
        expectedPath = path[:-len(LANG_EXT)] + ".test"
        asmPath      = path[:-len(LANG_EXT)] + ".asm"
        objPath      = path[:-len(LANG_EXT)] + ".obj"
        exePath      = path[:-len(LANG_EXT)] + ".exe"
        
        expected = loadTestCase(expectedPath)
        if expected == None:
            print(f"[ERROR] Failed to load test case: {expectedPath}")
            exit(1)
        
        # nasm -f win64 output.asm
        out = subprocess.run(f"nasm -f win64 -o {objPath} {asmPath}")
        if out.returncode != 0:
            print("[ERROR] NASM failed")
            exit(1)

        # TODO: the entry point will change
        # ld -e _start -o output.exe output.obj
        out = subprocess.run(f"ld -e _start -o {exePath} {objPath}")
        if out.returncode != 0:
            print("[ERROR] Linking failed")
            exit(1)
            
        out = subprocess.run(f"{exePath}", capture_output=True)
        if expected.returnCode == out.returncode and expected.stdout == out.stdout and expected.stderr == out.stderr:
            # passed
            return True
        else:
            return False
    else:
        print("[ERROR] Could not find test type")
        exit(1)

def recordTestCase(path: str) -> bool:
    if compileFile(path) != 0:
        print(f"[ERROR] Failed to build file: {path}")
        exit(1)
    
    if path.find("/asm") != -1:
        # asm
        expectedPath = path[:-len(LANG_EXT)] + ".test"
        asmPath      = path[:-len(LANG_EXT)] + ".asm"
        
        try:
            with open(asmPath, "rb") as f:
                recieved = f.read()
        except FileNotFoundError:
            print(f"[ERROR] Could not find file: {expectedPath}")
            exit(1)
        
        with open(expectedPath, "wb") as f:
            f.write(recieved)

        return True
    elif path.find("/exe") != -1:
        # exe
        expectedPath = path[:-len(LANG_EXT)] + ".test"
        asmPath      = path[:-len(LANG_EXT)] + ".asm"
        objPath      = path[:-len(LANG_EXT)] + ".obj"
        exePath      = path[:-len(LANG_EXT)] + ".exe"

        # nasm -f win64 output.asm
        out = subprocess.run(f"nasm -f win64 -o {objPath} {asmPath}")
        if out.returncode != 0:
            print("[ERROR] NASM failed")
            exit(1)

        # TODO: the entry point will change
        # ld -e _start -o output.exe output.obj
        out = subprocess.run(f"ld -e _start -o {exePath} {objPath}")
        if out.returncode != 0:
            print("[ERROR] Linking failed")
            exit(1)
            
        out = subprocess.run(f"{exePath}", capture_output=True)
        writeTestCase(expectedPath, out)

        return True
    else:
        print("[ERROR] Could not find test type")
        exit(1)

if __name__ == "__main__":
    exe_name, *argv = sys.argv # shift
    subcommand = "run"
    if len(argv) > 0:
        subcommand, *argv = argv
    
    if subcommand == "run":
        if buildCompiler() != 0:
            print("[ERROR] Could not rebuild compiler")
            exit(1)
        
        path = "all"
        if len(argv) > 0:
            path, *argv = argv

        if path == "all":
            count = 0
            passed = 0
            failed = 0
            # scan asm dir
            for entry in os.scandir(TESTS_DIR + "/asm"):
                if entry.path[-3:] == LANG_EXT:
                    count += 1
                    if testFile(entry.path):
                        print(f"[OK] Passed: {entry.path}")
                        passed += 1
                    else:
                        print(f"[FAIL] Failed: {entry.path}")
                        failed += 1
            # scan exe dir
            for entry in os.scandir(TESTS_DIR + "/exe"):
                if entry.path[-3:] == LANG_EXT:
                    count += 1
                    if testFile(entry.path):
                        print(f"[OK] Passed: {entry.path}")
                        passed += 1
                    else:
                        print(f"[FAIL] Failed: {entry.path}")
                        failed += 1
        else:
            # test only one file
            if testFile(path):
                print(f"[OK] Passed: {path}")
                exit(0)
            else:
                print(f"[FAIL] Failed: {path}")
                exit(0)

        # print collected info
        print(f"Total number of test run: {count} passed: {passed}/{count} failed: {failed}")
            
    elif subcommand == "record":
        if buildCompiler() != 0:
            print("[ERROR] Could not rebuild compiler")
            exit(1)
        
        path = "all"
        if len(argv) > 0:
            path, *argv = argv

        if path == "all":
            count = 0
            passed = 0
            failed = 0
            # scan asm dir
            for entry in os.scandir(TESTS_DIR + "/asm"):
                if entry.path[-3:] == LANG_EXT:
                    count += 1
                    if recordTestCase(entry.path):
                        print(f"[OK] Recorded: {entry.path}")
                        passed += 1
                    else:
                        print(f"[FAIL] Failed: {entry.path}")
                        failed += 1
            # scan exe dir
            for entry in os.scandir(TESTS_DIR + "/exe"):
                if entry.path[-3:] == LANG_EXT:
                    count += 1
                    if recordTestCase(entry.path):
                        print(f"[OK] Recorded: {entry.path}")
                        passed += 1
                    else:
                        print(f"[FAIL] Failed: {entry.path}")
                        failed += 1
        else:
            # record one
            if recordTestCase(path):
                print(f"[OK] Recorded: {path}")
                exit(0)
            else:
                print(f"[FAIL] Failed: {path}")
                exit(0)

        # print collected info
        print(f"Total number of test recorded: {count} success: {passed}/{count} failed: {failed}")
    elif subcommand == "help":
        usage(exe_name)
    else:
        print(f"[ERROR] Subcommand not found: {subcommand}")
        exit(1)
    
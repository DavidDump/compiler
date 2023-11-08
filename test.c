#include <windows.h>
#include <assert.h>
#include <stdio.h>

#define MINIRENT_IMPLEMENTATION
#include "minirent.h"

typedef int bool;
#define TRUE 1
#define FALSE 0

// https://stackoverflow.com/questions/1387064/how-to-get-the-error-message-from-the-error-code-returned-by-getlasterror
LPSTR GetLastErrorAsString(void){
    DWORD errorMessageId = GetLastError();
    if(errorMessageId == 0) return "0";

    LPSTR messageBuffer = NULL;

    DWORD dwFlags = FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS;
    DWORD size = FormatMessage(
        dwFlags,                                   // DWORD   dwFlags,
        NULL,                                      // LPCVOID lpSource,
        errorMessageId,                            // DWORD   dwMessageId,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // DWORD   dwLanguageId,
        (LPSTR) &messageBuffer,                    // LPTSTR  lpBuffer,
        0,                                         // DWORD   nSize,
        NULL                                       // va_list *Arguments
    );

    return messageBuffer;
}

char* ReadFromPipe(HANDLE handle){
    DWORD bytesAvailable = 0;
    DWORD bytesRead = 0;

    if(!PeekNamedPipe(handle, NULL, 0, NULL, &bytesAvailable, NULL)){
        printf("[ERROR] Failed to peek from pipe: %s\n", GetLastErrorAsString());
        exit(EXIT_FAILURE);
    }

    if(bytesAvailable == 0) return NULL;

    char* buffer = calloc(1, bytesAvailable + 1);
    assert(buffer);
    if(!ReadFile(handle, buffer, bytesAvailable, &bytesRead, NULL)){
        printf("[ERROR] Failed to peek from pipe\n");
        exit(EXIT_FAILURE);
    }

    return buffer;
}

// NOTE: the pipe ends passed in belong to the child, and are closed on this end
HANDLE CreateChildProcess(char* cmd, HANDLE fdin, HANDLE fdout){
    STARTUPINFO siStartInfo;
    ZeroMemory(&siStartInfo, sizeof(siStartInfo));
    siStartInfo.cb = sizeof(STARTUPINFO);
    siStartInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    siStartInfo.hStdOutput = fdout ? fdout : GetStdHandle(STD_OUTPUT_HANDLE);
    siStartInfo.hStdInput  = fdin  ? fdin  : GetStdHandle(STD_INPUT_HANDLE);
    siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

    PROCESS_INFORMATION piProcInfo;
    ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION));

    if(!CreateProcess(NULL, cmd, NULL, NULL, TRUE, 0, NULL, NULL, &siStartInfo, &piProcInfo)){
        printf("[ERROR] Failed to create child process: %s\n", GetLastErrorAsString());
    }

    if(fdout) CloseHandle(fdout);
    if(fdin)  CloseHandle(fdin);
    CloseHandle(piProcInfo.hThread);

    return piProcInfo.hProcess;
}

bool WaitForPid(HANDLE pid){
    DWORD result = WaitForSingleObject(pid, INFINITE);

    if (result == WAIT_FAILED) {
        printf("could not wait on child process: %s", GetLastErrorAsString());
        exit(EXIT_FAILURE);
    }

    DWORD exit_status;
    if (GetExitCodeProcess(pid, &exit_status) == 0) {
        printf("could not get process exit code: %lu", GetLastError());
        exit(EXIT_FAILURE);
    }

    if (exit_status != 0) {
        printf("command exited with exit code %lu", exit_status);
        exit(EXIT_FAILURE);
    }

    CloseHandle(pid);
}

int main(int argc, char** argv){
    // args
    if(argc < 2){
        printf("Usage: %s [subcommand]\n", argv[0]);
        printf("Subcommands: run\n");
        printf("             record\n");
        exit(EXIT_FAILURE);
    }

    bool record = FALSE;
    bool run = FALSE;
    if(strcmp(argv[1], "record") == 0) record = TRUE;
    else if(strcmp(argv[1], "run") == 0) run = TRUE;
    else printf("[ERROR] Invalid subcommand: %s\n", argv[1]);

    HANDLE childStdInR = NULL;
    HANDLE childStdInW = NULL; // use this
    HANDLE childStdOutR = NULL; // use this
    HANDLE childStdOutW = NULL;

    SECURITY_ATTRIBUTES saAttr = {0};
	saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = NULL;
	// Creating the pipes
	// stdout
	if(!CreatePipe(&childStdOutR, &childStdOutW, &saAttr, 0)){
		printf("[ERROR] Failed creating pipe for stdout.\n");
		exit(EXIT_FAILURE);
	}
	if(!SetHandleInformation(childStdOutR, HANDLE_FLAG_INHERIT, 0)){
		printf("[ERROR] Failed setting the inherit flag for stdout.\n");
		exit(EXIT_FAILURE);
	}
	// stdin
	if(!CreatePipe(&childStdInR, &childStdInW, &saAttr, 0)){
		printf("[ERROR] Failed creating pipe for stdout.\n");
		exit(EXIT_FAILURE);
	}
	if(!SetHandleInformation(childStdInW, HANDLE_FLAG_INHERIT, 0)){
		printf("[ERROR] Failed setting the inherit flag for stdout.\n");
		exit(EXIT_FAILURE);
	}

    DIR *dir = opendir("./tests/");
    if(dir){
        // error
    }

    errno = 0;
    struct dirent *dp = NULL;
    while ((dp = readdir(dir))) {
        if(dp->d_name[0] == '.') continue;
        int filenameLen = strlen(dp->d_name);

        char* command = "./compiler.exe --tokens ./tests/";
        int commandLen = strlen(command);
        char* commandFull = calloc(1, filenameLen + commandLen + 1);
        memcpy(commandFull, command, commandLen);
        memcpy(commandFull + commandLen, dp->d_name, filenameLen);

        // printf("full command: %s\n", commandFull);
        WaitForPid(CreateChildProcess(commandFull, childStdInR, childStdOutW));
        char* out = ReadFromPipe(childStdOutR);
        free(out);
    }
    assert(errno == 0);

    int err = closedir(dir);
    assert(err == 0);

    CloseHandle(childStdInW);
    CloseHandle(childStdOutR);

    return EXIT_SUCCESS;
}
#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#define NOB_EXPERIMENTAL_DELETE_OLD
#include "nob.h"

#define SRCPATH "src"
#define BUILDPATH "build"

#define TARGET "compiler.exe"

bool strEndsWith(const char* path, const char* postfix) {
    const size_t cstr_len = strlen(path);
    const size_t postfix_len = strlen(postfix);
    return postfix_len <= cstr_len && strcmp(path + cstr_len - postfix_len, postfix) == 0;
}

const char* stripExtention(const char* path) {
    size_t n = strlen(path);
    while (n > 0 && path[n - 1] != '.') {
        n -= 1;
    }

    if (n > 0) {
        char *result = malloc(n);
        memcpy(result, path, n);
        result[n - 1] = '\0';

        return result;
    } else {
        return path;
    }
}

void buildMsvc(bool full, bool debug) {
#define CXX "cl"
// #define CFLAGS "/W4", "/nologo"
#define CFLAGS "/nologo"
#define DEBUGFLAGS "/Od", "/Zi", "/DCOMP_DEBUG"

    Cmd cmd = {0};

    File_Paths filesInDir = {0};
    File_Paths dstFiles = {0};
    File_Paths deps = {0};
    if(!read_entire_dir(SRCPATH, &filesInDir)) exit(1);
    for(int i = 0; i < filesInDir.count; ++i) {
        const char* srcFile = filesInDir.items[i];
        
        // check if source file
        if(!strEndsWith(srcFile, ".c")) continue;

        // src file: src/string.c
        char* srcPath = temp_sprintf("%s/%s", SRCPATH, srcFile);
        da_append(&deps, srcPath);

        // header file: src/string.h
        const char* headrFile = stripExtention(srcFile);
        char* headrPath = temp_sprintf("%s/%s.h", SRCPATH, headrFile);

        // dst file: build/string.o
        const char* dstFile = stripExtention(srcFile);
        char* dstPath = temp_sprintf("%s/%s.obj", BUILDPATH, dstFile);
        da_append(&dstFiles, dstPath);

        // check if the source file has a header
        int res = file_exists(headrPath);
        if(res == 1) da_append(&deps, headrPath);
        else if(res == -1) exit(1);

        if(full || !file_exists(dstPath) || needs_rebuild(dstPath, deps.items, deps.count)) {
            cmd_append(&cmd, CXX, CFLAGS);
            if(debug) cmd_append(&cmd, DEBUGFLAGS);
            cmd_append(&cmd, temp_sprintf("/Fo:%s", dstPath));
            cmd_append(&cmd, "/c", srcPath);
            if(!cmd_run_sync_and_reset(&cmd)) exit(1);
        }

        deps.count = 0;
    }

    cmd_append(&cmd, CXX, CFLAGS);
    if(debug) cmd_append(&cmd, DEBUGFLAGS);
    cmd_append(&cmd, "/Fe:" TARGET);
    da_append_many(&cmd, dstFiles.items, dstFiles.count);
    if(!cmd_run_sync_and_reset(&cmd)) exit(1);

#undef CXX
#undef CFLAGS
#undef DEBUGFLAGS
}

void buildGcc(bool full, bool debug) {
#define CXX "gcc"
#define CFLAGS "-Wall", "-Wextra", "-Werror", "-Wshadow", "-pedantic"
#define DEBUGFLAGS "-O0", "-ggdb", "-DCOMP_DEBUG"

    Cmd cmd = {0};

    File_Paths filesInDir = {0};
    File_Paths dstFiles = {0};
    File_Paths deps = {0};
    if(!read_entire_dir(SRCPATH, &filesInDir)) exit(1);
    for(int i = 0; i < filesInDir.count; ++i) {
        const char* srcFile = filesInDir.items[i];
        
        // check if source file
        if(!strEndsWith(srcFile, ".c")) continue;

        // src file: src/string.c
        char* srcPath = temp_sprintf("%s/%s", SRCPATH, srcFile);
        da_append(&deps, srcPath);

        // header file: src/string.h
        const char* headrFile = stripExtention(srcFile);
        char* headrPath = temp_sprintf("%s/%s.h", SRCPATH, headrFile);

        // dst file: build/string.o
        const char* dstFile = stripExtention(srcFile);
        char* dstPath = temp_sprintf("%s/%s.o", BUILDPATH, dstFile);
        da_append(&dstFiles, dstPath);

        // check if the source file has a header
        int res = file_exists(headrPath);
        if(res == 1) da_append(&deps, headrPath);
        else if(res == -1) exit(1);

        if(full || !file_exists(dstPath) || needs_rebuild(dstPath, deps.items, deps.count)) {
            cmd_append(&cmd, CXX, CFLAGS);
            if(debug) cmd_append(&cmd, DEBUGFLAGS);
            cmd_append(&cmd, "-o", dstPath);
            cmd_append(&cmd, "-c", srcPath);
            if(!cmd_run_sync_and_reset(&cmd)) exit(1);
        }

        deps.count = 0;
    }

    cmd_append(&cmd, CXX, CFLAGS);
    if(debug) cmd_append(&cmd, DEBUGFLAGS);
    cmd_append(&cmd, "-o", TARGET);
    da_append_many(&cmd, dstFiles.items, dstFiles.count);
    if(!cmd_run_sync_and_reset(&cmd)) exit(1);

#undef CXX
#undef CFLAGS
#undef DEBUGFLAGS
}

int main(int argc, char **argv) {
    NOB_GO_REBUILD_URSELF(argc, argv);

    Cmd cmd = {0};

    // parse args
    char* name = shift(argv, argc);

    bool clean = FALSE;
    bool debug = FALSE;
    bool full  = FALSE;
    bool msvc  = FALSE;
    bool gcc   = FALSE;

    char* arg = 0;
    while(argc > 0) {
        arg = shift(argv, argc);
        if(0);
        else if(strcmp(arg, "clean") == 0) clean = TRUE;
        else if(strcmp(arg, "debug") == 0) debug = TRUE;
        else if(strcmp(arg, "full")  == 0) full  = TRUE;
        else if(strcmp(arg, "msvc")  == 0) msvc  = TRUE;
        else if(strcmp(arg, "gcc")   == 0) gcc   = TRUE;
        else {
            nob_log(NOB_ERROR, "no subcommand \"%s\" found\n", arg);
            return 1;
        }
    }

    // check compiler flags
    if(msvc && gcc) {
        nob_log(NOB_ERROR, "Only one compiler can be specified: \"msvc\" or \"gcc\"");
        return 1;
    }

    if(msvc == FALSE && gcc == FALSE) {
        gcc = TRUE;
        nob_log(NOB_INFO, "compiler not specified, assuming: gcc");
    }

    // clean
    if(clean && file_exists(BUILDPATH) == 1) {
        File_Paths buildDirPaths = {0};
        if(!read_entire_dir(BUILDPATH, &buildDirPaths)) return 1;
        for(int i = 0; i < buildDirPaths.count; ++i) {
            if(strcmp(buildDirPaths.items[i], ".") == 0 || strcmp(buildDirPaths.items[i], "..") == 0) continue;
            char* path = temp_sprintf("%s/%s", BUILDPATH, buildDirPaths.items[i]);
            if(!delete_file(path)) return 1;
        }

        if(file_exists(TARGET) == 1) if(!delete_file(TARGET)) return 1;
        return 0;
    }

    if(!mkdir_if_not_exists(BUILDPATH)) return 1;

    // build using the selected compiler
    if(0);
    else if(msvc) buildMsvc(full, debug);
    else if(gcc)  buildGcc(full, debug);

    return 0;
}

#define NOBUILD_IMPLEMENTATION
#include "nobuild.h"

#define CXX "gcc"
// NOTE: -w flag has an enable (-Wall) and disable (-Wno-all) version
#define CFLAGS "-Wall"
#define DEBUGFLAGS "-O0 -ggdb"

#define SRCPATH "src"
#define BUILDPATH "build"

#define TARGET "main.exe"

void EnsureBuildDir(){
    if(PATH_EXISTS(PATH("build"))) return;
    MKDIRS(PATH("build"));
}

int main(int argc, char** argv){
    GO_REBUILD_URSELF(argc, argv);

    int debug = 0;
    if(argc > 1){
        if(strcmp(argv[1], "clean") == 0){
            RM(PATH("build"));
            return 0;
        }else if(strcmp(argv[1], "debug") == 0){
            debug = 1;
        }else{
            printf("no subcommand \"%s\" found\n", argv[1]);
            return 1;
        }
    }

    Cstr objString = 0;

    EnsureBuildDir();

    // Build all object files and collect paths in objString
    DIR* dir = opendir(PATH(SRCPATH));
    struct dirent* ent;
    while(ent = readdir(dir)){
        int extCorrect = ENDS_WITH(ent->d_name, ".c");
        if(extCorrect){
            Cstr srcPath = PATH(JOIN("/", SRCPATH, ent->d_name));
            Cstr destPath = PATH(JOIN("/", BUILDPATH, CONCAT(NOEXT(ent->d_name), ".o")));

            if(!objString) objString = destPath;
            else objString = JOIN(" ", objString, destPath);

            // TODO: also rebuild if the .h file is updated
            if(!PATH_EXISTS(PATH(destPath)) || is_path1_modified_after_path2(srcPath, destPath)){
                if(!debug) CMD(CXX, CFLAGS, "-c", srcPath, "-o", destPath);
                else CMD(CXX, CFLAGS, DEBUGFLAGS, "-c", srcPath, "-o", destPath);
            }
        }
    }
    closedir(dir);

    if(!debug) CMD(CXX, CFLAGS, "-o", TARGET, objString);
    else CMD(CXX, CFLAGS, DEBUGFLAGS, "-o", TARGET, objString);

    return 0;
}
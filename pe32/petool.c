#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef struct Args {
    int read;
    int debugPrintStringTable;
    int stringTableLimit;
    int debugPrintPeHeader;
    int debugPrintOptStandardHeader;
    int debugPrintOptSpecHeader;
    int debugPrintOptDataHeader;
    int debugPrintSectionTable;
    int debugPrintSymbolTable;
    int debugPrintImportDirectoryTable;
    int debugPrintImportLookupTable;
    char* filename;
    char* dumpfilepath;

    int write;
} Args;

char* EntireFileRead(const char* filePath, int* len){
    FILE* f = fopen(filePath, "rb");
    
    if(f){
        fseek(f, 0, SEEK_END);
        *len = ftell(f);
        fseek(f, 0, SEEK_SET);
        
        char* fileBuffer = malloc(*len * sizeof(char));
        fread(fileBuffer, sizeof(char), *len, f);
        fclose(f);

        return fileBuffer;
    }else{
        printf("[ERROR] Failed to open file: %s\n", filePath);
        return NULL;
    }
}

#include "common.h"
#include "peReaders.c"
#include "peWriters.c"

void usage(int argc, char** argv) {
    printf("PE Tool v0.1\n");
    printf("usage: %s <read|write> [options] <filepath>\n", argv[0]);
    printf("    read mode: read the PE file contents and print the info to the screen\n");
    printf("        read options:\n");
    printf("        -o <filepath>          Output the read .text segment to the specified file\n");
    printf("        -pe-header             Print out the PE header\n");
    printf("        -opt-std-header        Print out the optional standard header\n");
    printf("        -opt-spec-header       Print out the optional windowns specific header\n");
    printf("        -opt-data-header       Print out the optional data directory header\n");
    printf("        -section-table         Print out the section table\n");
    printf("        -symbol-table          Print out the symbol table\n");
    printf("        -string-table <limit>  Print out the strings stored in the string table of the file\n");
    printf("                               limit is the number of strings you want to print, set to -1 for no limit\n");
    printf("        -import-dirs           Print out the import directory table\n");
    printf("        -import-names          Print out the symbols to import\n");
    printf("        -all                   Print all the headers and tables\n");
    printf("    write mode: use bytecode data to create a PE32 file\n");
}

int main(int argc, char** argv) {
    if(argc < 3) {
        usage(argc, argv);
        return 1;
    }

    Args args = {0};
    for(int i = 1; i < argc; i++) {
        if(strcmp(argv[i], "read") == 0) {
            args.read = 1;
            i++;
            
            if(strcmp(argv[i], "-o") == 0) {
                i++;
                args.dumpfilepath = argv[i];
                i++;
            } else if(strcmp(argv[i], "-pe-header") == 0) {
                args.debugPrintPeHeader = 1;
                i++;
            } else if(strcmp(argv[i], "-opt-std-header") == 0) {
                args.debugPrintOptStandardHeader = 1;
                i++;
            } else if(strcmp(argv[i], "-opt-spec-header") == 0) {
                args.debugPrintOptSpecHeader = 1;
                i++;
            } else if(strcmp(argv[i], "-opt-data-header") == 0) {
                args.debugPrintOptDataHeader = 1;
                i++;
            } else if(strcmp(argv[i], "-section-table") == 0) {
                args.debugPrintSectionTable = 1;
                i++;
            } else if(strcmp(argv[i], "-symbol-table") == 0) {
                args.debugPrintSymbolTable = 1;
                i++;
            } else if(strcmp(argv[i], "-string-table") == 0) {
                args.debugPrintStringTable = 1;
                i++;

                // lmit 
                args.stringTableLimit = strtol(argv[i], NULL, 10);
                i++;
            } else if(strcmp(argv[i], "-import-dirs") == 0) {
                args.debugPrintImportDirectoryTable = 1;
                i++;
            } else if(strcmp(argv[i], "-import-names") == 0) {
                args.debugPrintImportLookupTable = 1;
                i++;
            } else if(strcmp(argv[i], "-all") == 0) {
                args.debugPrintStringTable = 1;
                args.stringTableLimit = 1;
                args.debugPrintPeHeader = 1;
                args.debugPrintOptStandardHeader = 1;
                args.debugPrintOptSpecHeader = 1;
                args.debugPrintOptDataHeader = 1;
                args.debugPrintSectionTable = 1;
                args.debugPrintSymbolTable = 1;
                args.debugPrintImportDirectoryTable = 1;
                args.debugPrintImportLookupTable = 1;
                i++;
            } else if(argv[i][0] == '-'){
                printf("[ERROR] Unkown argument: %s\n", argv[i]);
                return 1;
            }

            if(i == argc - 1){
                args.filename = argv[i];
                break;
            } else {
                usage(argc, argv);
                printf("[ERROR] Could not parse options\n");
                return 1;
            }
        } else if(strcmp(argv[i], "write") == 0) {
            args.write = 1;
        } else {
            usage(argc, argv);
            printf("[ERROR] Unkown argument: %s\n", argv[i]);
            return 1;
        }
    }

    ReadResult fileData = readFile(args);

    printf("Number of bytes read: %i/%i (%.2f%%)\n", bytesRead, fileLen, ((float)bytesRead / (float)fileLen) * 100);
    return 0;
}

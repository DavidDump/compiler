#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "peReaders.c"

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
    printf("    write mode: use bytecode data to create a PE32 file\n");
}

int main(int argc, char** argv) {
    if(argc < 3) {
        usage(argc, argv);
        return 1;
    }

    int read = 0;
    int write = 0;
    int debugPrintStringTable = 0;
    int stringTableLimit = 0;
    int debugPrintPeHeader = 0;
    int debugPrintOptStandardHeader = 0;
    int debugPrintOptSpecHeader = 0;
    int debugPrintOptDataHeader = 0;
    int debugPrintSectionTable = 0;
    int debugPrintSymbolTable = 0;
    int debugPrintImportDirectoryTable = 0;
    int debugPrintImportLookupTable = 0;
    char* filename = NULL;
    char* dumpfilepath = NULL;
    for(int i = 1; i < argc; i++) {
        if(strcmp(argv[i], "read") == 0) {
            read = 1;
            i++;
            
            if(strcmp(argv[i], "-o") == 0) {
                i++;
                dumpfilepath = argv[i];
                i++;
            } else if(strcmp(argv[i], "-pe-header") == 0) {
                debugPrintPeHeader = 1;
                i++;
            } else if(strcmp(argv[i], "-opt-std-header") == 0) {
                debugPrintOptStandardHeader = 1;
                i++;
            } else if(strcmp(argv[i], "-opt-spec-header") == 0) {
                debugPrintOptSpecHeader = 1;
                i++;
            } else if(strcmp(argv[i], "-opt-data-header") == 0) {
                debugPrintOptDataHeader = 1;
                i++;
            } else if(strcmp(argv[i], "-section-table") == 0) {
                debugPrintSectionTable = 1;
                i++;
            } else if(strcmp(argv[i], "-symbol-table") == 0) {
                debugPrintSymbolTable = 1;
                i++;
            } else if(strcmp(argv[i], "-string-table") == 0) {
                debugPrintStringTable = 1;
                i++;

                // lmit 
                stringTableLimit = strtol(argv[i], NULL, 10);
                i++;
            } else if(strcmp(argv[i], "-import-dirs") == 0) {
                debugPrintImportDirectoryTable = 1;
                i++;
            } else if(strcmp(argv[i], "-import-names") == 0) {
                debugPrintImportLookupTable = 1;
                i++;
            } else if(argv[i][0] == '-'){
                printf("[ERROR] Unkown argument: %s\n", argv[i]);
                return 1;
            }

            if(i == argc - 1){
                filename = argv[i];
                break;
            } else {
                usage(argc, argv);
                printf("[ERROR] Could not parse options\n");
                return 1;
            }
        } else if(strcmp(argv[i], "write") == 0) {
            write = 1;
        } else {
            usage(argc, argv);
            printf("[ERROR] Unkown argument: %s\n", argv[i]);
            return 1;
        }
    }

    int fileLen = 0;
    u8* fileBuffer = EntireFileRead(filename, &fileLen);
    if(fileBuffer[0] != 0x4d || fileBuffer[1] != 0x5a){
        printf("[ERROR] Magic number not found\n");
        return 1;
    }

    int nextSectionFirstByteAddr = 0;

    #define PE_HEADER_PTR_LOC 0x3c
    u32 peHeaderOffset = Read32(fileBuffer, PE_HEADER_PTR_LOC);
    printf("peHeaderOffset: 0x%x\n", peHeaderOffset);
    u32 peMagic = Read32(fileBuffer, peHeaderOffset);
    peHeaderOffset += 4;
    printf("peMagic: 0x%x\n", peMagic);

    // PE Header
    PEHeader peHeader = readPeHeader(fileBuffer, peHeaderOffset);
    if(debugPrintPeHeader) printPeHeader(peHeader);
    nextSectionFirstByteAddr = peHeaderOffset + sizeOfPeHeader;

    // Optonal Standard Header
    u32 optionalStandardOffset = nextSectionFirstByteAddr;
    OptionalHeaderStandardFields optionalStandard = readOptHeader(fileBuffer, optionalStandardOffset);
    if(debugPrintOptStandardHeader) printOptHeader(optionalStandard);
    nextSectionFirstByteAddr = optionalStandardOffset + sizeOfOptionalHeaderStandardFields;

    // Optional Windows Specific Header
    if(optionalStandard.Magic == OPT_HEADR_MAGIC_PE) {
        // pe32
        u32 optHeaderWinSpecOffset = nextSectionFirstByteAddr;
        OptionalHeaderWindowsSpecificFields32 optHeaderWinSpec = readOptHeaderWinSpec32(fileBuffer, optHeaderWinSpecOffset);
        if(debugPrintOptSpecHeader) printOptHeaderWinSpec32(optHeaderWinSpec);
        nextSectionFirstByteAddr = optHeaderWinSpecOffset + sizeOfOptionalHeaderWindowsSpecificFields32;
    } else if(optionalStandard.Magic == OPT_HEADR_MAGIC_PE_PLUS) {
        // pe32+
        u32 optHeaderWinSpecOffset = nextSectionFirstByteAddr;
        OptionalHeaderWindowsSpecificFields64 optHeaderWinSpec = readOptHeaderWinSpec64(fileBuffer, optHeaderWinSpecOffset);
        if(debugPrintOptSpecHeader) printOptHeaderWinSpec64(optHeaderWinSpec);
        nextSectionFirstByteAddr = optHeaderWinSpecOffset + sizeOfOptionalHeaderWindowsSpecificFields64;
    } else {
        printf("optionalHeaderMagic is incorrect: 0x%x\n", optionalStandard.Magic);
        return 1;
    }

    // Optional Data Directory Header
    u32 optDataHeaderOffset = nextSectionFirstByteAddr;
    OptionalHeaderDataDirectories optDataHeader = readOptDataHeader(fileBuffer, optDataHeaderOffset);
    if(debugPrintOptDataHeader) printOptDataHeader(optDataHeader);
    nextSectionFirstByteAddr = optDataHeaderOffset + sizeOfOptionalHeaderDataDirectories;

    // Section Table
    // u32 sectionTableOffset = nextSectionFirstByteAddr;
    u32 sectionTableOffset = (peHeaderOffset + sizeOfPeHeader) + peHeader.SizeOfOptionalHeader;
    SectionTable sectionTable = readSectionTable(fileBuffer, sectionTableOffset, peHeader);
    if(debugPrintSectionTable) printSectionTable(sectionTable);
    nextSectionFirstByteAddr = sectionTableOffset + (sizeOfSectionTableEntry * sectionTable.count);

    if(dumpfilepath){
        u8* codeBuffer = fileBuffer + sectionTable.entries[0].PointerToRawData;
        FILE* f = fopen(dumpfilepath, "wb");
        assert(f && "Failed to open file to dump data");
        fwrite(codeBuffer, sizeof(u8), sectionTable.entries[0].SizeOfRawData, f);
        fclose(f);
    }

    // Read the section data, just to mark the sections as read
    for(int h = 0; h < sectionTable.entries[0].SizeOfRawData; h++) Read8(fileBuffer, sectionTable.entries[0].PointerToRawData + h);
    for(int h = 0; h < sectionTable.entries[1].SizeOfRawData; h++) Read8(fileBuffer, sectionTable.entries[1].PointerToRawData + h);
    for(int j = 10; j < sectionTable.count; j++) for(int h = 0; h < sectionTable.entries[j].SizeOfRawData; h++) Read8(fileBuffer, sectionTable.entries[j].PointerToRawData + h);

    // Symbol Table
    SymbolTable symbolTable = readSymbolTable(fileBuffer, peHeader);
    if(debugPrintSymbolTable) printSymbolTable(symbolTable);
    // NOTE: this isnt necesseceraly correct, because the symbol table doesnt have to follow the previous sections
    nextSectionFirstByteAddr = symbolTable.SymbolTableOffset + (symbolTable.count * sizeOfSymbolTableEntry);

    // Strings table
    int stringTableOffset = peHeader.PointerToSymbolTable + sizeOfSymbolTableEntry * peHeader.NumberOfSymbols;
    StringTable stringTable = readStringTable(fileBuffer, stringTableOffset, &nextSectionFirstByteAddr);
    if(debugPrintStringTable) printStringTable(stringTable, stringTableLimit == -1 ? INT32_MAX : stringTableLimit);

    // .rdata common info
    u32 importDirectoryTableOffset = 0;
    u32 importTableSize = 0;
    int importSectionIndex = 0;
    for(int i = 0; i < peHeader.NumberOfSections; i++){
        if(sectionTable.entries[i].VirtualAddress == optDataHeader.ImportTable.addr) {
            importDirectoryTableOffset = sectionTable.entries[i].PointerToRawData;
            importTableSize = sectionTable.entries[i].SizeOfRawData;
            importSectionIndex = i;
        }
    }
    
    // Import Directory Table
    ImportDirectoryTable importDirectoryTable = readImportTableDirectory(fileBuffer, importDirectoryTableOffset, importTableSize, &nextSectionFirstByteAddr);
    if(debugPrintImportDirectoryTable) printImportDirectoryTable(importDirectoryTable, fileBuffer, sectionTable.entries[importSectionIndex]);

    // Import Lookup Table
    for(int i = 0; i < importDirectoryTable.count; i++) {
        SectionTableEntry section = sectionTable.entries[importSectionIndex];
        int offset = section.PointerToRawData + importDirectoryTable.entries[i].ImportLookupTableRVA - section.VirtualAddress;
        
        int importLookupTableOffset = offset;
        int bytesPerEntry = optionalStandard.Magic == OPT_HEADR_MAGIC_PE ? 4 : 8;
        ImportLookupTable lookupTable = readImportLookupTable(fileBuffer, importLookupTableOffset, bytesPerEntry, section, &nextSectionFirstByteAddr);
        if(debugPrintImportLookupTable) printImportLookupTable(lookupTable);
    }

    printf("Number of bytes read: %i/%i (%.2f%%)\n", bytesRead, fileLen, ((float)bytesRead / (float)fileLen) * 100);
    return 0;
}

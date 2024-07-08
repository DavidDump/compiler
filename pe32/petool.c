#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef struct PEHeader {
    u16 Machine;
    u16 NumberOfSections;
    u32 TimeDateStamp;
    u32 PointerToSymbolTable;
    u32 NumberOfSymbols;
    u16 SizeOfOptionalHeader;
    u16 Characteristics;
} PEHeader;

typedef struct OptionalHeaderStandardFields {
    u16 Magic;
    u8 MajorLinkerVersion;
    u8 MinorLinkerVersion;
    u32 SizeOfCode;
    u32 SizeOfInitializedData;
    u32 SizeOfUninitializedData;
    u32 AddressOfEntryPoint;
    u32 BaseOfCode;

    // only if pe32, not present on pe32+
    u32 BaseOfData;
} OptionalHeaderStandardFields;

typedef struct OptionalHeaderWindowsSpecificFields64 {
    u64 ImageBase;
    u32 SectionAlignment;
    u32 FileAlignment;
    u16 MajorOperatingSystemVersion;
    u16 MinorOperatingSystemVersion;
    u16 MajorImageVersion;
    u16 MinorImageVersion;
    u16 MajorSubsystemVersion;
    u16 MinorSubsystemVersion;
    u32 Win32VersionValue;
    u32 SizeOfImage;
    u32 SizeOfHeaders;
    u32 CheckSum;
    u16 Subsystem;
    u16 DllCharacteristics;
    u64 SizeOfStackReserve;
    u64 SizeOfStackCommit;
    u64 SizeOfHeapReserve;
    u64 SizeOfHeapCommit;
    u32 LoaderFlags;
    u32 NumberOfRvaAndSizes;
} OptionalHeaderWindowsSpecificFields64;

typedef struct OptionalHeaderWindowsSpecificFields32 {
    u32 ImageBase;
    u32 SectionAlignment;
    u32 FileAlignment;
    u16 MajorOperatingSystemVersion;
    u16 MinorOperatingSystemVersion;
    u16 MajorImageVersion;
    u16 MinorImageVersion;
    u16 MajorSubsystemVersion;
    u16 MinorSubsystemVersion;
    u32 Win32VersionValue;
    u32 SizeOfImage;
    u32 SizeOfHeaders;
    u32 CheckSum;
    u16 Subsystem;
    u16 DllCharacteristics;
    u32 SizeOfStackReserve;
    u32 SizeOfStackCommit;
    u32 SizeOfHeapReserve;
    u32 SizeOfHeapCommit;
    u32 LoaderFlags;
    u32 NumberOfRvaAndSizes;
} OptionalHeaderWindowsSpecificFields32;

typedef struct DataDirectory {
    u32 addr;
    u32 size;
} DataDirectory;

typedef struct OptionalHeaderDataDirectories {
    DataDirectory ExportTable;
    DataDirectory ImportTable;
    DataDirectory ResourceTable;
    DataDirectory ExceptionTable;
    DataDirectory CertificateTable;
    DataDirectory BaseRelocationTable;
    DataDirectory Debug;
    DataDirectory Architecture;
    DataDirectory GlobalPtr;
    DataDirectory TLSTable;
    DataDirectory LoadConfigTable;
    DataDirectory BoundImport;
    DataDirectory IAT;
    DataDirectory DelayImportDescriptor;
    DataDirectory CLRRuntimeHeader;
    DataDirectory Reserved;
} OptionalHeaderDataDirectories;

typedef struct SectionTable {
    union {
        u64 as_u64;
        u8 as_u8s[8];
    } Name;
    u32 VirtualSize;
    u32 VirtualAddress;
    u32 SizeOfRawData;
    u32 PointerToRawData;
    u32 PointerToRelocations;
    u32 PointerToLinenumbers;
    u16 NumberOfRelocations;
    u16 NumberOfLinenumbers;
    u32 Characteristics;
} SectionTable;

typedef struct SymbolTableEntry {
    union {
        u64 as_u64;
        u8 as_u8s[8];
    } Name;
    u32 Value;
    u16 SectionNumber;
    u16 Type;
    u8 StorageClass;
    u8 NumberOfAuxSymbol;
} SymbolTableEntry;

typedef struct ImportDirectoryTableEntry {
    u32 ImportLookupTableRVA;
    u32 DateTimeStamp;
    u32 ForwarderChain;
    u32 NameRVA;
    u32 ImportAddressTableRVA;
} ImportDirectoryTableEntry;

typedef struct ImportLookupTableEntry32 {
    // NameFlag;
    // OrdinalNumber;
    // NameTableRVA;
    u32 data;
} ImportLookupTableEntry32;

typedef struct ImportLookupTableEntry64 {
    // NameFlag;
    // OrdinalNumber;
    // NameTableRVA;
    u64 data;
} ImportLookupTableEntry64;

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

int bytesRead = 0;

u8 Read8(u8* buffer, u64 offset) {
    u8 result = 0;
    
    result |= (buffer[offset + 0] << (8 * 0));

    bytesRead += 1;
    return result;
}

u16 Read16(u8* buffer, u64 offset) {
    u16 result = 0;
    
    result |= (buffer[offset + 0] << (8 * 0));
    result |= (buffer[offset + 1] << (8 * 1));

    bytesRead += 2;
    return result;
}

u32 Read32(u8* buffer, u64 offset) {
    u32 result = 0;
    
    result |= (buffer[offset + 0] << (8 * 0));
    result |= (buffer[offset + 1] << (8 * 1));
    result |= (buffer[offset + 2] << (8 * 2));
    result |= (buffer[offset + 3] << (8 * 3));

    bytesRead += 4;
    return result;
}

u64 Read64(u8* buffer, u64 offset) {
    u64 result = 0;
    
    result |= ((u64)buffer[offset + 0] << (8 * 0));
    result |= ((u64)buffer[offset + 1] << (8 * 1));
    result |= ((u64)buffer[offset + 2] << (8 * 2));
    result |= ((u64)buffer[offset + 3] << (8 * 3));
    result |= ((u64)buffer[offset + 4] << (8 * 4));
    result |= ((u64)buffer[offset + 5] << (8 * 5));
    result |= ((u64)buffer[offset + 6] << (8 * 6));
    result |= ((u64)buffer[offset + 7] << (8 * 7));

    bytesRead += 8;
    return result;
}

int FindHowManyNonNullBytes(u64 data) {
    int result = 0;
    u8 tmp = 0;
    for(int i = 0; i < 8; i++){
        tmp = ((data >> (8 * i)) & 0xff);
        if(tmp == 0) result++;
    }
    return result;
}

int FindHowManyHexDigits(u32 data) {
    int result = 1;
    while(data >> 4 != 0) {
        data >>= 4;
        result++;
    }
    return result;
}

void usage(int argc, char** argv) {
    printf("PE Tool v0.1\n");
    printf("usage: %s <read|write> [options] <filepath>\n", argv[0]);
    printf("    read mode: read the PE file contents and print the info to the screen\n");
    printf("        read options:\n");
    printf("        -o <filepath>  Output the read .text segment to the specified file\n");
    printf("        -strings       Print out the strings stored in the string table of the file\n");
    printf("    write mode: use bytecode data to create a PE32 file\n");
}

int main(int argc, char** argv) {
    if(argc < 3) {
        usage(argc, argv);
        return 1;
    }

    int read = 0;
    int write = 0;
    int printStrings = 0;
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
            }else if(strcmp(argv[i], "-strings") == 0) {
                printStrings = 1;
                i++;
            } else if(argv[i][0] == '-'){
                printf("[ERROR] Unkown argument: %s\n", argv[i]);
                return 1;
            }

            filename = argv[i];
            break;
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

    PEHeader peHeader = {0};
    peHeader.Machine              = Read16(fileBuffer, peHeaderOffset + 0);
    peHeader.NumberOfSections     = Read16(fileBuffer, peHeaderOffset + 2);
    peHeader.TimeDateStamp        = Read32(fileBuffer, peHeaderOffset + 4);
    peHeader.PointerToSymbolTable = Read32(fileBuffer, peHeaderOffset + 8);
    peHeader.NumberOfSymbols      = Read32(fileBuffer, peHeaderOffset + 12);
    peHeader.SizeOfOptionalHeader = Read16(fileBuffer, peHeaderOffset + 16);
    peHeader.Characteristics      = Read16(fileBuffer, peHeaderOffset + 18);

    nextSectionFirstByteAddr = peHeaderOffset + 18;
    nextSectionFirstByteAddr += 2;

    printf("----------------- PE Header (0x%x) -----------------\n", peHeaderOffset);
    printf("Machine:              0x%x\n", peHeader.Machine);
    printf("NumberOfSections:     %i\n", peHeader.NumberOfSections);
    printf("TimeDateStamp:        %i\n", peHeader.TimeDateStamp);
    printf("PointerToSymbolTable: 0x%x\n", peHeader.PointerToSymbolTable);
    printf("NumberOfSymbols:      %i\n", peHeader.NumberOfSymbols);
    printf("SizeOfOptionalHeader: %i\n", peHeader.SizeOfOptionalHeader);
    printf("Characteristics:      0x%x\n", peHeader.Characteristics);

    u32 optionalStandardOffset = nextSectionFirstByteAddr;
    OptionalHeaderStandardFields optionalStandard = {0};
    optionalStandard.Magic =                   Read16(fileBuffer, optionalStandardOffset + 0);
    optionalStandard.MajorLinkerVersion =      Read8(fileBuffer,  optionalStandardOffset + 2);
    optionalStandard.MinorLinkerVersion =      Read8(fileBuffer,  optionalStandardOffset + 3);
    optionalStandard.SizeOfCode =              Read32(fileBuffer, optionalStandardOffset + 4);
    optionalStandard.SizeOfInitializedData =   Read32(fileBuffer, optionalStandardOffset + 8);
    optionalStandard.SizeOfUninitializedData = Read32(fileBuffer, optionalStandardOffset + 12);
    optionalStandard.AddressOfEntryPoint =     Read32(fileBuffer, optionalStandardOffset + 16);
    optionalStandard.BaseOfCode =              Read32(fileBuffer, optionalStandardOffset + 20);

    nextSectionFirstByteAddr = optionalStandardOffset + 20;
    nextSectionFirstByteAddr += 4;

    char* versionStr = NULL;
    if(optionalStandard.Magic == 0x10b) {
        versionStr = "pe32";
        optionalStandard.BaseOfData = Read32(fileBuffer, optionalStandardOffset + 24);
        nextSectionFirstByteAddr += 4;
    } else if(optionalStandard.Magic == 0x20b) {
        versionStr = "pe32+";
    } else {
        printf("optionalHeaderMagic is incorrect: 0x%x\n", optionalStandard.Magic);
        return 1;
    }

    // TODO: the optional fields are not guaranteed, so some check to make sure they are present
    printf("------------- Optional Header Standard Fields (0x%x) -------------\n", optionalStandardOffset);
    printf("Magic:                   0x%x (%s)\n", optionalStandard.Magic, versionStr);
    printf("MajorLinkerVersion:      %u\n", optionalStandard.MajorLinkerVersion);
    printf("MinorLinkerVersion:      %u\n", optionalStandard.MinorLinkerVersion);
    printf("SizeOfCode:              %i\n", optionalStandard.SizeOfCode);
    printf("SizeOfInitializedData:   %i\n", optionalStandard.SizeOfInitializedData);
    printf("SizeOfUninitializedData: %i\n", optionalStandard.SizeOfUninitializedData);
    printf("AddressOfEntryPoint:     0x%x\n", optionalStandard.AddressOfEntryPoint);
    printf("BaseOfCode:              0x%x\n", optionalStandard.BaseOfCode);
    printf("BaseOfData:              0x%x\n", optionalStandard.BaseOfData);

    if(optionalStandard.Magic == 0x10b) {
        // pe32
        OptionalHeaderWindowsSpecificFields32 optionalSpecific = {0};
        optionalSpecific.ImageBase =                   Read32(fileBuffer, optionalStandardOffset + 28);
        optionalSpecific.SectionAlignment =            Read32(fileBuffer, optionalStandardOffset + 32);
        optionalSpecific.FileAlignment =               Read32(fileBuffer, optionalStandardOffset + 36);
        optionalSpecific.MajorOperatingSystemVersion = Read16(fileBuffer, optionalStandardOffset + 40);
        optionalSpecific.MinorOperatingSystemVersion = Read16(fileBuffer, optionalStandardOffset + 42);
        optionalSpecific.MajorImageVersion =           Read16(fileBuffer, optionalStandardOffset + 44);
        optionalSpecific.MinorImageVersion =           Read16(fileBuffer, optionalStandardOffset + 46);
        optionalSpecific.MajorSubsystemVersion =       Read16(fileBuffer, optionalStandardOffset + 48);
        optionalSpecific.MinorSubsystemVersion =       Read16(fileBuffer, optionalStandardOffset + 50);
        optionalSpecific.Win32VersionValue =           Read32(fileBuffer, optionalStandardOffset + 52);
        optionalSpecific.SizeOfImage =                 Read32(fileBuffer, optionalStandardOffset + 56);
        optionalSpecific.SizeOfHeaders =               Read32(fileBuffer, optionalStandardOffset + 60);
        optionalSpecific.CheckSum =                    Read32(fileBuffer, optionalStandardOffset + 64);
        optionalSpecific.Subsystem =                   Read16(fileBuffer, optionalStandardOffset + 68);
        optionalSpecific.DllCharacteristics =          Read16(fileBuffer, optionalStandardOffset + 70);
        optionalSpecific.SizeOfStackReserve =          Read32(fileBuffer, optionalStandardOffset + 72);
        optionalSpecific.SizeOfStackCommit =           Read32(fileBuffer, optionalStandardOffset + 76);
        optionalSpecific.SizeOfHeapReserve =           Read32(fileBuffer, optionalStandardOffset + 80);
        optionalSpecific.SizeOfHeapCommit =            Read32(fileBuffer, optionalStandardOffset + 84);
        optionalSpecific.LoaderFlags =                 Read32(fileBuffer, optionalStandardOffset + 88);
        optionalSpecific.NumberOfRvaAndSizes =         Read32(fileBuffer, optionalStandardOffset + 92);

        nextSectionFirstByteAddr = optionalStandardOffset + 92;
        nextSectionFirstByteAddr += 4;
    } else if(optionalStandard.Magic == 0x20b) {
        // pe32+
        OptionalHeaderWindowsSpecificFields64 optionalSpecific = {0};
        optionalSpecific.ImageBase =                   Read64(fileBuffer, optionalStandardOffset + 24);
        optionalSpecific.SectionAlignment =            Read32(fileBuffer, optionalStandardOffset + 32);
        optionalSpecific.FileAlignment =               Read32(fileBuffer, optionalStandardOffset + 36);
        optionalSpecific.MajorOperatingSystemVersion = Read16(fileBuffer, optionalStandardOffset + 40);
        optionalSpecific.MinorOperatingSystemVersion = Read16(fileBuffer, optionalStandardOffset + 42);
        optionalSpecific.MajorImageVersion =           Read16(fileBuffer, optionalStandardOffset + 44);
        optionalSpecific.MinorImageVersion =           Read16(fileBuffer, optionalStandardOffset + 46);
        optionalSpecific.MajorSubsystemVersion =       Read16(fileBuffer, optionalStandardOffset + 48);
        optionalSpecific.MinorSubsystemVersion =       Read16(fileBuffer, optionalStandardOffset + 50);
        optionalSpecific.Win32VersionValue =           Read32(fileBuffer, optionalStandardOffset + 52);
        optionalSpecific.SizeOfImage =                 Read32(fileBuffer, optionalStandardOffset + 56);
        optionalSpecific.SizeOfHeaders =               Read32(fileBuffer, optionalStandardOffset + 60);
        optionalSpecific.CheckSum =                    Read32(fileBuffer, optionalStandardOffset + 64);
        optionalSpecific.Subsystem =                   Read16(fileBuffer, optionalStandardOffset + 68);
        optionalSpecific.DllCharacteristics =          Read16(fileBuffer, optionalStandardOffset + 70);
        optionalSpecific.SizeOfStackReserve =          Read64(fileBuffer, optionalStandardOffset + 72);
        optionalSpecific.SizeOfStackCommit =           Read64(fileBuffer, optionalStandardOffset + 80);
        optionalSpecific.SizeOfHeapReserve =           Read64(fileBuffer, optionalStandardOffset + 88);
        optionalSpecific.SizeOfHeapCommit =            Read64(fileBuffer, optionalStandardOffset + 96);
        optionalSpecific.LoaderFlags =                 Read32(fileBuffer, optionalStandardOffset + 104);
        optionalSpecific.NumberOfRvaAndSizes =         Read32(fileBuffer, optionalStandardOffset + 108);

        nextSectionFirstByteAddr = optionalStandardOffset + 108;
        nextSectionFirstByteAddr += 4;

        printf("--------------- Optional Header Windows-Specific Fields (0x%x) ---------------\n", optionalStandardOffset + 24);
        printf("ImageBase:                   0x%x\n", optionalSpecific.ImageBase);
        printf("SectionAlignment:            %i\n", optionalSpecific.SectionAlignment);
        printf("FileAlignment:               %i\n", optionalSpecific.FileAlignment);
        printf("MajorOperatingSystemVersion: %i\n", optionalSpecific.MajorOperatingSystemVersion);
        printf("MinorOperatingSystemVersion: %i\n", optionalSpecific.MinorOperatingSystemVersion);
        printf("MajorImageVersion:           %i\n", optionalSpecific.MajorImageVersion);
        printf("MinorImageVersion:           %i\n", optionalSpecific.MinorImageVersion);
        printf("MajorSubsystemVersion:       %i\n", optionalSpecific.MajorSubsystemVersion);
        printf("MinorSubsystemVersion:       %i\n", optionalSpecific.MinorSubsystemVersion);
        printf("Win32VersionValue:           %i\n", optionalSpecific.Win32VersionValue);
        printf("SizeOfImage:                 %i\n", optionalSpecific.SizeOfImage);
        printf("SizeOfHeaders:               %i\n", optionalSpecific.SizeOfHeaders);
        printf("CheckSum:                    %i\n", optionalSpecific.CheckSum);
        printf("Subsystem:                   %i\n", optionalSpecific.Subsystem);
        printf("DllCharacteristics:          0x%x\n", optionalSpecific.DllCharacteristics);
        printf("SizeOfStackReserve:          %i\n", optionalSpecific.SizeOfStackReserve);
        printf("SizeOfStackCommit:           %i\n", optionalSpecific.SizeOfStackCommit);
        printf("SizeOfHeapReserve:           %i\n", optionalSpecific.SizeOfHeapReserve);
        printf("SizeOfHeapCommit:            %i\n", optionalSpecific.SizeOfHeapCommit);
        printf("LoaderFlags:                 %i\n", optionalSpecific.LoaderFlags);
        printf("NumberOfRvaAndSizes:         %i\n", optionalSpecific.NumberOfRvaAndSizes);
    } else {
        printf("optionalHeaderMagic is incorrect: 0x%x\n", optionalStandard.Magic);
        return 1;
    }

    OptionalHeaderDataDirectories optionalDataDirs = {0};
    optionalDataDirs.ExportTable.addr           = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 112 + 0 : 96 + 0));
    optionalDataDirs.ExportTable.size           = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 112 + 4 : 96 + 4));
    optionalDataDirs.ImportTable.addr           = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 120 + 0 : 104 + 0));
    optionalDataDirs.ImportTable.size           = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 120 + 4 : 104 + 4));
    optionalDataDirs.ResourceTable.addr         = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 128 + 0 : 112 + 0));
    optionalDataDirs.ResourceTable.size         = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 128 + 4 : 112 + 4));
    optionalDataDirs.ExceptionTable.addr        = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 136 + 0 : 120 + 0));
    optionalDataDirs.ExceptionTable.size        = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 136 + 4 : 120 + 4));
    optionalDataDirs.CertificateTable.addr      = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 144 + 0 : 128 + 0));
    optionalDataDirs.CertificateTable.size      = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 144 + 4 : 128 + 4));
    optionalDataDirs.BaseRelocationTable.addr   = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 152 + 0 : 136 + 0));
    optionalDataDirs.BaseRelocationTable.size   = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 152 + 4 : 136 + 4));
    optionalDataDirs.Debug.addr                 = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 160 + 0 : 144 + 0));
    optionalDataDirs.Debug.size                 = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 160 + 4 : 144 + 4));
    optionalDataDirs.Architecture.addr          = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 168 + 0 : 152 + 0));
    optionalDataDirs.Architecture.size          = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 168 + 4 : 152 + 4));
    optionalDataDirs.GlobalPtr.addr             = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 176 + 0 : 160 + 0));
    optionalDataDirs.GlobalPtr.size             = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 176 + 4 : 160 + 4));
    optionalDataDirs.TLSTable.addr              = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 184 + 0 : 168 + 0));
    optionalDataDirs.TLSTable.size              = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 184 + 4 : 168 + 4));
    optionalDataDirs.LoadConfigTable.addr       = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 192 + 0 : 176 + 0));
    optionalDataDirs.LoadConfigTable.size       = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 192 + 4 : 176 + 4));
    optionalDataDirs.BoundImport.addr           = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 200 + 0 : 184 + 0));
    optionalDataDirs.BoundImport.size           = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 200 + 4 : 184 + 4));
    optionalDataDirs.IAT.addr                   = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 208 + 0 : 192 + 0));
    optionalDataDirs.IAT.size                   = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 208 + 4 : 192 + 4));
    optionalDataDirs.DelayImportDescriptor.addr = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 216 + 0 : 200 + 0));
    optionalDataDirs.DelayImportDescriptor.size = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 216 + 4 : 200 + 4));
    optionalDataDirs.CLRRuntimeHeader.addr      = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 224 + 0 : 208 + 0));
    optionalDataDirs.CLRRuntimeHeader.size      = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 224 + 4 : 208 + 4));
    optionalDataDirs.Reserved.addr              = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 232 + 0 : 216 + 0));
    optionalDataDirs.Reserved.size              = Read32(fileBuffer, optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 232 + 4 : 216 + 4));

    nextSectionFirstByteAddr = optionalStandardOffset + (optionalStandard.Magic == 0x20b ? 232 + 4 : 216 + 4);
    nextSectionFirstByteAddr += 4;

    printf("---------------- Optional Header Data Directories (0x%x) ----------------\n", optionalStandard.Magic == 0x20b ? 112 + 0 : 96 + 0);
    printf("ExportTable:           (addr: 0x%x, size: %i)\n", optionalDataDirs.ExportTable.addr, optionalDataDirs.ExportTable.size);
    printf("ImportTable:           (addr: 0x%x, size: %i)\n", optionalDataDirs.ImportTable.addr, optionalDataDirs.ImportTable.size);
    printf("ResourceTable:         (addr: 0x%x, size: %i)\n", optionalDataDirs.ResourceTable.addr, optionalDataDirs.ResourceTable.size);
    printf("ExceptionTable:        (addr: 0x%x, size: %i)\n", optionalDataDirs.ExceptionTable.addr, optionalDataDirs.ExceptionTable.size);
    printf("CertificateTable:      (addr: 0x%x, size: %i)\n", optionalDataDirs.CertificateTable.addr, optionalDataDirs.CertificateTable.size);
    printf("BaseRelocationTable:   (addr: 0x%x, size: %i)\n", optionalDataDirs.BaseRelocationTable.addr, optionalDataDirs.BaseRelocationTable.size);
    printf("Debug:                 (addr: 0x%x, size: %i)\n", optionalDataDirs.Debug.addr, optionalDataDirs.Debug.size);
    printf("Architecture:          (addr: 0x%x, size: %i)\n", optionalDataDirs.Architecture.addr, optionalDataDirs.Architecture.size);
    printf("GlobalPtr:             (addr: 0x%x, size: %i)\n", optionalDataDirs.GlobalPtr.addr, optionalDataDirs.GlobalPtr.size);
    printf("TLSTable:              (addr: 0x%x, size: %i)\n", optionalDataDirs.TLSTable.addr, optionalDataDirs.TLSTable.size);
    printf("LoadConfigTable:       (addr: 0x%x, size: %i)\n", optionalDataDirs.LoadConfigTable.addr, optionalDataDirs.LoadConfigTable.size);
    printf("BoundImport:           (addr: 0x%x, size: %i)\n", optionalDataDirs.BoundImport.addr, optionalDataDirs.BoundImport.size);
    printf("IAT:                   (addr: 0x%x, size: %i)\n", optionalDataDirs.IAT.addr, optionalDataDirs.IAT.size);
    printf("DelayImportDescriptor: (addr: 0x%x, size: %i)\n", optionalDataDirs.DelayImportDescriptor.addr, optionalDataDirs.DelayImportDescriptor.size);
    printf("CLRRuntimeHeader:      (addr: 0x%x, size: %i)\n", optionalDataDirs.CLRRuntimeHeader.addr, optionalDataDirs.CLRRuntimeHeader.size);
    printf("Reserved:              (addr: 0x%x, size: %i)\n", optionalDataDirs.Reserved.addr, optionalDataDirs.Reserved.size);

    #if 0 // NOTE: Either way of finding the sections works
    int sectionTableOffset = nextSectionFirstByteAddr;
    #else
    int sectionTableOffset = (peHeaderOffset + 20) + peHeader.SizeOfOptionalHeader;
    #endif
    SectionTable* sections = malloc(peHeader.NumberOfSections * sizeof(*sections));
    printf("------------------------ Section Table (0x%x) ------------------------\n", sectionTableOffset);
    printf("  name  | VirtualSize | VirtualAddress | SizeOfRawData | PointerToRawData | Characteristics\n");
    for (int i = 0; i < peHeader.NumberOfSections; i++) {
        sections[i].Name.as_u64          = Read64(fileBuffer, sectionTableOffset + i * sizeof(*sections) + 0);
        sections[i].VirtualSize          = Read32(fileBuffer, sectionTableOffset + i * sizeof(*sections) + 8);
        sections[i].VirtualAddress       = Read32(fileBuffer, sectionTableOffset + i * sizeof(*sections) + 12);
        sections[i].SizeOfRawData        = Read32(fileBuffer, sectionTableOffset + i * sizeof(*sections) + 16);
        sections[i].PointerToRawData     = Read32(fileBuffer, sectionTableOffset + i * sizeof(*sections) + 20);
        sections[i].PointerToRelocations = Read32(fileBuffer, sectionTableOffset + i * sizeof(*sections) + 24);
        sections[i].PointerToLinenumbers = Read32(fileBuffer, sectionTableOffset + i * sizeof(*sections) + 28);
        sections[i].NumberOfRelocations  = Read16(fileBuffer, sectionTableOffset + i * sizeof(*sections) + 32);
        sections[i].NumberOfLinenumbers  = Read16(fileBuffer, sectionTableOffset + i * sizeof(*sections) + 34);
        sections[i].Characteristics      = Read32(fileBuffer, sectionTableOffset + i * sizeof(*sections) + 36);

        nextSectionFirstByteAddr = sectionTableOffset + i * sizeof(*sections) + 36;
        nextSectionFirstByteAddr += 4;

        int collumWidth = 8;
        printf("%.8s", sections[i].Name.as_u8s);
        for(int h = 0; h < collumWidth - (8 - FindHowManyNonNullBytes(sections[i].Name.as_u64)); h++) printf(" ");
        printf("|");

        collumWidth = 13;
        printf(" 0x%x", sections[i].VirtualSize);
        for(int h = 0; h < (collumWidth - 3) - FindHowManyHexDigits(sections[i].VirtualSize); h++) printf(" ");
        printf("|");
        
        collumWidth = 16;
        printf(" 0x%x", sections[i].VirtualAddress);
        for(int h = 0; h < (collumWidth - 3) - FindHowManyHexDigits(sections[i].VirtualAddress); h++) printf(" ");
        printf("|");
        
        collumWidth = 15;
        printf(" 0x%x", sections[i].SizeOfRawData);
        for(int h = 0; h < (collumWidth - 3) - FindHowManyHexDigits(sections[i].SizeOfRawData); h++) printf(" ");
        printf("|");
        
        collumWidth = 18;
        printf(" 0x%x", sections[i].PointerToRawData);
        for(int h = 0; h < (collumWidth - 3) - FindHowManyHexDigits(sections[i].PointerToRawData); h++) printf(" ");
        printf("|");
        
        collumWidth = 17;
        printf(" 0x%x", sections[i].Characteristics);
        for(int h = 0; h < (collumWidth - 3) - FindHowManyHexDigits(sections[i].Characteristics); h++) printf(" ");
        printf("|");
        printf("\n");
    }
    
    if(dumpfilepath){
        u8* codeBuffer = fileBuffer + sections[0].PointerToRawData;
        FILE* f = fopen(dumpfilepath, "wb");
        assert(f && "Failed to open file to dump data");
        fwrite(codeBuffer, sizeof(u8), sections[0].SizeOfRawData, f);
        fclose(f);
    }

    // Read the section data, just to mark the sections as read
    // for(int i = 0; i < peHeader.NumberOfSections; i++){
    //     for(int h = 0; h < sections[i].SizeOfRawData; h++){
    //         Read8(fileBuffer, sections[i].PointerToRawData + h);
    //     }
    // }

    int sizeofSymbolTableEntry = 18;
    assert(sizeof(SymbolTableEntry) == 24 && "The struct is packed so i dont want to use sizeof() so i just hardcoded it");
    
    int symbolTableOffset = peHeader.PointerToSymbolTable;
    SymbolTableEntry* symbolTable = malloc(peHeader.NumberOfSymbols * sizeof(*symbolTable));
    printf("------------------ Symbol Table (0x%x) ------------------\n", symbolTableOffset);
    for(int i = 0; i < peHeader.NumberOfSymbols; i++){
        symbolTable[i].Name.as_u64       = Read64(fileBuffer, symbolTableOffset + i * sizeofSymbolTableEntry + 0);
        symbolTable[i].Value             = Read32(fileBuffer, symbolTableOffset + i * sizeofSymbolTableEntry + 8);
        symbolTable[i].SectionNumber     = Read16(fileBuffer, symbolTableOffset + i * sizeofSymbolTableEntry + 12);
        symbolTable[i].Type              = Read16(fileBuffer, symbolTableOffset + i * sizeofSymbolTableEntry + 14);
        symbolTable[i].StorageClass      = Read8(fileBuffer, symbolTableOffset + i * sizeofSymbolTableEntry + 16);
        symbolTable[i].NumberOfAuxSymbol = Read8(fileBuffer, symbolTableOffset + i * sizeofSymbolTableEntry + 17);

        nextSectionFirstByteAddr = symbolTableOffset + i * sizeofSymbolTableEntry + 17;
        nextSectionFirstByteAddr += 1;

        if(i == 0){
            printf("Name:              %.8s (0x%x)\n", symbolTable[i].Name.as_u8s, symbolTable[i].Name.as_u64);
            printf("Value:             0x%x\n", symbolTable[i].Value);
            printf("SectionNumber:     0x%x\n", symbolTable[i].SectionNumber);
            printf("Type:              0x%x\n", symbolTable[i].Type);
            printf("StorageClass:      0x%x\n", symbolTable[i].StorageClass);
            printf("NumberOfAuxSymbol: 0x%x\n", symbolTable[i].NumberOfAuxSymbol);
        }
    }

    printf("calculated strings table addr: 0x%x\n", peHeader.PointerToSymbolTable + sizeofSymbolTableEntry * peHeader.NumberOfSymbols);

    int stringTableOffset = peHeader.PointerToSymbolTable + sizeofSymbolTableEntry * peHeader.NumberOfSymbols;
    u32 sizeofStringTable = Read32(fileBuffer, stringTableOffset);
    u8** strings = NULL;
    printf("sizeofStringTable: %i\n", sizeofStringTable);
    int bytesReadStr = 0;
    if(sizeofStringTable != 4) {
        u8* stringTable = (u8*)fileBuffer + stringTableOffset;
        stringTable += 4;
        bytesReadStr += 4;
        
        int i;
        for(i = 0; bytesReadStr < sizeofStringTable; i++){
            if(printStrings) printf("[%i]: %s\n", i, stringTable);
            int len = strlen(stringTable);
            stringTable += len + 1;
            bytesReadStr += len + 1;
            bytesRead += len + 1;
        }

        i++; // i stores index but we want count
        strings = malloc(i * sizeof(u8*));
        stringTable = (u8*)fileBuffer + stringTableOffset;
        stringTable += 4;
        bytesReadStr = 4;
        for(int h = 0; h < i; h++){
            strings[h] = stringTable;
            int len = strlen(stringTable);
            stringTable += len + 1;
            bytesReadStr += len + 1;
        }
    }

    // ImportDirectoryTableEntry

    int importDirectoryTableOffset = 0;
    int importTableSize = 0;
    for(int i = 0; i < peHeader.NumberOfSections; i++){
        if(sections[i].VirtualAddress == optionalDataDirs.ImportTable.addr) {
            importDirectoryTableOffset = sections[i].PointerToRawData;
            importTableSize = sections[i].SizeOfRawData;
        }
    }

    assert(sizeof(ImportDirectoryTableEntry) == 20 && "Make sure struct is not packed");
    int dirTableCount = 0;
    for(u8* addr = fileBuffer + importDirectoryTableOffset; addr < fileBuffer + importDirectoryTableOffset + importTableSize;){
        ImportDirectoryTableEntry tmp = {0};
        ImportDirectoryTableEntry zero = {0};
        memcpy(&tmp, addr, sizeof(tmp));
        if(memcmp(&tmp, &zero, sizeof(tmp)) == 0) break;
        dirTableCount++;
        addr += sizeof(tmp);
    }

    ImportDirectoryTableEntry* importDirectoryTable = malloc(dirTableCount * sizeof(*importDirectoryTable));
    printf("------------------ Import Directory Table (0x%x) ------------------\n", importDirectoryTableOffset);
    for(int i = 0; i < dirTableCount; i++){
        importDirectoryTable[i].ImportLookupTableRVA  = Read32(fileBuffer, importDirectoryTableOffset + i * sizeof(*importDirectoryTable) + 0);
        importDirectoryTable[i].DateTimeStamp         = Read32(fileBuffer, importDirectoryTableOffset + i * sizeof(*importDirectoryTable) + 4);
        importDirectoryTable[i].ForwarderChain        = Read32(fileBuffer, importDirectoryTableOffset + i * sizeof(*importDirectoryTable) + 8);
        importDirectoryTable[i].NameRVA               = Read32(fileBuffer, importDirectoryTableOffset + i * sizeof(*importDirectoryTable) + 12);
        importDirectoryTable[i].ImportAddressTableRVA = Read32(fileBuffer, importDirectoryTableOffset + i * sizeof(*importDirectoryTable) + 16);

        if(1){
            int nameAddr = importDirectoryTableOffset + (importDirectoryTable[i].NameRVA - optionalDataDirs.ImportTable.addr);
            printf("ImportLookupTableRVA:  0x%x\n", importDirectoryTable[i].ImportLookupTableRVA);
            printf("DateTimeStamp:         0x%x\n", importDirectoryTable[i].DateTimeStamp);
            printf("ForwarderChain:        0x%x\n", importDirectoryTable[i].ForwarderChain);
            printf("ImportAddressTableRVA: 0x%x\n", importDirectoryTable[i].ImportAddressTableRVA);
            if(importDirectoryTable[i].NameRVA){
                printf("NameRVA:               0x%x (%s)\n", importDirectoryTable[i].NameRVA, fileBuffer + nameAddr);
            } else {
                printf("NameRVA:               0x%x\n", importDirectoryTable[i].NameRVA);
            }
        }
    }
    nextSectionFirstByteAddr = importDirectoryTableOffset + (dirTableCount + 1) * sizeof(*importDirectoryTable);

    // ImportLookupTableEntry - this need to be repeated per directory table

    int importLookupTableOffset = nextSectionFirstByteAddr;
    printf("------------------- Import Lookup Table (0x%x) -------------------\n", importLookupTableOffset);
    if(optionalStandard.Magic == 0x10b) {
        // pe32
        assert(sizeof(ImportLookupTableEntry32) == 4 && "Make sure struct is not packed");
        int tableCount = 0;
        for(u8* addr = fileBuffer + importLookupTableOffset; addr < fileBuffer + importLookupTableOffset + importTableSize;){
            ImportLookupTableEntry32 tmp = {0};
            ImportLookupTableEntry32 zero = {0};
            memcpy(&tmp, addr, sizeof(tmp));
            if(memcmp(&tmp, &zero, sizeof(tmp)) == 0) break;
            tableCount++;
            addr += sizeof(tmp);
        }

        ImportLookupTableEntry32* importLookupTable32 = malloc(tableCount * sizeof(*importLookupTable32));
        for(int i = 0; i < tableCount; i++){
            importLookupTable32[i].data = Read32(fileBuffer, importLookupTableOffset + i * sizeof(*importLookupTable32));

            printf("[%i]: 0x%x\n", i, importLookupTable32[i].data);
        }

        nextSectionFirstByteAddr = importLookupTableOffset + (tableCount + 1) * sizeof(*importLookupTable32);
    }else if (optionalStandard.Magic == 0x20b){
        // pe32+
        assert(sizeof(ImportLookupTableEntry64) == 8 && "Make sure struct is not packed");
        int tableCount = 0;
        for(u8* addr = fileBuffer + importLookupTableOffset; addr < fileBuffer + importLookupTableOffset + importTableSize;){
            ImportLookupTableEntry64 tmp = {0};
            ImportLookupTableEntry64 zero = {0};
            memcpy(&tmp, addr, sizeof(tmp));
            if(memcmp(&tmp, &zero, sizeof(tmp)) == 0) break;
            tableCount++;
            addr += sizeof(tmp);
        }

        ImportLookupTableEntry64* importLookupTable64 = malloc(tableCount * sizeof(*importLookupTable64));
        for(int i = 0; i < tableCount; i++){
            importLookupTable64[i].data = Read64(fileBuffer, importLookupTableOffset + i * sizeof(*importLookupTable64));

            int nameFlag = importLookupTable64[i].data & 0x8000000000000000;
            int ordinal  = importLookupTable64[i].data & 0x000000000000FFFF;
            int name     = importLookupTable64[i].data & 0x000000007FFFFFFF; // pointer to the ascii name, used to lookup in the .dll file export table if the hint lookup fails
            int nameTableEntryAddr = importDirectoryTableOffset + (name - optionalDataDirs.ImportTable.addr);
            u16 hint = Read16(fileBuffer, nameTableEntryAddr); // hint is the index into the .dll file export table, used to lookup first
            printf("[%i]: ", i);
            printf("flag: %i, ", nameFlag);
            printf("ordinalNumber: %i, ", ordinal);
            printf("offset: 0x%x, ", name);
            printf("name: %s, ", fileBuffer + nameTableEntryAddr + 2);
            printf("hint: %i", hint);
            printf("\n");
        }

        nextSectionFirstByteAddr = importLookupTableOffset + (tableCount + 1) * sizeof(*importLookupTable64);
    }

    printf("Number of read bytes: %i/%i (%.2f%%)\n", bytesRead, fileLen, ((float)bytesRead / (float)fileLen) * 100);
    return 0;
}

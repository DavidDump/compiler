#include "common.h"

// 2 + 2 + 4 + 4 + 4 + 2 + 2
int sizeOfPeHeader = 20; // how much space this field takes up in the file
typedef struct PEHeader {
    u16 Machine;
    u16 NumberOfSections;
    u32 TimeDateStamp;
    u32 PointerToSymbolTable;
    u32 NumberOfSymbols;
    u16 SizeOfOptionalHeader;
    u16 Characteristics;

    u32 peHeaderOffset; // the offset to the begining of the header from the begining of the file
} PEHeader;

#define OPT_HEADR_MAGIC_PE      0x10b
#define OPT_HEADR_MAGIC_PE_PLUS 0x20b

// 2 + 1 + 1 + 4 + 4 + 4 + 4 + 4 + (4)
int sizeOfOptionalHeaderStandardFields = 24; // how much space this field takes up in the file
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

    u32 optionalStandardOffset; // the offset to the begining of the header from the begining of the file
} OptionalHeaderStandardFields;

// 8 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 4 + 2 + 2 + 8 + 8 + 8 + 8 + 4 + 4
int sizeOfOptionalHeaderWindowsSpecificFields64 = 88; // how much space this field takes up in the file
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

    u32 OptionalHeaderWindowsSpecificFields64Offset;
} OptionalHeaderWindowsSpecificFields64;

// 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 4 + 2 + 2 + 4 + 4 + 4 + 4 + 4 + 4
int sizeOfOptionalHeaderWindowsSpecificFields32 = 68; // how much space this field takes up in the file
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

    u32 OptionalHeaderWindowsSpecificFields32Offset;
} OptionalHeaderWindowsSpecificFields32;

typedef struct DataDirectory {
    u32 addr;
    u32 size;
} DataDirectory;

int sizeOfOptionalHeaderDataDirectories = 128;
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

    u32 OptionalHeaderDataDirectoriesOffset;
} OptionalHeaderDataDirectories;

// 8 + 4 + 4 + 4 + 4 + 4 + 4 + 2 + 2 + 4
int sizeOfSectionTableEntry = 40;
typedef struct SectionTableEntry {
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
} SectionTableEntry;

typedef struct SectionTable {
    SectionTableEntry* entries;
    int count;

    u32 SectionTableOffset;
} SectionTable;

// 8 + 4 + 2 + 2 + 1 + 1
int sizeOfSymbolTableEntry = 18;
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

typedef struct SymbolTable {
    SymbolTableEntry* entries;
    int count;

    u32 SymbolTableOffset;
} SymbolTable;

typedef struct StringTable {
    u8** strings;
    int count;

    u32 StringTableOffset;
} StringTable;

// 4 + 4 + 4 + 4 + 4
int sizeOfImportDirectoryTableEntry = 20;
typedef struct ImportDirectoryTableEntry {
    u32 ImportLookupTableRVA;
    u32 DateTimeStamp;
    u32 ForwarderChain;
    u32 NameRVA;
    u32 ImportAddressTableRVA;
} ImportDirectoryTableEntry;

typedef struct ImportDirectoryTable {
    ImportDirectoryTableEntry* entries;
    int count;

    u32 ImportDirectoryTableOffset;
} ImportDirectoryTable;

// NOTE: in the file all this is stored in a single u32/u64
typedef struct ImportLookupTableEntry {
    u8 NameFlag;
    u16 OrdinalNumber;
    u32 NameTableRVA;
    
    // These are from the Hint/Name table, but the data is so closely linked that it makes more sense to combine the two
    u16 Hint;
    u8* Name;
} ImportLookupTableEntry;

typedef struct ImportLookupTable {
    ImportLookupTableEntry* entries;
    int count;

    u32 ImportLookupTableOffset;
} ImportLookupTable;

typedef struct ReadResult {
    PEHeader peHeader;
    OptionalHeaderStandardFields optStandard;
    union {
        OptionalHeaderWindowsSpecificFields32 as_32;
        OptionalHeaderWindowsSpecificFields64 as_64;
    } optWinSpec;
    OptionalHeaderDataDirectories optDataDirs;
    SectionTable sectionTable;
    SymbolTable symbolTable;
    StringTable stringTable;
    ImportDirectoryTable importDirsTable;
    ImportLookupTable importLookupTable;
} ReadResult;

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

int fileLen = 0;
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

PEHeader readPeHeader(u8* fileBuffer, u32 offset) {
    PEHeader result = {0};
    result.Machine              = Read16(fileBuffer, offset + 0);
    result.NumberOfSections     = Read16(fileBuffer, offset + 2);
    result.TimeDateStamp        = Read32(fileBuffer, offset + 4);
    result.PointerToSymbolTable = Read32(fileBuffer, offset + 8);
    result.NumberOfSymbols      = Read32(fileBuffer, offset + 12);
    result.SizeOfOptionalHeader = Read16(fileBuffer, offset + 16);
    result.Characteristics      = Read16(fileBuffer, offset + 18);
    result.peHeaderOffset       = offset;

    return result;
}

void printPeHeader(PEHeader peHeader) {
    printf("----------------- PE Header (0x%x) -----------------\n", peHeader.peHeaderOffset);
    printf("Machine:              0x%x\n", peHeader.Machine);
    printf("NumberOfSections:     %i\n", peHeader.NumberOfSections);
    printf("TimeDateStamp:        %i\n", peHeader.TimeDateStamp);
    printf("PointerToSymbolTable: 0x%x\n", peHeader.PointerToSymbolTable);
    printf("NumberOfSymbols:      %i\n", peHeader.NumberOfSymbols);
    printf("SizeOfOptionalHeader: %i\n", peHeader.SizeOfOptionalHeader);
    printf("Characteristics:      0x%x\n", peHeader.Characteristics);
}

OptionalHeaderStandardFields readOptHeader(u8* fileBuffer, u32 offset) {
    OptionalHeaderStandardFields result = {0};
    result.Magic                   = Read16(fileBuffer, offset + 0);
    result.MajorLinkerVersion      = Read8(fileBuffer,  offset + 2);
    result.MinorLinkerVersion      = Read8(fileBuffer,  offset + 3);
    result.SizeOfCode              = Read32(fileBuffer, offset + 4);
    result.SizeOfInitializedData   = Read32(fileBuffer, offset + 8);
    result.SizeOfUninitializedData = Read32(fileBuffer, offset + 12);
    result.AddressOfEntryPoint     = Read32(fileBuffer, offset + 16);
    result.BaseOfCode              = Read32(fileBuffer, offset + 20);
    result.optionalStandardOffset  = offset;
    
    if(result.Magic == OPT_HEADR_MAGIC_PE) {
        result.BaseOfData = Read32(fileBuffer, offset + 24);
        sizeOfOptionalHeaderStandardFields = 28;
    }

    return result;
}

void printOptHeader(OptionalHeaderStandardFields optionalStandard) {
    printf("------------- Optional Header Standard Fields (0x%x) -------------\n", optionalStandard.optionalStandardOffset);
    printf("Magic:                   0x%x\n", optionalStandard.Magic);
    printf("MajorLinkerVersion:      %u\n", optionalStandard.MajorLinkerVersion);
    printf("MinorLinkerVersion:      %u\n", optionalStandard.MinorLinkerVersion);
    printf("SizeOfCode:              %i\n", optionalStandard.SizeOfCode);
    printf("SizeOfInitializedData:   %i\n", optionalStandard.SizeOfInitializedData);
    printf("SizeOfUninitializedData: %i\n", optionalStandard.SizeOfUninitializedData);
    printf("AddressOfEntryPoint:     0x%x\n", optionalStandard.AddressOfEntryPoint);
    printf("BaseOfCode:              0x%x\n", optionalStandard.BaseOfCode);
    printf("BaseOfData:              0x%x\n", optionalStandard.BaseOfData);
}

OptionalHeaderWindowsSpecificFields32 readOptHeaderWinSpec32(u8* fileBuffer, u32 offset) {
    OptionalHeaderWindowsSpecificFields32 result = {0};
    result.ImageBase                   = Read32(fileBuffer, offset + 0); // original offset: 28, 28 - 28 = 0
    result.SectionAlignment            = Read32(fileBuffer, offset + 4); // original offset: 32, 32 - 28 = 4
    result.FileAlignment               = Read32(fileBuffer, offset + 8); // original offset: 36, 36 - 28 = 8
    result.MajorOperatingSystemVersion = Read16(fileBuffer, offset + 12); // original offset: 40, 40 - 28 = 12
    result.MinorOperatingSystemVersion = Read16(fileBuffer, offset + 14); // original offset: 42, 42 - 28 = 14
    result.MajorImageVersion           = Read16(fileBuffer, offset + 16); // original offset: 44, 44 - 28 = 16
    result.MinorImageVersion           = Read16(fileBuffer, offset + 18); // original offset: 46, 46 - 28 = 18
    result.MajorSubsystemVersion       = Read16(fileBuffer, offset + 20); // original offset: 48, 48 - 28 = 20
    result.MinorSubsystemVersion       = Read16(fileBuffer, offset + 22); // original offset: 50, 50 - 28 = 22
    result.Win32VersionValue           = Read32(fileBuffer, offset + 24); // original offset: 52, 52 - 28 = 24
    result.SizeOfImage                 = Read32(fileBuffer, offset + 28); // original offset: 56, 56 - 28 = 28
    result.SizeOfHeaders               = Read32(fileBuffer, offset + 32); // original offset: 60, 60 - 28 = 32
    result.CheckSum                    = Read32(fileBuffer, offset + 36); // original offset: 64, 64 - 28 = 36
    result.Subsystem                   = Read16(fileBuffer, offset + 40); // original offset: 68, 68 - 28 = 40
    result.DllCharacteristics          = Read16(fileBuffer, offset + 42); // original offset: 70, 70 - 28 = 42
    result.SizeOfStackReserve          = Read32(fileBuffer, offset + 44); // original offset: 72, 72 - 28 = 44
    result.SizeOfStackCommit           = Read32(fileBuffer, offset + 48); // original offset: 76, 76 - 28 = 48
    result.SizeOfHeapReserve           = Read32(fileBuffer, offset + 52); // original offset: 80, 80 - 28 = 52
    result.SizeOfHeapCommit            = Read32(fileBuffer, offset + 56); // original offset: 84, 84 - 28 = 56
    result.LoaderFlags                 = Read32(fileBuffer, offset + 60); // original offset: 88, 88 - 28 = 60
    result.NumberOfRvaAndSizes         = Read32(fileBuffer, offset + 64); // original offset: 92, 92 - 28 = 64
    result.OptionalHeaderWindowsSpecificFields32Offset = offset;

    return result;
}

OptionalHeaderWindowsSpecificFields64 readOptHeaderWinSpec64(u8* fileBuffer, u32 offset) {
    OptionalHeaderWindowsSpecificFields64 result = {0};
    result.ImageBase                   = Read64(fileBuffer, offset + 0); // original offset: 24, 24 - 24 = 0
    result.SectionAlignment            = Read32(fileBuffer, offset + 8); // original offset: 32, 32 - 24 = 8
    result.FileAlignment               = Read32(fileBuffer, offset + 12); // original offset: 36, 36 - 24 = 12
    result.MajorOperatingSystemVersion = Read16(fileBuffer, offset + 16); // original offset: 40, 40 - 24 = 16
    result.MinorOperatingSystemVersion = Read16(fileBuffer, offset + 18); // original offset: 42, 42 - 24 = 18
    result.MajorImageVersion           = Read16(fileBuffer, offset + 20); // original offset: 44, 44 - 24 = 20
    result.MinorImageVersion           = Read16(fileBuffer, offset + 22); // original offset: 46, 46 - 24 = 22
    result.MajorSubsystemVersion       = Read16(fileBuffer, offset + 24); // original offset: 48, 48 - 24 = 24
    result.MinorSubsystemVersion       = Read16(fileBuffer, offset + 26); // original offset: 50, 50 - 24 = 26
    result.Win32VersionValue           = Read32(fileBuffer, offset + 28); // original offset: 52, 52 - 24 = 28
    result.SizeOfImage                 = Read32(fileBuffer, offset + 32); // original offset: 56, 56 - 24 = 32
    result.SizeOfHeaders               = Read32(fileBuffer, offset + 36); // original offset: 60, 60 - 24 = 36
    result.CheckSum                    = Read32(fileBuffer, offset + 40); // original offset: 64, 64 - 24 = 40
    result.Subsystem                   = Read16(fileBuffer, offset + 44); // original offset: 68, 68 - 24 = 44
    result.DllCharacteristics          = Read16(fileBuffer, offset + 46); // original offset: 70, 70 - 24 = 46
    result.SizeOfStackReserve          = Read64(fileBuffer, offset + 48); // original offset: 72, 72 - 24 = 48
    result.SizeOfStackCommit           = Read64(fileBuffer, offset + 56); // original offset: 80, 80 - 24 = 56
    result.SizeOfHeapReserve           = Read64(fileBuffer, offset + 64); // original offset: 88, 88 - 24 = 64
    result.SizeOfHeapCommit            = Read64(fileBuffer, offset + 72); // original offset: 96, 96 - 24 = 72
    result.LoaderFlags                 = Read32(fileBuffer, offset + 80); // original offset: 104, 104 - 24 = 80
    result.NumberOfRvaAndSizes         = Read32(fileBuffer, offset + 84); // original offset: 108, 108 - 24 = 84
    result.OptionalHeaderWindowsSpecificFields64Offset = offset;
    
    return result;
}

void printOptHeaderWinSpec64(OptionalHeaderWindowsSpecificFields64 optionalSpecific) {
    printf("--------------- Optional Header Windows-Specific Fields (0x%x) ---------------\n", optionalSpecific.OptionalHeaderWindowsSpecificFields64Offset);
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
}

void printOptHeaderWinSpec32(OptionalHeaderWindowsSpecificFields32 optionalSpecific) {
    printf("--------------- Optional Header Windows-Specific Fields (0x%x) ---------------\n", optionalSpecific.OptionalHeaderWindowsSpecificFields32Offset);
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
}

OptionalHeaderDataDirectories readOptDataHeader(u8* fileBuffer, u32 offset) {
    OptionalHeaderDataDirectories result = {0};
    result.ExportTable.addr           = Read32(fileBuffer, offset + 0 + 0); // if pe32+: 112, if pe32: 96
    result.ExportTable.size           = Read32(fileBuffer, offset + 0 + 4); // if pe32+: 112, if pe32: 96
    result.ImportTable.addr           = Read32(fileBuffer, offset + 8 + 0); // if pe32+: 120, if pe32: 104
    result.ImportTable.size           = Read32(fileBuffer, offset + 8 + 4); // if pe32+: 120, if pe32: 104
    result.ResourceTable.addr         = Read32(fileBuffer, offset + 16 + 0); // if pe32+: 128, if pe32: 112
    result.ResourceTable.size         = Read32(fileBuffer, offset + 16 + 4); // if pe32+: 128, if pe32: 112
    result.ExceptionTable.addr        = Read32(fileBuffer, offset + 24 + 0); // if pe32+: 136, if pe32: 120
    result.ExceptionTable.size        = Read32(fileBuffer, offset + 24 + 4); // if pe32+: 136, if pe32: 120
    result.CertificateTable.addr      = Read32(fileBuffer, offset + 32 + 0); // if pe32+: 144, if pe32: 128
    result.CertificateTable.size      = Read32(fileBuffer, offset + 32 + 4); // if pe32+: 144, if pe32: 128
    result.BaseRelocationTable.addr   = Read32(fileBuffer, offset + 40 + 0); // if pe32+: 152, if pe32: 136
    result.BaseRelocationTable.size   = Read32(fileBuffer, offset + 40 + 4); // if pe32+: 152, if pe32: 136
    result.Debug.addr                 = Read32(fileBuffer, offset + 48 + 0); // if pe32+: 160, if pe32: 144
    result.Debug.size                 = Read32(fileBuffer, offset + 48 + 4); // if pe32+: 160, if pe32: 144
    result.Architecture.addr          = Read32(fileBuffer, offset + 56 + 0); // if pe32+: 168, if pe32: 152
    result.Architecture.size          = Read32(fileBuffer, offset + 56 + 4); // if pe32+: 168, if pe32: 152
    result.GlobalPtr.addr             = Read32(fileBuffer, offset + 64 + 0); // if pe32+: 176, if pe32: 160
    result.GlobalPtr.size             = Read32(fileBuffer, offset + 64 + 4); // if pe32+: 176, if pe32: 160
    result.TLSTable.addr              = Read32(fileBuffer, offset + 72 + 0); // if pe32+: 184, if pe32: 168
    result.TLSTable.size              = Read32(fileBuffer, offset + 72 + 4); // if pe32+: 184, if pe32: 168
    result.LoadConfigTable.addr       = Read32(fileBuffer, offset + 80 + 0); // if pe32+: 192, if pe32: 176
    result.LoadConfigTable.size       = Read32(fileBuffer, offset + 80 + 4); // if pe32+: 192, if pe32: 176
    result.BoundImport.addr           = Read32(fileBuffer, offset + 88 + 0); // if pe32+: 200, if pe32: 184
    result.BoundImport.size           = Read32(fileBuffer, offset + 88 + 4); // if pe32+: 200, if pe32: 184
    result.IAT.addr                   = Read32(fileBuffer, offset + 96 + 0); // if pe32+: 208, if pe32: 192
    result.IAT.size                   = Read32(fileBuffer, offset + 96 + 4); // if pe32+: 208, if pe32: 192
    result.DelayImportDescriptor.addr = Read32(fileBuffer, offset + 104 + 0); // if pe32+: 216, if pe32: 200
    result.DelayImportDescriptor.size = Read32(fileBuffer, offset + 104 + 4); // if pe32+: 216, if pe32: 200
    result.CLRRuntimeHeader.addr      = Read32(fileBuffer, offset + 112 + 0); // if pe32+: 224, if pe32: 208
    result.CLRRuntimeHeader.size      = Read32(fileBuffer, offset + 112 + 4); // if pe32+: 224, if pe32: 208
    result.Reserved.addr              = Read32(fileBuffer, offset + 120 + 0); // if pe32+: 232, if pe32: 216
    result.Reserved.size              = Read32(fileBuffer, offset + 120 + 4); // if pe32+: 232, if pe32: 216
    result.OptionalHeaderDataDirectoriesOffset = offset;

    return result;
}

void printOptDataHeader(OptionalHeaderDataDirectories optionalDataDirs) {
    printf("---------------- Optional Header Data Directories (0x%x) ----------------\n", optionalDataDirs.OptionalHeaderDataDirectoriesOffset);
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
}

SectionTable readSectionTable(u8* fileBuffer, u32 offset, PEHeader peHeader) {
    SectionTable result = {0};
    result.count = peHeader.NumberOfSections;
    result.SectionTableOffset = offset;
    result.entries = malloc(result.count * sizeof(*result.entries));
    for (int i = 0; i < result.count; i++) {
        result.entries[i].Name.as_u64          = Read64(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 0);
        result.entries[i].VirtualSize          = Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 8);
        result.entries[i].VirtualAddress       = Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 12);
        result.entries[i].SizeOfRawData        = Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 16);
        result.entries[i].PointerToRawData     = Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 20);
        result.entries[i].PointerToRelocations = Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 24);
        result.entries[i].PointerToLinenumbers = Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 28);
        result.entries[i].NumberOfRelocations  = Read16(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 32);
        result.entries[i].NumberOfLinenumbers  = Read16(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 34);
        result.entries[i].Characteristics      = Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 36);
    }
    return result;
}

void printSectionCharacteristics(u32 characteristics) {
    if(characteristics & IMAGE_SCN_TYPE_NO_PAD)            printf("NO_PAD          ");
    if(characteristics & IMAGE_SCN_CNT_CODE)               printf("CODE            ");
    if(characteristics & IMAGE_SCN_CNT_INITIALIZED_DATA)   printf("INITIALIZED     ");
    if(characteristics & IMAGE_SCN_CNT_UNINITIALIZED_DATA) printf("UNINITIALIZED   ");
    if(characteristics & IMAGE_SCN_LNK_OTHER)              printf("LNK_OTHER       ");
    if(characteristics & IMAGE_SCN_LNK_INFO)               printf("LNK_INFO        ");
    if(characteristics & IMAGE_SCN_LNK_REMOVE)             printf("LNK_REMOVE      ");
    if(characteristics & IMAGE_SCN_LNK_COMDAT)             printf("LNK_COMDAT      ");
    if(characteristics & IMAGE_SCN_GPREL)                  printf("GPREL           ");
    if(characteristics & IMAGE_SCN_MEM_PURGEABLE)          printf("MEM_PURGEABLE   ");
    if(characteristics & IMAGE_SCN_MEM_16BIT)              printf("MEM_16BIT       ");
    if(characteristics & IMAGE_SCN_MEM_LOCKED)             printf("MEM_LOCKED      ");
    if(characteristics & IMAGE_SCN_MEM_PRELOAD)            printf("MEM_PRELOAD     ");
    if(characteristics & IMAGE_SCN_ALIGN_1BYTES)           printf("ALIGN_1BYTES    ");
    if(characteristics & IMAGE_SCN_ALIGN_2BYTES)           printf("ALIGN_2BYTES    ");
    if(characteristics & IMAGE_SCN_ALIGN_4BYTES)           printf("ALIGN_4BYTES    ");
    if(characteristics & IMAGE_SCN_ALIGN_8BYTES)           printf("ALIGN_8BYTES    ");
    if(characteristics & IMAGE_SCN_ALIGN_16BYTES)          printf("ALIGN_16BYTES   ");
    if(characteristics & IMAGE_SCN_ALIGN_32BYTES)          printf("ALIGN_32BYTES   ");
    if(characteristics & IMAGE_SCN_ALIGN_64BYTES)          printf("ALIGN_64BYTES   ");
    if(characteristics & IMAGE_SCN_ALIGN_128BYTES)         printf("ALIGN_128BYTES  ");
    if(characteristics & IMAGE_SCN_ALIGN_256BYTES)         printf("ALIGN_256BYTES  ");
    if(characteristics & IMAGE_SCN_ALIGN_512BYTES)         printf("ALIGN_512BYTES  ");
    if(characteristics & IMAGE_SCN_ALIGN_1024BYTES)        printf("ALIGN_1024BYTES ");
    if(characteristics & IMAGE_SCN_ALIGN_2048BYTES)        printf("ALIGN_2048BYTES ");
    if(characteristics & IMAGE_SCN_ALIGN_4096BYTES)        printf("ALIGN_4096BYTES ");
    if(characteristics & IMAGE_SCN_ALIGN_8192BYTES)        printf("ALIGN_8192BYTES ");
    if(characteristics & IMAGE_SCN_LNK_NRELOC_OVFL)        printf("LNK_NRELOC_OVFL ");
    if(characteristics & IMAGE_SCN_MEM_DISCARDABLE)        printf("MEM_DISCARDABLE ");
    if(characteristics & IMAGE_SCN_MEM_NOT_CACHED)         printf("MEM_NOT_CACHED  ");
    if(characteristics & IMAGE_SCN_MEM_NOT_PAGED)          printf("MEM_NOT_PAGED   ");
    if(characteristics & IMAGE_SCN_MEM_SHARED)             printf("MEM_SHARED      ");
    if(characteristics & IMAGE_SCN_MEM_EXECUTE)            printf("MEM_EXECUTE     ");
    if(characteristics & IMAGE_SCN_MEM_READ)               printf("MEM_READ        ");
    if(characteristics & IMAGE_SCN_MEM_WRITE)              printf("MEM_WRITE       ");
}

void printSectionTable(SectionTable sectionTable) {
    printf("------------------------ Section Table (0x%x) ------------------------\n", sectionTable.SectionTableOffset);
    printf("  name  | VirtualSize | VirtualAddress | SizeOfRawData | PointerToRawData | Characteristics\n");

    for(int i = 0; i < sectionTable.count; i++) {
        // name
        int collumWidth = 8;
        printf("%.8s", sectionTable.entries[i].Name.as_u8s);
        for(int h = 0; h < collumWidth - (8 - FindHowManyNonNullBytes(sectionTable.entries[i].Name.as_u64)); h++) printf(" ");
        printf("|");

        // VirtualSize
        collumWidth = 13;
        printf(" 0x%x", sectionTable.entries[i].VirtualSize);
        for(int h = 0; h < (collumWidth - 3) - FindHowManyHexDigits(sectionTable.entries[i].VirtualSize); h++) printf(" ");
        printf("|");
        
        // VirtualAddress
        collumWidth = 16;
        printf(" 0x%x", sectionTable.entries[i].VirtualAddress);
        for(int h = 0; h < (collumWidth - 3) - FindHowManyHexDigits(sectionTable.entries[i].VirtualAddress); h++) printf(" ");
        printf("|");
        
        // SizeOfRawData
        collumWidth = 15;
        printf(" 0x%x", sectionTable.entries[i].SizeOfRawData);
        for(int h = 0; h < (collumWidth - 3) - FindHowManyHexDigits(sectionTable.entries[i].SizeOfRawData); h++) printf(" ");
        printf("|");
        
        // PointerToRawData
        collumWidth = 18;
        printf(" 0x%x", sectionTable.entries[i].PointerToRawData);
        for(int h = 0; h < (collumWidth - 3) - FindHowManyHexDigits(sectionTable.entries[i].PointerToRawData); h++) printf(" ");
        printf("|");
        
        // Characteristics
        collumWidth = 17;
        printf(" ");
        printSectionCharacteristics(sectionTable.entries[i].Characteristics);
        // printf(" 0x%x", sectionTable.entries[i].Characteristics);
        // for(int h = 0; h < (collumWidth - 3) - FindHowManyHexDigits(sectionTable.entries[i].Characteristics); h++) printf(" ");
        // printf("|");
        printf("\n");
    }
}

SymbolTable readSymbolTable(u8* fileBuffer, PEHeader peHeader) {
    SymbolTable result = {0};
    result.count = peHeader.NumberOfSymbols;
    result.SymbolTableOffset = peHeader.PointerToSymbolTable;
    result.entries = malloc(result.count * sizeof(*result.entries));
    for(int i = 0; i < result.count; i++){
        result.entries[i].Name.as_u64       = Read64(fileBuffer, result.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 0);
        result.entries[i].Value             = Read32(fileBuffer, result.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 8);
        result.entries[i].SectionNumber     = Read16(fileBuffer, result.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 12);
        result.entries[i].Type              = Read16(fileBuffer, result.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 14);
        result.entries[i].StorageClass      = Read8(fileBuffer, result.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 16);
        result.entries[i].NumberOfAuxSymbol = Read8(fileBuffer, result.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 17);
    }
    return result;
}

void printSymbolTable(SymbolTable symbolTable) {
    printf("------------------ Symbol Table (0x%x) ------------------\n", symbolTable.SymbolTableOffset);
    for(int i = 0; i < symbolTable.count; i++){
        printf("Name:              %.8s (0x%x)\n", symbolTable.entries[i].Name.as_u8s, symbolTable.entries[i].Name.as_u64);
        printf("Value:             0x%x\n", symbolTable.entries[i].Value);
        printf("SectionNumber:     0x%x\n", symbolTable.entries[i].SectionNumber);
        printf("Type:              0x%x\n", symbolTable.entries[i].Type);
        printf("StorageClass:      0x%x\n", symbolTable.entries[i].StorageClass);
        printf("NumberOfAuxSymbol: 0x%x\n", symbolTable.entries[i].NumberOfAuxSymbol);
    }
}

StringTable readStringTable(u8* fileBuffer, u32 offset, u32* nextByteAfter) {
    StringTable result = {0};
    result.StringTableOffset = offset;
    u32 stringTableSizeBytes = Read32(fileBuffer, offset);
    
    if(stringTableSizeBytes > 4) {
        u8* stringTable = (u8*)fileBuffer + offset;
        
        int count = 0;
        for(int i = 4; i < stringTableSizeBytes; i++) {
            if (stringTable[i] == 0) count++;
            bytesRead++; // NOTE: this is just for keeping track of what percentage of the file we processed
        }

        result.strings = malloc(count * sizeof(u8*));
        result.count = count;

        count = 1;
        result.strings[0] = &stringTable[4];
        for(int i = 5; i < stringTableSizeBytes; i++) {
            if (stringTable[i] == 0 && i < stringTableSizeBytes - 1) {
                result.strings[count] = &stringTable[i + 1];
                count++;
            }
        }
    }
    *nextByteAfter = offset + stringTableSizeBytes;
    return result;
}

void printStringTable(StringTable stringTable, int limit) {
    printf("-------------- String Table (0x%x) --------------\n", stringTable.StringTableOffset);
    for(int i = 0; i < stringTable.count; i++){
        if(i < limit) printf("[%i] %s\n", i, stringTable.strings[i]);
    }
}

ImportDirectoryTable readImportTableDirectory(u8* fileBuffer, u32 offset, u32 idataSize, u32* nextByteAfter) {
    ImportDirectoryTable result = {0};

    // Find the count
    int dirTableCount = 0;
    for(u8* addr = fileBuffer + offset; addr < fileBuffer + offset + idataSize;){
        ImportDirectoryTableEntry tmp = {0};
        ImportDirectoryTableEntry zero = {0};
        memcpy(&tmp, addr, sizeof(tmp));
        if(memcmp(&tmp, &zero, sizeof(tmp)) == 0) break;
        dirTableCount++;
        addr += sizeof(tmp);
    }

    result.entries = malloc(dirTableCount * sizeof(*result.entries));
    result.count = dirTableCount;
    result.ImportDirectoryTableOffset = offset;
    for(int i = 0; i < dirTableCount; i++){
        result.entries[i].ImportLookupTableRVA  = Read32(fileBuffer, offset + (i * sizeOfImportDirectoryTableEntry) + 0);
        result.entries[i].DateTimeStamp         = Read32(fileBuffer, offset + (i * sizeOfImportDirectoryTableEntry) + 4);
        result.entries[i].ForwarderChain        = Read32(fileBuffer, offset + (i * sizeOfImportDirectoryTableEntry) + 8);
        result.entries[i].NameRVA               = Read32(fileBuffer, offset + (i * sizeOfImportDirectoryTableEntry) + 12);
        result.entries[i].ImportAddressTableRVA = Read32(fileBuffer, offset + (i * sizeOfImportDirectoryTableEntry) + 16);
    }
    // NOTE: the +1 is for the entry at the end filled with all zeros
    *nextByteAfter = offset + ((result.count + 1) * sizeOfImportDirectoryTableEntry);
    return result;
}

void printImportDirectoryTable(ImportDirectoryTable dirs, u8* fileBuffer, SectionTableEntry section) {
    printf("------------------ Import Directory Table (0x%x) ------------------\n", dirs.ImportDirectoryTableOffset);
    for(int i = 0; i < dirs.count; i++){
        printf("[%i]: Import Directory Entry (0x%x) ------------------\n", i, dirs.ImportDirectoryTableOffset + i * sizeOfImportDirectoryTableEntry);
        int nameAddr = section.PointerToRawData + (dirs.entries[i].NameRVA - section.VirtualAddress);
        printf("  ImportLookupTableRVA:  0x%x\n", dirs.entries[i].ImportLookupTableRVA);
        printf("  DateTimeStamp:         0x%x\n", dirs.entries[i].DateTimeStamp);
        printf("  ForwarderChain:        0x%x\n", dirs.entries[i].ForwarderChain);
        printf("  ImportAddressTableRVA: 0x%x\n", dirs.entries[i].ImportAddressTableRVA);
        if(dirs.entries[i].NameRVA){
            printf("  NameRVA:               0x%x (%s)\n", dirs.entries[i].NameRVA, fileBuffer + nameAddr);
        } else {
            printf("  NameRVA:               0x%x\n", dirs.entries[i].NameRVA);
        }
    }
}

ImportLookupTable readImportLookupTable(u8* fileBuffer, u32 offset, int bytesPerEntry, SectionTableEntry section, u32* nextByteAfter) {
    ImportLookupTable result = {0};
    
    assert((bytesPerEntry == 4 || bytesPerEntry == 8) && "Can only be u32 or u64 based if its a pe32 or pe32+");
    u8* basePtr = fileBuffer + offset;
    int entryCount = 0;
    while(basePtr < fileBuffer + offset + section.SizeOfRawData){
        u64 tmp = 0;
        u64 zero = 0;
        memcpy(&tmp, basePtr, bytesPerEntry);
        if(memcmp(&tmp, &zero, bytesPerEntry) == 0) break;
        entryCount++;
        basePtr += bytesPerEntry;
    }

    result.entries = malloc(entryCount * sizeof(*result.entries));
    result.count = entryCount;
    result.ImportLookupTableOffset = offset;

    for(int i = 0; i < entryCount; i++){
        u64 data = 0;
        if(bytesPerEntry == 4) {
            data = Read32(fileBuffer, offset + (i * sizeof(u32)));
            result.entries[i].NameFlag = (data & 0x80000000) ? 1 : 0;
        } else if(bytesPerEntry == 8) {
            data = Read64(fileBuffer, offset + (i * sizeof(u64)));
            result.entries[i].NameFlag = (data & 0x8000000000000000) ? 1 : 0;
        } else {
            assert(0 && "Unreachable");
        }
        result.entries[i].OrdinalNumber = data & 0x000000000000FFFF;
        result.entries[i].NameTableRVA  = data & 0x000000007FFFFFFF; // pointer to the ascii name, used to lookup in the .dll file export table if the hint lookup fails

        // Get the string
        int nameAddr = section.PointerToRawData + (result.entries[i].NameTableRVA - section.VirtualAddress);
        result.entries[i].Hint = Read16(fileBuffer, nameAddr); // hint is the index into the .dll file export table, used to lookup first
        result.entries[i].Name = fileBuffer + nameAddr + 2; // +2 to skip the hint

        // for stats
        int len = strlen(result.entries[i].Name);
        bytesRead += len + 1; // +1 for the null byte
    }
    
    // NOTE: the +1 is for the entry at the end filled with all zeros
    *nextByteAfter = offset + ((result.count + 1) * bytesPerEntry);
    return result;
}

void printImportLookupTable(ImportLookupTable table) {
    printf("------------------- Import Lookup Table (0x%x) -------------------\n", table.ImportLookupTableOffset);
    for(int i = 0; i < table.count; i++){
        printf("[%i]: ", i);
        printf("flag: %i, ", table.entries[i].NameFlag);
        printf("ordinalNumber: %i, ", table.entries[i].OrdinalNumber);
        printf("offset: 0x%x, ", table.entries[i].NameTableRVA);
        printf("name: %s, ", table.entries[i].Name);
        printf("hint: %i", table.entries[i].Hint);
        printf("\n");
    }
}

ReadResult readFile(Args args) {
    ReadResult result = {0};
    u8* fileBuffer = EntireFileRead(args.filename, &fileLen);
    if(fileBuffer[0] != 0x4d || fileBuffer[1] != 0x5a){
        fprintf(stderr, "[ERROR] Magic number not found\n");
        exit(1);
    }

    int nextSectionFirstByteAddr = 0;

    #define PE_HEADER_PTR_LOC 0x3c
    u32 peHeaderOffset = Read32(fileBuffer, PE_HEADER_PTR_LOC);
    printf("peHeaderOffset: 0x%x\n", peHeaderOffset);
    u32 peMagic = Read32(fileBuffer, peHeaderOffset);
    peHeaderOffset += 4;
    printf("peMagic: 0x%x\n", peMagic);

    // PE Header
    result.peHeader = readPeHeader(fileBuffer, peHeaderOffset);
    if(args.debugPrintPeHeader) printPeHeader(result.peHeader);
    nextSectionFirstByteAddr = peHeaderOffset + sizeOfPeHeader;

    // Optonal Standard Header
    u32 optionalStandardOffset = nextSectionFirstByteAddr;
    result.optStandard = readOptHeader(fileBuffer, optionalStandardOffset);
    if(args.debugPrintOptStandardHeader) printOptHeader(result.optStandard);
    nextSectionFirstByteAddr = optionalStandardOffset + sizeOfOptionalHeaderStandardFields;

    // Optional Windows Specific Header
    if(result.optStandard.Magic == OPT_HEADR_MAGIC_PE) {
        // pe32
        u32 optHeaderWinSpecOffset = nextSectionFirstByteAddr;
        result.optWinSpec.as_32 = readOptHeaderWinSpec32(fileBuffer, optHeaderWinSpecOffset);
        if(args.debugPrintOptSpecHeader) printOptHeaderWinSpec32(result.optWinSpec.as_32);
        nextSectionFirstByteAddr = optHeaderWinSpecOffset + sizeOfOptionalHeaderWindowsSpecificFields32;
    } else if(result.optStandard.Magic == OPT_HEADR_MAGIC_PE_PLUS) {
        // pe32+
        u32 optHeaderWinSpecOffset = nextSectionFirstByteAddr;
        result.optWinSpec.as_64 = readOptHeaderWinSpec64(fileBuffer, optHeaderWinSpecOffset);
        if(args.debugPrintOptSpecHeader) printOptHeaderWinSpec64(result.optWinSpec.as_64);
        nextSectionFirstByteAddr = optHeaderWinSpecOffset + sizeOfOptionalHeaderWindowsSpecificFields64;
    } else {
        printf("optionalHeaderMagic is incorrect: 0x%x\n", result.optStandard.Magic);
        exit(1);
    }

    // Optional Data Directory Header
    u32 optDataHeaderOffset = nextSectionFirstByteAddr;
    result.optDataDirs = readOptDataHeader(fileBuffer, optDataHeaderOffset);
    if(args.debugPrintOptDataHeader) printOptDataHeader(result.optDataDirs);
    nextSectionFirstByteAddr = optDataHeaderOffset + sizeOfOptionalHeaderDataDirectories;

    // Section Table
    // u32 sectionTableOffset = nextSectionFirstByteAddr;
    u32 sectionTableOffset = (peHeaderOffset + sizeOfPeHeader) + result.peHeader.SizeOfOptionalHeader;
    result.sectionTable = readSectionTable(fileBuffer, sectionTableOffset, result.peHeader);
    if(args.debugPrintSectionTable) printSectionTable(result.sectionTable);
    nextSectionFirstByteAddr = sectionTableOffset + (sizeOfSectionTableEntry * result.sectionTable.count);

    if(args.dumpfilepath){
        u8* codeBuffer = fileBuffer + result.sectionTable.entries[0].PointerToRawData;
        FILE* f = fopen(args.dumpfilepath, "wb");
        assert(f && "Failed to open file to dump data");
        fwrite(codeBuffer, sizeof(u8), result.sectionTable.entries[0].SizeOfRawData, f);
        fclose(f);
    }

    // Read the section data, just to mark the sections as read
    // for(int h = 0; h < result.sectionTable.entries[0].SizeOfRawData; h++) Read8(fileBuffer, result.sectionTable.entries[0].PointerToRawData + h);
    // for(int h = 0; h < result.sectionTable.entries[1].SizeOfRawData; h++) Read8(fileBuffer, result.sectionTable.entries[1].PointerToRawData + h);
    // for(int j = 10; j < result.sectionTable.count; j++) for(int h = 0; h < result.sectionTable.entries[j].SizeOfRawData; h++) Read8(fileBuffer, result.sectionTable.entries[j].PointerToRawData + h);

    // Symbol Table
    if(result.peHeader.NumberOfSymbols) {
        result.symbolTable = readSymbolTable(fileBuffer, result.peHeader);
        if(args.debugPrintSymbolTable) printSymbolTable(result.symbolTable);
        // NOTE: this isnt necesseceraly correct, because the symbol table doesnt have to follow the previous sections
        nextSectionFirstByteAddr = result.symbolTable.SymbolTableOffset + (result.symbolTable.count * sizeOfSymbolTableEntry);
    }

    // Strings table
    // TODO: find out how to confirm if the file has string table, and where it is located if no symbol table is present
    if(result.peHeader.NumberOfSymbols) {
        int stringTableOffset = result.peHeader.PointerToSymbolTable + sizeOfSymbolTableEntry * result.peHeader.NumberOfSymbols;
        result.stringTable = readStringTable(fileBuffer, stringTableOffset, &nextSectionFirstByteAddr);
        if(args.debugPrintStringTable) printStringTable(result.stringTable, args.stringTableLimit == -1 ? INT32_MAX : args.stringTableLimit);
    }

    // .idata common info
    // if not in sections, just use absolute address
    u32 importDirectoryTableOffset = result.optDataDirs.ImportTable.addr;
    u32 importTableSize = result.optDataDirs.ImportTable.size;
    int importSectionIndex = 0;
    for(int i = 0; i < result.peHeader.NumberOfSections; i++){
        if(result.sectionTable.entries[i].VirtualAddress == result.optDataDirs.ImportTable.addr) {
            importDirectoryTableOffset = result.sectionTable.entries[i].PointerToRawData;
            importTableSize = result.sectionTable.entries[i].SizeOfRawData;
            importSectionIndex = i;
            break;
        }
    }
    
    // Import Directory Table
    result.importDirsTable = readImportTableDirectory(fileBuffer, importDirectoryTableOffset, importTableSize, &nextSectionFirstByteAddr);
    if(args.debugPrintImportDirectoryTable) printImportDirectoryTable(result.importDirsTable, fileBuffer, result.sectionTable.entries[importSectionIndex]);

    // Import Lookup Table
    for(int i = 0; i < result.importDirsTable.count; i++) {
        SectionTableEntry section = result.sectionTable.entries[importSectionIndex];
        int offset = section.PointerToRawData + result.importDirsTable.entries[i].ImportLookupTableRVA - section.VirtualAddress;
        
        int importLookupTableOffset = offset;
        int bytesPerEntry = result.optStandard.Magic == OPT_HEADR_MAGIC_PE ? 4 : 8;
        result.importLookupTable = readImportLookupTable(fileBuffer, importLookupTableOffset, bytesPerEntry, section, &nextSectionFirstByteAddr);
        if(args.debugPrintImportLookupTable) printImportLookupTable(result.importLookupTable);
    }

    return result;
}

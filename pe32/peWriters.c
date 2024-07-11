#include "common.h"

void Write8(u8* buffer, u32 offset, u8 data) {
    buffer[offset] = data;
}

void Write16(u8* buffer, u32 offset, u16 data) {
    buffer[offset + 0] = (data >> (8 * 0)) & 0xFF;
    buffer[offset + 1] = (data >> (8 * 1)) & 0xFF;
}

void Write32(u8* buffer, u32 offset, u32 data) {
    buffer[offset + 0] = (data >> (8 * 0)) & 0xFF;
    buffer[offset + 1] = (data >> (8 * 1)) & 0xFF;
    buffer[offset + 2] = (data >> (8 * 2)) & 0xFF;
    buffer[offset + 3] = (data >> (8 * 3)) & 0xFF;
}

void Write64(u8* buffer, u32 offset, u64 data) {
    buffer[offset + 0] = (data >> (8 * 0)) & 0xFF;
    buffer[offset + 1] = (data >> (8 * 1)) & 0xFF;
    buffer[offset + 2] = (data >> (8 * 2)) & 0xFF;
    buffer[offset + 3] = (data >> (8 * 3)) & 0xFF;
    buffer[offset + 4] = (data >> (8 * 4)) & 0xFF;
    buffer[offset + 5] = (data >> (8 * 5)) & 0xFF;
    buffer[offset + 6] = (data >> (8 * 6)) & 0xFF;
    buffer[offset + 7] = (data >> (8 * 7)) & 0xFF;
}

void writePeHeader(u8* fileBuffer, PEHeader header) {
    u32 offset = header.peHeaderOffset;
    
    Write16(fileBuffer, offset + 0, header.Machine);
    Write16(fileBuffer, offset + 2, header.NumberOfSections);
    Write32(fileBuffer, offset + 4, header.TimeDateStamp);
    Write32(fileBuffer, offset + 8, header.PointerToSymbolTable);
    Write32(fileBuffer, offset + 12, header.NumberOfSymbols);
    Write16(fileBuffer, offset + 16, header.SizeOfOptionalHeader);
    Write16(fileBuffer, offset + 18, header.Characteristics);
}

void writeOptHeader(u8* fileBuffer, OptionalHeaderStandardFields header) {
    u32 offset = header.optionalStandardOffset;
    Write16(fileBuffer, offset + 0, header.Magic);
    Write8(fileBuffer,  offset + 2, header.MajorLinkerVersion);
    Write8(fileBuffer,  offset + 3, header.MinorLinkerVersion);
    Write32(fileBuffer, offset + 4, header.SizeOfCode);
    Write32(fileBuffer, offset + 8, header.SizeOfInitializedData);
    Write32(fileBuffer, offset + 12, header.SizeOfUninitializedData);
    Write32(fileBuffer, offset + 16, header.AddressOfEntryPoint);
    Write32(fileBuffer, offset + 20, header.BaseOfCode);
    
    if(header.Magic == OPT_HEADR_MAGIC_PE) {
        Write32(fileBuffer, offset + 24, header.BaseOfData);
    }
}

void writeOptHeaderWinSpec32(u8* fileBuffer, OptionalHeaderWindowsSpecificFields32 header) {
    u32 offset = header.OptionalHeaderWindowsSpecificFields32Offset;
    Read32(fileBuffer, offset + 0, header.ImageBase);
    Read32(fileBuffer, offset + 4, header.SectionAlignment);
    Read32(fileBuffer, offset + 8, header.FileAlignment);
    Read16(fileBuffer, offset + 12, header.MajorOperatingSystemVersion);
    Read16(fileBuffer, offset + 14, header.MinorOperatingSystemVersion);
    Read16(fileBuffer, offset + 16, header.MajorImageVersion);
    Read16(fileBuffer, offset + 18, header.MinorImageVersion);
    Read16(fileBuffer, offset + 20, header.MajorSubsystemVersion);
    Read16(fileBuffer, offset + 22, header.MinorSubsystemVersion);
    Read32(fileBuffer, offset + 24, header.Win32VersionValue);
    Read32(fileBuffer, offset + 28, header.SizeOfImage);
    Read32(fileBuffer, offset + 32, header.SizeOfHeaders);
    Read32(fileBuffer, offset + 36, header.CheckSum);
    Read16(fileBuffer, offset + 40, header.Subsystem);
    Read16(fileBuffer, offset + 42, header.DllCharacteristics);
    Read32(fileBuffer, offset + 44, header.SizeOfStackReserve);
    Read32(fileBuffer, offset + 48, header.SizeOfStackCommit);
    Read32(fileBuffer, offset + 52, header.SizeOfHeapReserve);
    Read32(fileBuffer, offset + 56, header.SizeOfHeapCommit);
    Read32(fileBuffer, offset + 60, header.LoaderFlags);
    Read32(fileBuffer, offset + 64, header.NumberOfRvaAndSizes);
}

void writeOptHeaderWinSpec64(u8* fileBuffer, OptionalHeaderWindowsSpecificFields64 header) {
    u32 offset = header.OptionalHeaderWindowsSpecificFields64Offset;
    Read64(fileBuffer, offset + 0, header.ImageBase);
    Read32(fileBuffer, offset + 8, header.SectionAlignment);
    Read32(fileBuffer, offset + 12, header.FileAlignment);
    Read16(fileBuffer, offset + 16, header.MajorOperatingSystemVersion);
    Read16(fileBuffer, offset + 18, header.MinorOperatingSystemVersion);
    Read16(fileBuffer, offset + 20, header.MajorImageVersion);
    Read16(fileBuffer, offset + 22, header.MinorImageVersion);
    Read16(fileBuffer, offset + 24, header.MajorSubsystemVersion);
    Read16(fileBuffer, offset + 26, header.MinorSubsystemVersion);
    Read32(fileBuffer, offset + 28, header.Win32VersionValue);
    Read32(fileBuffer, offset + 32, header.SizeOfImage);
    Read32(fileBuffer, offset + 36, header.SizeOfHeaders);
    Read32(fileBuffer, offset + 40, header.CheckSum);
    Read16(fileBuffer, offset + 44, header.Subsystem);
    Read16(fileBuffer, offset + 46, header.DllCharacteristics);
    Read64(fileBuffer, offset + 48, header.SizeOfStackReserve);
    Read64(fileBuffer, offset + 56, header.SizeOfStackCommit);
    Read64(fileBuffer, offset + 64, header.SizeOfHeapReserve);
    Read64(fileBuffer, offset + 72, header.SizeOfHeapCommit);
    Read32(fileBuffer, offset + 80, header.LoaderFlags);
    Read32(fileBuffer, offset + 84, header.NumberOfRvaAndSizes);
}

void writeOptDataHeader(u8* fileBuffer, OptionalHeaderDataDirectories header) {
    u32 offset = header.OptionalHeaderDataDirectoriesOffset;
    Read32(fileBuffer, offset + 0 + 0, header.ExportTable.addr);
    Read32(fileBuffer, offset + 0 + 4, header.ExportTable.size);
    Read32(fileBuffer, offset + 8 + 0, header.ImportTable.addr);
    Read32(fileBuffer, offset + 8 + 4, header.ImportTable.size);
    Read32(fileBuffer, offset + 16 + 0, header.ResourceTable.addr);
    Read32(fileBuffer, offset + 16 + 4, header.ResourceTable.size);
    Read32(fileBuffer, offset + 24 + 0, header.ExceptionTable.addr);
    Read32(fileBuffer, offset + 24 + 4, header.ExceptionTable.size);
    Read32(fileBuffer, offset + 32 + 0, header.CertificateTable.addr);
    Read32(fileBuffer, offset + 32 + 4, header.CertificateTable.size);
    Read32(fileBuffer, offset + 40 + 0, header.BaseRelocationTable.addr);
    Read32(fileBuffer, offset + 40 + 4, header.BaseRelocationTable.size);
    Read32(fileBuffer, offset + 48 + 0, header.Debug.addr);
    Read32(fileBuffer, offset + 48 + 4, header.Debug.size);
    Read32(fileBuffer, offset + 56 + 0, header.Architecture.addr);
    Read32(fileBuffer, offset + 56 + 4, header.Architecture.size);
    Read32(fileBuffer, offset + 64 + 0, header.GlobalPtr.addr);
    Read32(fileBuffer, offset + 64 + 4, header.GlobalPtr.size);
    Read32(fileBuffer, offset + 72 + 0, header.TLSTable.addr);
    Read32(fileBuffer, offset + 72 + 4, header.TLSTable.size);
    Read32(fileBuffer, offset + 80 + 0, header.LoadConfigTable.addr);
    Read32(fileBuffer, offset + 80 + 4, header.LoadConfigTable.size);
    Read32(fileBuffer, offset + 88 + 0, header.BoundImport.addr);
    Read32(fileBuffer, offset + 88 + 4, header.BoundImport.size);
    Read32(fileBuffer, offset + 96 + 0, header.IAT.addr);
    Read32(fileBuffer, offset + 96 + 4, header.IAT.size);
    Read32(fileBuffer, offset + 104 + 0, header.DelayImportDescriptor.addr);
    Read32(fileBuffer, offset + 104 + 4, header.DelayImportDescriptor.size);
    Read32(fileBuffer, offset + 112 + 0, header.CLRRuntimeHeader.addr);
    Read32(fileBuffer, offset + 112 + 4, header.CLRRuntimeHeader.size);
    Read32(fileBuffer, offset + 120 + 0, header.Reserved.addr);
    Read32(fileBuffer, offset + 120 + 4, header.Reserved.size);
}

void writeSectionTable(u8* fileBuffer, SectionTable table) {
    u32 offset = table.SectionTableOffset;
    for (int i = 0; i < table.count; i++) {
        Read64(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 0, table.entries[i].Name.as_u64);
        Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 8, table.entries[i].VirtualSize);
        Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 12, table.entries[i].VirtualAddress);
        Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 16, table.entries[i].SizeOfRawData);
        Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 20, table.entries[i].PointerToRawData);
        Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 24, table.entries[i].PointerToRelocations);
        Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 28, table.entries[i].PointerToLinenumbers);
        Read16(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 32, table.entries[i].NumberOfRelocations);
        Read16(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 34, table.entries[i].NumberOfLinenumbers);
        Read32(fileBuffer, offset + (i * sizeOfSectionTableEntry) + 36, table.entries[i].Characteristics);
    }
}

void writeSymbolTable(u8* fileBuffer, SymbolTable table) {
    u32 offset = table.SymbolTableOffset;
    for(int i = 0; i < table.count; i++){
        Read64(fileBuffer, table.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 0, table.entries[i].Name.as_u64);
        Read32(fileBuffer, table.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 8, table.entries[i].Value);
        Read16(fileBuffer, table.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 12, table.entries[i].SectionNumber);
        Read16(fileBuffer, table.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 14, table.entries[i].Type);
        Read8(fileBuffer, table.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 16, table.entries[i].StorageClass);
        Read8(fileBuffer, table.SymbolTableOffset + (i * sizeOfSymbolTableEntry) + 17, table.entries[i].NumberOfAuxSymbol);
    }
}

// NOTE: maybe its better to pass the size of the struct in bytes and just memcpy it
void writeStringTable(u8* fileBuffer, StringTable table) {
    u32 offset = table.StringTableOffset;
    u32 bytesWritten = 4;
    
    if(table.count > 0) {
        for(int i = 0; i < table.count; i++) {
            u8* str = table.strings[i];
            while(str != 0) {
                Write8(fileBuffer, offset + 4 + bytesWritten, *str);
                bytesWritten++;
            }
            Write8(fileBuffer, offset + bytesWritten, 0);
            bytesWritten++;
        }
    }
    Write32(fileBuffer, offset, bytesWritten);
}

// NOTE: needs to end with a zero ImportDirectoryTableEntry
void writeImportTableDirectory(u8* fileBuffer, u32 idataSize, ImportDirectoryTable table) {
    u32 offset = table.ImportDirectoryTableOffset;

    for(int i = 0; i < table.count; i++){
        Read32(fileBuffer, offset + (i * sizeOfImportDirectoryTableEntry) + 0, table.entries[i].ImportLookupTableRVA);
        Read32(fileBuffer, offset + (i * sizeOfImportDirectoryTableEntry) + 4, table.entries[i].DateTimeStamp);
        Read32(fileBuffer, offset + (i * sizeOfImportDirectoryTableEntry) + 8, table.entries[i].ForwarderChain);
        Read32(fileBuffer, offset + (i * sizeOfImportDirectoryTableEntry) + 12, table.entries[i].NameRVA);
        Read32(fileBuffer, offset + (i * sizeOfImportDirectoryTableEntry) + 16, table.entries[i].ImportAddressTableRVA);
    }
}

// NOTE: needs to end with a zero ImportLookupTableEntry
void writeImportLookupTable(u8* fileBuffer, int bytesPerEntry, SectionTableEntry section, ImportLookupTable table) {
    assert((bytesPerEntry == 4 || bytesPerEntry == 8) && "Can only be u32 or u64 based if its a pe32 or pe32+");
    u32 offset = table.ImportLookupTableOffset;

    for(int i = 0; i < table.count; i++) {
        if(bytesPerEntry == 4) {
            u32 data = 0;
            if(table.entries[i].NameFlag) {
                data |= 0x80000000;
                data |= table.entries[i].NameTableRVA;
            } else {
                data |= table.entries[i].OrdinalNumber;
            }
            Write32(fileBuffer, offset + i * bytesPerEntry, data);
        } else if(bytesPerEntry == 8) {
            u64 data = 0;
            if(table.entries[i].NameFlag) {
                data |= 0x8000000000000000;
                data |= table.entries[i].NameTableRVA;
            } else {
                data |= table.entries[i].OrdinalNumber;
            }
            Write64(fileBuffer, offset + i * bytesPerEntry, data);
        } else {
            assert(0 && "Unreachable");
        }
    }

    // for(int i = 0; i < table.count; i++){
    //     // Get the string
    //     int nameAddr = section.PointerToRawData + (result.entries[i].NameTableRVA - section.VirtualAddress);
    //     result.entries[i].Hint = Read16(fileBuffer, nameAddr); // hint is the index into the .dll file export table, used to lookup first
    //     result.entries[i].Name = fileBuffer + nameAddr + 2; // +2 to skip the hint
    // }
}


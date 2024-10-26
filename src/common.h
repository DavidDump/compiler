#ifndef COMP_COMMON_H
#define COMP_COMMON_H

#include <stdint.h>

typedef int bool;
#define TRUE 1
#define FALSE 0

#define ARRAY_SIZE(x) (sizeof(x)/sizeof((x)[0]))
#define UNIMPLEMENTED(x) printf("[NOT IMPLEMENTED] %s:%i: %s\n", __FILE__, __LINE__, (x)), exit(EXIT_FAILURE)
#define UNREACHABLE(x) printf("[UNREACHABLE] %s:%i: %s\n", __FILE__, __LINE__, (x)), exit(EXIT_FAILURE)
#define UNUSED(x) (void)(x)
#define ERROR(loc, msg) printf("[ERROR] "STR_FMT":%i:%i %s\n", STR_PRINT((loc).filename), (loc).line, (loc).collum, msg), exit(EXIT_FAILURE)

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

#define S8_MIN  ((s8)0x80)
#define S8_MAX  ((s8)0x7F)
#define S16_MIN ((s16)0x8000)
#define S16_MAX ((s16)0x7FFF)
#define S32_MIN ((s32)0x80000000)
#define S32_MAX ((s32)0x7FFFFFFF)
#define S64_MIN ((s64)0x8000000000000000)
#define S64_MAX ((s64)0x7FFFFFFFFFFFFFFF)

#define U8_MIN  ((u8)0x0)
#define U8_MAX  ((u8)0xFF)
#define U16_MIN ((u16)0x0)
#define U16_MAX ((u16)0xFFFF)
#define U32_MIN ((u32)0x0)
#define U32_MAX ((u32)0xFFFFFFFF)
#define U64_MIN ((u64)0x0)
#define U64_MAX ((u64)0xFFFFFFFFFFFFFFFF)

#endif // COMP_COMMON_H
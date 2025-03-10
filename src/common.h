#ifndef COMP_COMMON_H
#define COMP_COMMON_H

#include <stdint.h>
#include <stdio.h>

typedef int bool;
#define TRUE 1
#define FALSE 0

#define ARRAY_SIZE(x) (sizeof(x)/sizeof((x)[0]))
#define UNIMPLEMENTED(x) printf("[NOT IMPLEMENTED] %s:%i: %s\n", __FILE__, __LINE__, (x)), exit(EXIT_FAILURE)
#define UNUSED(x) (void)(x)

#ifndef COMP_DEBUG
#  define assert(...)
#  define assertf(...)
#  define ERROR(loc, msg) printf("[ERROR] "STR_FMT":%i:%i %s\n", STR_PRINT((loc).filename), (loc).line, (loc).collum, msg), exit(EXIT_FAILURE)
#  define ERROR_VA(loc, msg, ...) printf("[ERROR] "STR_FMT":%i:%i "msg"\n", STR_PRINT((loc).filename), (loc).line, (loc).collum, __VA_ARGS__), exit(EXIT_FAILURE)
#  define UNREACHABLE(_msg_) printf("[UNREACHABLE] %s:%i: %s\n", __FILE__, __LINE__, (_msg_)), exit(EXIT_FAILURE)
#  define UNREACHABLE_VA(_msg_, ...) printf("[UNREACHABLE] %s:%i: "_msg_"\n", __FILE__, __LINE__, __VA_ARGS__), exit(EXIT_FAILURE)
#else
#  define assert(_condition_, _msg_) !(_condition_) ? (void)(printf("[ASSERT] %s:%i: %s\n", __FILE__, __LINE__, _msg_), *(int*)(0) = 0) : (void)0
#  define assertf(_condition_, _fmt_, ...) !(_condition_) ? (void)(printf("[ASSERT] %s:%i: "_fmt_"\n", __FILE__, __LINE__, __VA_ARGS__), *(int*)(0) = 0) : (void)0
#  define ERROR(loc, msg) printf("[ERROR] "STR_FMT":%i:%i %s\n", STR_PRINT((loc).filename), (loc).line, (loc).collum, msg), assert(0, "")
#  define ERROR_VA(loc, msg, ...) printf("[ERROR] "STR_FMT":%i:%i "msg"\n", STR_PRINT((loc).filename), (loc).line, (loc).collum, __VA_ARGS__), assert(0, "")
#  define UNREACHABLE(_msg_) printf("[UNREACHABLE] %s:%i: %s\n", __FILE__, __LINE__, (_msg_)), assert(0, "")
#  define UNREACHABLE_VA(_msg_, ...) printf("[UNREACHABLE] %s:%i: "_msg_"\n", __FILE__, __LINE__, __VA_ARGS__), assert(0, "")
#endif

// NOTE: assert used in arena.h is redefined here, so its only in one place
#define ARENA_ASSERT(_condition_) assert(_condition_, "")

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef float  f32;
typedef double f64;

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

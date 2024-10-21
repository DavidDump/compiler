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
#define ERROR(loc, msg) printf("[ERROR] %.*s:%i:%i %s\n", (loc).filename.length, (loc).filename.str, (loc).line, (loc).collum, msg), exit(EXIT_FAILURE)

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

#endif // COMP_COMMON_H
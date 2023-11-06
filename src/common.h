#ifndef COMP_COMMON_H
#define COMP_COMMON_H

typedef int bool;
#define TRUE 1
#define FALSE 0

// NOTE: -pedantic does not support __FUNCTION__
// #define UNIMPLEMENTED(x) printf("[NOT IMPLEMENTED] %s:%i in %s() %s\n", __FILE__, __LINE__, __FUNCTION__, (x))
#define UNIMPLEMENTED(x) printf("[NOT IMPLEMENTED] %s:%i: %s\n", __FILE__, __LINE__, (x))
#define UNUSED(x) (void)(x)
#define ERROR(loc, msg) printf("[ERROR] %.*s:%i:%i %s\n", (loc).filename.length, (loc).filename.str, (loc).line, (loc).collum, msg)

#endif // COMP_COMMON_H
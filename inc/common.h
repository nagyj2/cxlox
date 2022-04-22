#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h> // bool
#include <stddef.h> // NULL, size_t
#include <stdint.h> // uint8_t

// Enables chunk dumping when the compiler finishes with the chunk.
// #define DEBUG_PRINT_CODE

// Debug define to allow easier viewing of VM internals
// #define DEBUG_TRACE_EXECUTION
// #define DEBUG_TRACE_STACK

// Stress test mode for the garbage collector. Makes the GC run whenever it can.
// #define DEBUG_STRESS_GC

// Perform debug logging of the GC operations.
// #define DEBUG_LOG_GC

// Load the standard library. Muddies up GC output, so it can be disabled.
// #define DEBUG_LOAD_STDLIB

// If defined, the stack based OP_DEL_PROPERTY will be used.
// #define USE_STACK_PROPERTY_DELETE

#define UINT8_COUNT (UINT8_MAX + 1)

// Maximum length for a input string
#define MAX_STRING_LEN 256

#endif /* clox_common_h */

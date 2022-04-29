#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h> // bool
#include <stddef.h> // NULL, size_t
#include <stdint.h> // uint8_t

// Enable NaN boxing -> Using 64 bit floats to contain either a float, nil, true, false or pointer to a Obj
#define NAN_BOXING

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
#define DEBUG_LOAD_STDLIB

// If defined, the stack based OP_DEL_PROPERTY will be used.
#define USE_STACK_PROPERTY_DELETE

#define UINT8_COUNT (UINT8_MAX + 1)

// Maximum length for a input string
#define MAX_STRING_LEN 256

/* --- PRINTF_BYTE_TO_BINARY macro's --- */
#define PRINTF_BINARY_PATTERN_INT8 "%c%c%c%c%c%c%c%c"
#define PRINTF_BYTE_TO_BINARY_INT8(i)    \
    (((i) & 0x80ll) ? '1' : '0'), \
    (((i) & 0x40ll) ? '1' : '0'), \
    (((i) & 0x20ll) ? '1' : '0'), \
    (((i) & 0x10ll) ? '1' : '0'), \
    (((i) & 0x08ll) ? '1' : '0'), \
    (((i) & 0x04ll) ? '1' : '0'), \
    (((i) & 0x02ll) ? '1' : '0'), \
    (((i) & 0x01ll) ? '1' : '0')

#define PRINTF_BINARY_PATTERN_INT16 \
    PRINTF_BINARY_PATTERN_INT8              PRINTF_BINARY_PATTERN_INT8
#define PRINTF_BYTE_TO_BINARY_INT16(i) \
    PRINTF_BYTE_TO_BINARY_INT8((i) >> 8),   PRINTF_BYTE_TO_BINARY_INT8(i)
#define PRINTF_BINARY_PATTERN_INT32 \
    PRINTF_BINARY_PATTERN_INT16             PRINTF_BINARY_PATTERN_INT16
#define PRINTF_BYTE_TO_BINARY_INT32(i) \
    PRINTF_BYTE_TO_BINARY_INT16((i) >> 16), PRINTF_BYTE_TO_BINARY_INT16(i)
#define PRINTF_BINARY_PATTERN_INT64    \
    PRINTF_BINARY_PATTERN_INT32             PRINTF_BINARY_PATTERN_INT32
#define PRINTF_BYTE_TO_BINARY_INT64(i) \
    PRINTF_BYTE_TO_BINARY_INT32((i) >> 32), PRINTF_BYTE_TO_BINARY_INT32(i)

// Example:
// printf("global:  "
//   PRINTF_BINARY_PATTERN_INT32 "\n",
// 	PRINTF_BYTE_TO_BINARY_INT32(global));

/* --- end macros --- */

#endif /* clox_common_h */

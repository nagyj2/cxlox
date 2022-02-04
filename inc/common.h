#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h> // bool
#include <stddef.h> // NULL, size_t
#include <stdint.h> // uint8_t

// Enables chunk dumping when the compiler finishes with the chunk.
#define DEBUG_PRINT_CODE

// Debug define to allow easier viewing of VM internals.
#define DEBUG_TRACE_EXECUTION

#define UINT8_COUNT (UINT8_MAX + 1)

#endif /* clox_common_h */

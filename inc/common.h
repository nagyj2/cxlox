#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h> // bool
#include <stddef.h> // NULL, size_t
#include <stdint.h> // uint8_t

// ---- OPTIONS ----
// Enable NaN boxing -> Using 64 bit floats to contain either a float, nil, true, false or pointer to a Obj
// #define NAN_BOXING

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
#define USE_STACK_PROPERTY_DELETE

#define UINT8_COUNT (UINT8_MAX + 1)

// Maximum length for a input string
#define MAX_STRING_LEN 256

// Whether code comments should be permitted
#define USE_TYPE_COMMENT

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

// Typedef so everyone has access
typedef struct _vm VM;
 
typedef struct _Obj Obj;
typedef struct _ObjString ObjString;
typedef struct _ObjModule ObjModule;
typedef struct _ObjFunction ObjFunction;
typedef struct _ObjNative ObjNative;
typedef struct _ObjUpvalue ObjUpvalue;
typedef struct _ObjClosure ObjClosure;
typedef struct _ObjClass ObjClass;
typedef struct _ObjInstance ObjInstance;
typedef struct _ObjBoundMethod ObjBoundMethod;
typedef struct _ObjList ObjList;

typedef struct _Compiler Compiler;
// typedef enum _FunctionType FunctionType;

//! 'Unknown' or 'incomplete' type errors when placed inside object.h
/** The type of code which is being compiled. */
typedef enum {
	TYPE_FUNCTION,			//* A function body is being compiled.
	TYPE_LAMBDA,				//* An anonymous function is being compiled.
	TYPE_SCRIPT,				//* The top-level (global) code is being compiled.
	TYPE_METHOD,				//* A method is being compiled.
	TYPE_INITIALIZER,		//* Initializer method is being compiled.
} FunctionType;

/** Initialize state of the VM.
 * @details
 * Resets the stack, initializes internal hash tables and GC metadata.
 */
VM* initVM();

/** Cleanup and free the state of the VM.
 * @details
 * Frees all memory allocated to objects and contents of state hash tables.
 */
void freeVM(VM* vm);

/* Result of the VM's interpretation. */
typedef enum {
	INTERPRET_OK,							//* VM completed interpretation successfully.
	INTERPRET_COMPILE_ERROR,	//* VM encountered a compile-time error.
	INTERPRET_RUNTIME_ERROR		//* VM encountered a runtime-error.
} InterpretResult;

#endif /* clox_common_h */

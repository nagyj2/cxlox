#ifndef clox_vm_h
#define clox_vm_h

#include <stdarg.h>

#include "common.h"
#include "chunk.h"
#include "compiler.h"
#include "table.h"
#include "value.h"
#include "object.h"

// Maximum call depth.
#define FRAMES_MAX 64
// Maximum size of the stack because each call can only index UINT8_COUNT locals.
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

/** Each function invocation creates a call frame which keeps track of properties used by a function call.
 */
typedef struct {
	ObjClosure* closure;		//* The closure being called. Instruction pointer can be accessed through the function's chunk
	uint8_t* ip;						//* Current execution location within the chunk.
	Value* slots;						//* The first slot on the stack which the function can use.
} CallFrame;

/* State information of the VM. */
struct _vm {
	// VM Execution
	Compiler* compiler;			//* Compiler which created the VM's bytecode
	Value stack[STACK_MAX]; //* The entire argument stack.
	Value* stackTop;				//* Pointer to the slot just PAST the top of the stack. 
	CallFrame frames[FRAMES_MAX];	//* The call stack.
	int frameCount;					//* Number of frames on the call stack.
	Obj* objects; 					//* A pointer to the first allocated object
	// stack == stackTop = > empty stack.

	Table strings;						//* A table for string internment.
	Table globals;						//* A table for global variables.
	Table constants;					//* A table for global constants.
	bool isREPL;							//* Whether or not we are in REPL mode.
	ObjUpvalue* openUpvalues;	//* A linked list of upvalues which are still open.
	ObjString* initString;		//* String which represents an initializer method. Fast b/c of string interning.

	// GC
	int grayCount;					//* Number of gray objects.
	int grayCapacity;				//* Capacity of the gray stack.
	Obj** grayStack;				//* Stack of gray objects.

	size_t bytesAllocated;	//* Number of bytes allocated for the runtime.
	size_t nextGC;					//* Bytes until the next GC cycle should run.
	void* rainyDay;					//* A portion of memory allocated in case the GC needs an allocation to function but cannot.
	bool usedRainyDay;			//* Whether or not the rainy day memory has been used.

	// Modules
	Table modules;					//* A table of active modules.
};

/** Prints an error message to stderr. Also resets the stack.
 * To prevent nonsense line methods, `frame->ip = ip;` must preceed this function call in run()
 *
 * @param[in] format Format string to print to stderr.
 * @param[in] ... The arguments to put into the format string.
 */
void runtimeError(VM* vm, const char* format, ...);

/** Performs interpretation of a input source text.
 * 
 * @param[in] source The source code to compile and interpret.
 * @return InterpreterResult representing the final state of the VM.
 */
InterpretResult interpret(VM* vm, const char* moduleName, const char* source);

/** Places a value onto the top of the stack. Increments the size of the stack.
 * 
 * @param[in] value The value to place.
 */
void push(VM* vm, Value value);

/** Removes the top value of the stack. Decrements the size of the stack.
 * 
 * @return Value The element which was last on top of the stack.
 */
Value pop(VM* vm);

#endif /* clox_vm_h */

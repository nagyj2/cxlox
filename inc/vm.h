#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

/* State information of the VM. VM is a global singleton. */
typedef struct {
	Chunk* chunk;	//* Currently executing chunk.
	uint8_t* ip;	//* Instruction pointer. Points to the NEXT EXECUTED instruction.
	Value stack[STACK_MAX]; //* The entire argument stack.
	Value* stackTop;				//* Pointer to the slot just PAST the top of the stack. 
	Obj* objects; 					//* A pointer to the first allocated object
	// stack == stackTop = > empty stack.
} VM;

// Declare vm struct so other files can access it.
extern VM vm;

/* Result of the VM's interpretation. */
typedef enum {
	INTERPRET_OK,							//* VM completed interpretation successfully.
	INTERPRET_COMPILE_ERROR,	//* VM encountered a compile-time error.
	INTERPRET_RUNTIME_ERROR		//* VM encountered a runtime-error.
} InterpretResult;

/** Initialize state of the VM. 
 * Includes resetting the stack.
 *
 */
void initVM();

/** Cleanup and free the state of the VM.
 * 
 */
void freeVM();

/** Performs interpretation of a input source text.
 * 
 * @param[in] source The source code to compile and interpret.
 * @return InterpreterResult representing the final state of the VM.
 */
InterpretResult interpret(const char* source);

/** Places a value onto the top of the stack. Increments the size of the stack.
 * 
 * @param[in] value The value to place.
 */
void push(Value value);

/** Removes the top value of the stack. Decrements the size of the stack.
 * 
 * @return Value The element which was last on top of the stack.
 */
Value pop();

#endif /* clox_vm_h */

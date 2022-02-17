#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

/** Define the Operation Codes which can exist in the bytecode. */
typedef enum {
	OP_CONSTANT,				//* OPCODE INDEX : (SE+1) Introduces a constant into the constant pool.
	OP_ADD,							//* OPCODE : (SE-1) Performs addition on the top 2 stack elements and pushes the result.
	OP_SUBTRACT,				//* OPCODE : (SE-1) Performs subtraction on the top 2 stack elements and pushes the result.
	OP_MULTIPLY,				//* OPCODE : (SE-1) Performs multiplication on the top 2 stack elements and pushes the result.
	OP_DIVIDE,					//* OPCODE : (SE-1) Performs division on the top 2 stack elements and pushes the result.
	OP_NEGATE,					//* OPCODE : (SE0) Negates the top element on the stack and pushes the result.
	OP_RETURN,					//* OPCODE : (SE0) Returns from the current function. Return value is the top element of the stack. Also implicitly closes all open upvalues.
	OP_NIL,							//* OPCODE : (SE+1) Introduces a 'nil' constant into the constant pool.
	OP_TRUE,						//* OPCODE : (SE+1) Introduces a 'true' constant into the constant pool.
	OP_FALSE,						//* OPCODE : (SE+1) Introduces a 'false' constant into the constant pool.
	OP_NOT,							//* OPCODE : (SE0) Inverts the truth value of the top element on the stack and pushes the result.
	OP_EQUAL,						//* OPCODE : (SE-1) Checks equality on the top 2 stack elements and pushes the result.
	OP_GREATER,					//* OPCODE : (SE-1) Checks if the top element is lesser than the 2nd highest element and pushes the result.
	OP_LESSER,					//* OPCODE : (SE-1) Checks if the top element is greater than the 2nd highest element and pushes the result.
	OP_PRINT,						//* OPCODE : (SE-1) Pops the top element of the stack and displays it to stdout.
	OP_POP,							//* OPCODE : (SE-1) Pops the top element of the stack.
	OP_DEFINE_GLOBAL,		//* OPCODE INDEX : (SE-1) Defines a global variable and assigns the top element to it.
	OP_GET_GLOBAL,			//* OPCODE INDEX : (SE+1) Retrieves a global variable by index and pushes its current value to the stack.
	OP_SET_GLOBAL,			//* OPCODE INDEX : (SE0) Retrieves a global variable and places the top of the stack at the indexed variable.
	OP_GET_LOCAL,				//* OPCODE INDEX : (SE+1) Retrieves a local variable by index from beneath the stack top and pushes it on top.
	OP_SET_LOCAL,				//* OPCODE INDEX : (SE0) Retrieves a local variable by index and overwrites its value in the stack with the top element of the stack.
	OP_JUMP,						//* OPCODE OFFSET OFFSET : (SE0) Unconditionally jumps forward a number of bytes equal to the given offset.
	OP_JUMP_IF_FALSE,		//* OPCODE OFFSET OFFSET : (SE0) If the popped element is false, jumps forward a number of bytes equal to the given offset.
	OP_LOOP,						//* OPCODE OFFSET OFFSET : (SE0) Unconditionally jumps backwards a number of bytes equal to the given offset.
	OP_CALL,						//* OPCODE NUMBER : (SE0) Creates a new frame over the stack a number of elements back equal to the operand and executes a function call.
	OP_CLOSURE,					//* OPCODE FUNCTION <2*i> : (SE0) Relies on ObjFunction's upvalueCount. First argument is the function being called. Other arguments come in pairs of 2. First byte is the locality of the upvalue and the second is the index.
	OP_GET_UPVALUE,			//* OPCODE INDEX : (SE+1) Retrieves a variable from the function's upvalue array using a given index and places it onto the stack.
	OP_SET_UPVALUE,			//* OPCODE INDEX : (SE0) Saves the top element of the stack to a position in the upvalue array using the given index.
	OP_CLOSE_UPVALUE,		//* OPCODE : (SE0) Updates the upvalue pointer location of the top element from a stack position to a storage location in the ObjUpvalue itself. This allows the variable to persist outside the stack.
} OpCode;

/* Note that the entire AST structure from jlox has been recreated just by 3 dynamic arrays, the chunk, constant pool and source line numbers */

/** Dynamic array representing a sequence of bytecode. Also contains a constant pool for constants introduced within the chunk. */
typedef struct {
	int count;						//* Number of elements within the ValueArray.
	int capacity; 				//* The maximum capacity of the ValueArray.
	uint8_t *code; 				//* An array of bytes which represents bytecode.
	int *lines; 					//* An array of integers to track where each instruction originated from in the source code.
	ValueArray constants; //* A dynamic array of values which make up the constant pool.
} Chunk;

/** Initialize the contents of an unitialzied Chunk pointer.
 * 
 * @param[out] chunk Pointer to be initialized.
 */
void initChunk(Chunk *chunk);

/** Writes a new byte to the end of a chunk. May enlarge @p chunk's memory allocation.
 * 
 * @param[in,out] chunk The chunk to be written to.
 * @param[in] byte The raw byte to place at the end of the chunk.
 * @param[in] line The source code line which generated the instruction.
 */
void writeChunk(Chunk *chunk, uint8_t byte, int line);

/** Append a new constant to a chunk's constant pool and return the constant pool index of the constant.
 * 
 * @param[in,out] chunk The chunk to place the constant in.
 * @param[in] value The constant to put into the constant pool.
 * @return The constant pool index of '@p value' input.
 */
int addConstant(Chunk *chunk, Value value);

/** Releases the memory held by a chunk and what it points to. Also resets metadata and nullifies pointer.
 *
 * @param[out] chunk The chunk to be freed.
 */
void freeChunk(Chunk *chunk);

#endif /* clox_chunk_h */

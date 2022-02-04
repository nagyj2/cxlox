#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

/** Define the Operation Codes which can exist in the bytecode. */
typedef enum {
	OP_CONSTANT_LONG,				//* OPCODE INDEX INDEX INDEX : (SE+1) Places a constant from the constant pool into the stack using a 24 bit address.
	OP_DEFINE_GLOBAL_LONG,	//* OPCODE INDEX INDEX INDEX : (SE-1) Defines a global variable using 24 bit index and assigns the top element to it.
	OP_GET_GLOBAL_LONG,			//* OPCODE INDEX INDEX INDEX : (SE0) Retrieves a global variable by 24 bit index and pushes its current value to the stack.
	OP_SET_GLOBAL_LONG,			//* OPCODE INDEX INDEX INDEX : (SE0) Retrieves a global variable by 24 bit index and places the top of the stack at the indexed variable.
	OP_CONDITIONAL,        	//* OPCODE : (SE-1) Pops 2 elements from the stack. If the 2nd element is truthy, the 1st element is pushed on the stack. Otherwise 'nil' is pushed.
	OP_OPTIONAL,						//* OPCODE : (SE-1) Pops 2 elements from the stack. If the 2nd element is 'nil', the 1st element is pushed. Otherwise the 2nd element is pushed.
	OP_POPN,								//* OPCODE NUMBER : (SE0) Pops n elements from the stack.
	OP_CONSTANT,				//* OPCODE INDEX : (SE+1) Introduces a constant into the constant pool.
	OP_ADD,							//* OPCODE : (SE-1) Performs addition on the top 2 stack elements and pushes the result.
	OP_SUBTRACT,				//* OPCODE : (SE-1) Performs subtraction on the top 2 stack elements and pushes the result.
	OP_MULTIPLY,				//* OPCODE : (SE-1) Performs multiplication on the top 2 stack elements and pushes the result.
	OP_DIVIDE,					//* OPCODE : (SE-1) Performs division on the top 2 stack elements and pushes the result.
	OP_NEGATE,					//* OPCODE : (SE0) Negates the top element on the stack and pushes the result.
	OP_RETURN,					//* OPCODE : (SE0) Returns from the current function.
	OP_NIL,							//* OPCODE : (SE+1) Places a 'nil' at the top of the stack.
	OP_TRUE,						//* OPCODE : (SE+1) Places a 'true' at the top of the stack.
	OP_FALSE,						//* OPCODE : (SE+1) Places a 'false' at the top of the stack.
	OP_NOT,							//* OPCODE : (SE0) Pops the top element, inverts the truthiness value and pushes the result.
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
} OpCode;

/** Structure to more efficiently track line numbers. Uses a dynamic array. */
typedef struct {
	int offset;	//* Offset the line starts at in the chunk.
	int line; 	//* Source line being represented. 
} LineStart;

/* Note that the entire AST structure from jlox has been recreated just by 3 dynamic arrays, the chunk, constant pool and source line numbers */

/** Dynamic array representing a sequence of bytecode. Also contains a constant pool for constants introduced within the chunk. */
typedef struct {
	int count;						//* Number of elements within the ValueArray.
	int capacity; 				//* The maximum capacity of the ValueArray.
	uint8_t *code; 				//* An array of bytes which represents bytecode.
	ValueArray constants; //* A dynamic array of values which make up the constant pool.
	int lineCount;				//* Number of lines being tracked.
	int lineCapacity;			//* Maximum capacity for lines being tracked.
	LineStart *lines;			//* Dynamic array of line counts. Decoupled from the chunk size because the number of elements in the chunk is greater than the number of lines.
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

/** Retrieve the line location of an instruction in a chunk. Assumes line numbers monotonically increase.
 * 
 * @param[in] chunk The chunk to retrieve the line number from.
 * @param[in] instruction An instruction offset to search for in the line number array.
 * @return int The offset of the LineStart array which contains the line @p instruction is on.
 */
int getLine(Chunk *chunk, int instruction);

/** Writes a new value to a chunk. If the number of constants is greater than 255, it will use OP_CONSTANT_LONG. Otherwise OP_CONSTANT.
 * 
 * @param[out] chunk The chunk to write to.
 * @param[in] value The value to write
 * @param[in] line The source line the value shows up on.
 */
void writeConstant(Chunk *chunk, Value value, int line);

#endif /* clox_chunk_h */

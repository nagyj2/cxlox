#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

/** Define the Operation Codes which can exist in the bytecode. */
typedef enum {
	OP_CONSTANT,		//* OPCODE INDEX : Opcode to introduce a constant into the constant pool.
	OP_ADD,					//* OPCODE : Opcode to represent mathematical addition.
	OP_SUBTRACT,		//* OPCODE : Opcode to represent mathematical subtraction.
	OP_MULTIPLY,		//* OPCODE : Opcode to represent mathematical multiplication.
	OP_DIVIDE,			//* OPCODE : Opcode to represent mathematical division.
	OP_NEGATE,			//* OPCODE : Opcode to remove the top stack element and place a negated version.
	OP_RETURN,			//* OPCODE : Opcode representing a function return.
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

/** Create a new Chunk. Pointer input gets initialized. 
 * 
 * @param[out] chunk The pointer to be initialized.
 */
void initChunk(Chunk *chunk);

/** Writes a new byte to a chunk. May enlarge @p chunk.
 * 
 * @param[in,out] chunk The chunk to be written to.
 * @param[in] byte The raw byte to put at the end of the chunk.
 * @param[in] line The source code line which generated the instruction.
 */
void writeChunk(Chunk *chunk, uint8_t byte, int line);

/** Add a new constant to a chunk's constant table.
 * 
 * @param[in,out] chunk The chunk to place the constant in.
 * @param[in] value The constant to put into the constant pool.
 * @return int The index for the @p value in the constant pool.
 */
int addConstant(Chunk *chunk, Value value);

/** Releases the memory held by a chunk and what it points to. Also resets metadata and nullifies pointer.
 *
 * @param[out] chunk The chunk to be freed.
 */
void freeChunk(Chunk *chunk);

#endif /* clox_chunk_h */

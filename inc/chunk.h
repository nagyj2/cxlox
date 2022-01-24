#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

/** Define the Operation Codes which can exist in the bytecode. */
typedef enum {
	OP_CONSTANT,		//* Opcode to introduce a constant into the constant pool.
	OP_CONSTANT_LONG,	//* Introduces a constant which is indexed by a 24-bit number
	OP_ADD,					//* OPCODE : Opcode to represent mathematical addition.
	OP_SUBTRACT,		//* OPCODE : Opcode to represent mathematical subtraction.
	OP_MULTIPLY,		//* OPCODE : Opcode to represent mathematical multiplication.
	OP_DIVIDE,			//* OPCODE : Opcode to represent mathematical division.
	OP_NEGATE,			//* OPCODE : Opcode to remove the top stack element and place a negated version.
	OP_RETURN,			//* OPCODE : Opcode representing a function return.
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

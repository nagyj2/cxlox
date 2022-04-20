#include <stdlib.h> // malloc

#include "chunk.h"
#include "memory.h"
#include "vm.h"

void initChunk(Chunk *chunk) {
	chunk->count = 0;
	chunk->capacity = 0;
	chunk->code = NULL;
	chunk->lines = NULL;
	chunk->lineCount = 0;
	chunk->lineCapacity = 0;
	initValueArray(&chunk->constants); // Takes pointer in and directly allocates memory
}

void writeChunk(Chunk *chunk, uint8_t byte, int line) {
	// Check to see if there is capacity. If not, grow the chunk
	if (chunk->capacity < chunk->count + 1) {
		int oldCapacity = chunk->capacity;
		// Set capacity to desired size
		chunk->capacity = GROW_CAPACITY(oldCapacity);
		// Move old data to the new chunk
		chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);

		// Dont grow lines automatically b/c its size is separate from the chunk size.
	}

	// Add byte to chunk
	chunk->code[chunk->count] = byte;
	chunk->count++;

	// If line being added is the same as the current line, return. Otherwise add an element, enlarging if need be.
	if (chunk->lineCount > 0 && chunk->lines[chunk->lineCount - 1].line == line) {
		return;
	}

	// If no capacity in line count, enlarge.
	if (chunk->lineCapacity < chunk->lineCount + 1) {
		int oldCapacity = chunk->lineCapacity;
		chunk->lineCapacity = GROW_CAPACITY(oldCapacity);
		chunk->lines = GROW_ARRAY(LineStart, chunk->lines, oldCapacity, chunk->lineCapacity);
	}

	// B/c we are creating a new line counter, grab the reference and initialize its values
	LineStart *lineStart = &(chunk->lines[chunk->lineCount++]);
	lineStart->offset = chunk->count - 1; // The first instruction in the new line
	lineStart->line = line; // The line being represented.
}

int getLine(Chunk *chunk, int instruction) {
	// Perform binary search for the starting position.
	int start = 0;
	int end = chunk->lineCount - 1;

	for (;;) {
		int mid = (start + end) / 2;
		// Retrieve middle line in the lines array
		LineStart *line = &chunk->lines[mid];
		// If line is lower than the determined offset, set current line to the higher end
		if (instruction < line->offset) {
			end = mid - 1;
		// If indexing a particular chunk's middle and the instruction offset is lesser than the next offset, the line number has been found
		} else if (mid == chunk->lineCount - 1 || instruction < chunk->lines[mid + 1].offset) {
			return line->line;
		// B/c process of elimination, raise the bottom line
		} else {
			start = mid + 1;
		}
	}
}

int addConstant(Chunk* chunk, Value value) {
	push(value); // Push for GC
	writeValueArray(&chunk->constants, value);
	pop();
	// 'chunk->constant' dereferences to the exact location in memory where the constant pool is. Therefore, use '.' to get data
	return chunk->constants.count - 1; // Return location to new constant
}

// void writeConstant(Chunk *chunk, Value value, int line) {
// 	int index = addConstant(chunk, value);
// 	if (index < 255) {
// 		writeChunk(chunk, OP_CONSTANT, line); 		// Write the instruction
// 		writeChunk(chunk, (uint8_t) index, line);	// Write the index
// 	} else {
// 		writeChunk(chunk, OP_CONSTANT_LONG, line);	// Write the instruction. Next is the number in small endian
// 		// 0bxxxxxxxxxxxxxxxxxxxxxxxx < Binary index
// 		// 0b333333332222222211111111 < Encoding order
// 		writeChunk(chunk, (uint8_t) (index & 0xff), line); // Write the FIRST 8 (1-8) bytes
// 		writeChunk(chunk, (uint8_t) ((index >> 8)  & 0xff), line); // Next 8 (9-16)...
// 		writeChunk(chunk, (uint8_t) ((index >> 16) & 0xff), line); // Next 8 (17-24)...
// 	}
// }

void freeChunk(Chunk *chunk) {
	FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
	FREE_ARRAY(LineStart, chunk->lines, chunk->lineCapacity);
	freeValueArray(&chunk->constants); // Need to call to also free ValueArray's metadata
	initChunk(chunk); // Zeros out the chunk's count and capacity fields and nullifies the pointer
}

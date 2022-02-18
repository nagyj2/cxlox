#include <stdlib.h> // malloc

#include "chunk.h"
#include "memory.h"
#include "vm.h"

void initChunk(Chunk *chunk) {
	chunk->count = 0;
	chunk->capacity = 0;
	chunk->code = NULL;
	chunk->lines = NULL;
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
		chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
	}

	// Add byte to chunk
	chunk->code[chunk->count] = byte;
	chunk->lines[chunk->count] = line;
	chunk->count++;
}

int addConstant(Chunk* chunk, Value value) {
	push(value); // Push for GC
	writeValueArray(&chunk->constants, value);
	pop();
	// 'chunk->constant' dereferences to the exact location in memory where the constant pool is. Therefore, use '.' to get data
	return chunk->constants.count - 1; // Return location to new constant
}

void freeChunk(Chunk *chunk) {
	FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
	FREE_ARRAY(int, chunk->lines, chunk->capacity);
	freeValueArray(&chunk->constants); // Need to call to also free ValueArray's metadata
	initChunk(chunk); // Zeros out the chunk's count and capacity fields and nullifies the pointer
}

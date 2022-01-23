#include <stdio.h> // printf

#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char *argvp[]) {
	Chunk chunk;
	initChunk(&chunk);

	for (int i = 0, j = 0; i < 280; i++) {
		// int constant = addConstant(&chunk, 1.0 * i);
		writeConstant(&chunk, 1.0 * i, j);
		if (i % 10 == 0) {
			j++;
		}
	}
	writeChunk(&chunk, OP_RETURN, 28);

	disassembleChunk(&chunk, "test chunk");
	freeChunk(&chunk);

	return 0;
}

#include <stdio.h> // printf

#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char *argvp[]) {
	Chunk chunk;
	initChunk(&chunk);

	int constant = addConstant(&chunk, 1.3);
	writeChunk(&chunk, OP_CONSTANT, 001);
	writeChunk(&chunk, constant, 001);
	writeChunk(&chunk, OP_RETURN, 002);

	disassembleChunk(&chunk, "test chunk");
	freeChunk(&chunk);

	return 0;
}

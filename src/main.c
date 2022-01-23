#include <stdio.h> // printf

#include "common.h"
#include "chunk.h"
#include "vm.h"
#include "debug.h"

int main(int argc, const char *argvp[]) {
	initVM();
	
	Chunk chunk;
	initChunk(&chunk);

	writeConstant(&chunk, 1.0, 1);
	writeConstant(&chunk, 2.0, 1);
	writeChunk(&chunk, OP_MULTIPLY, 1);
	writeConstant(&chunk, 3.0, 1);
	writeChunk(&chunk, OP_ADD, 1);
	writeChunk(&chunk, OP_RETURN, 1);

	// disassembleChunk(&chunk, "test chunk");
	interpret(&chunk);
	freeVM();
	freeChunk(&chunk);

	return 0;
}

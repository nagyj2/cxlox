#include <stdio.h> // printf

#include "common.h"
#include "chunk.h"
#include "vm.h"
#include "debug.h"

int main(int argc, const char *argvp[]) {
	initVM();
	
	Chunk chunk;
	initChunk(&chunk);

	int constant = addConstant(&chunk, 1.3);
	writeChunk(&chunk, OP_CONSTANT, 1);
	writeChunk(&chunk, constant, 1);

	constant = addConstant(&chunk, 3.4);
	writeChunk(&chunk, OP_CONSTANT, 1);
	writeChunk(&chunk, constant, 1);

	writeChunk(&chunk, OP_ADD, 1);

	constant = addConstant(&chunk, 5.6);
	writeChunk(&chunk, OP_CONSTANT, 1);
	writeChunk(&chunk, constant, 1);

	writeChunk(&chunk, OP_DIVIDE, 001);
	writeChunk(&chunk, OP_NEGATE, 002);
	writeChunk(&chunk, OP_RETURN, 003);

	// disassembleChunk(&chunk, "test chunk");
	interpret(&chunk);
	freeVM();
	freeChunk(&chunk);

	return 0;
}

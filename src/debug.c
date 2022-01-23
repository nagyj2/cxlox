#include <stdio.h>

#include "debug.h"
#include "value.h"

void disassembleChunk(Chunk *chunk, const char *name) {
	printf("== %s ==\n", name);

	for (int offset = 0; offset < chunk->count; ) {
		// Set offset via disassemble chunk b/c instructions have different lengths
		offset = disassembleInstruction(chunk, offset);
	}
}

/* Decodes an instruction which has no extra data. Opcode is the only data for this instruction. */
static int simpleInstruction(const char *name, int offset) {
	printf("%s\n", name);
	return offset + 1;
}

/* Decodes an instruction which is followed by an index to the constant pool. */
static int constantInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t constantIndex = chunk->code[offset + 1];
	printf("%-16s %4d '", name, constantIndex);
	printValue(chunk->constants.values[constantIndex]);
	printf("'\n");

	return offset + 2; // Return forward 2 spaces b/c opcode and index need to be skipped
}

/* Decodes an instruction which is followed by a long index. */
static int constantLongInstruction(const char *name, Chunk *chunk, int offset) {
	int constantIndex = (chunk->code[offset + 1]) |
											(chunk->code[offset + 2] << 8) |
											(chunk->code[offset + 3] << 16); // Reassemble number from the 3 parts
	printf("%-16s %4d '", name, constantIndex);
	printValue(chunk->constants.values[constantIndex]);
	printf("'\n");

	return offset + 4; // Return forward 4 bytes
}

int disassembleInstruction(Chunk *chunk, int offset) {
	printf("%04d ", offset); // Display instruction offset
	int line = getLine(chunk, offset); // B/c we now have a helper, we need to use that to get the proper line number
	if (offset > 0 && line == getLine(chunk, offset - 1)) {
		printf("   | ");
	} else {
		printf("%4d ", line);
	}

	uint8_t instruction = chunk->code[offset]; // Retrieve opcode
	switch (instruction) {
		case OP_RETURN:
			return simpleInstruction("OP_RETURN", offset);
		case OP_CONSTANT:
			return constantInstruction("OP_CONSTANT", chunk, offset);
		case OP_CONSTANT_LONG:
			return constantLongInstruction("OP_CONSTANT_LONG", chunk, offset);
		default:
			printf("Unknown opcode %d\n", instruction);
			return offset + 1;
	}
}

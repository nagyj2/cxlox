#include <stdio.h>

#include "debug.h"
#include "value.h"

void disassembleChunk(Chunk* chunk, const char* name) {
	printf("== %s ==\n", name);

	for (int offset = 0; offset < chunk->count; ) {
		// Set offset via disassemble chunk b/c instructions have different lengths
		offset = disassembleInstruction(chunk, offset);
	}
}

/** Decodes an 8-byte instruction.
 * 
 * @param[in] name Name to give to the decoded instruction.
 * @param[in] offset Offset position of this instruction opcode.
 * @return int index of the next instruction's opcode.
 */
static int simpleInstruction(const char* name, int offset) {
	printf("%s\n", name);
	return offset + 1;
}

/** Decodes an 8-byte instruction with an 8-byte operand.
 * 
 * @param[in] name Name to give the decoded instruction.
 * @param[in] chunk Chunk to retrieve the operand from.
 * @param[in] offset Offset position of this instruction opcode.
 * @return int index of the next instruction's opcode.
 */
static int constantInstruction(const char *name, Chunk *chunk, int offset) {
	uint8_t constantIndex = chunk->code[offset + 1];
	printf("%-16s %4d '", name, constantIndex);
	printValue(chunk->constants.values[constantIndex]);
	printf("'\n");

	return offset + 2; // Return forward 2 spaces b/c opcode and index need to be skipped
}


int disassembleInstruction(Chunk *chunk, int offset) {
	printf("%04d ", offset); // Display instruction offset
	if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
		printf("   | ");
	} else {
		printf("%4d ", chunk->lines[offset]);
	}

	uint8_t instruction = chunk->code[offset]; // Retrieve opcode
	switch (instruction) {
		case OP_RETURN:
			return simpleInstruction("OP_RETURN", offset);
		case OP_ADD:
			return simpleInstruction("OP_ADD", offset);
		case OP_SUBTRACT:
			return simpleInstruction("OP_SUBTRACT", offset);
		case OP_MULTIPLY:
			return simpleInstruction("OP_MULTIPLY", offset);
		case OP_DIVIDE:
			return simpleInstruction("OP_DIVIDE", offset);
		case OP_CONSTANT:
			return constantInstruction("OP_CONSTANT", chunk, offset);
		case OP_NEGATE:
			return simpleInstruction("OP_NEGATE", offset);
		default:
			printf("Unknown opcode %d\n", instruction);
			return offset + 1;
	}
}

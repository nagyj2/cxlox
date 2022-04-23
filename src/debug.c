#include <stdio.h>

#include "debug.h"
#include "value.h"
#include "object.h"

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
	printf("%-24s %4d '", name, constantIndex);
	printValue(chunk->constants.values[constantIndex]);
	printf("'\n");

	return offset + 2; // Return forward 2 spaces b/c opcode and index need to be skipped
}

/* Decodes an instruction which is followed by a long index. */
static int constantLongInstruction(const char *name, Chunk *chunk, int offset) {
	int constantIndex = (chunk->code[offset + 1]) |
											(chunk->code[offset + 2] << 8) |
											(chunk->code[offset + 3] << 16); // Reassemble number from the 3 parts
	printf("%-24s %4d '", name, constantIndex);
	printValue(chunk->constants.values[constantIndex]);
	printf("'\n");

	return offset + 4; // Return forward 4 bytes
}

static int byteInstruction(const char* name, Chunk* chunk, int offset) {
	uint8_t slot = chunk->code[offset + 1];
	printf("%-24s %4d\n", name, slot);
	return offset + 2;
}

static int jumpInstruction(const char* name, int sign, Chunk* chunk, int offset) {
	uint16_t jump = (uint16_t) ((chunk->code[offset + 1] << 8) | chunk->code[offset + 2]);
	printf("%-24s %4d -> %d\n", name, offset, offset + 3 + sign * jump); // Display this location and where it goes to
	return offset + 3;
}

static int shortInstruction(const char* name, Chunk* chunk, int offset) {
	uint16_t slot = (uint16_t) ((chunk->code[offset + 1] << 8) | chunk->code[offset + 2]);
	printf("%-24s %4d\n", name, slot);
	return offset + 3;
}

static int invokeInstruction(const char* name, Chunk* chunk, int offset) {
	uint8_t constant = chunk->code[offset + 1];
	uint8_t argCount = chunk->code[offset + 2];
	printf("%-24s %4d'", name, constant);
	printValue(chunk->constants.values[constant]);
	printf("\n                                   %4d args'\n", argCount);
	return offset + 3;
}

static int invokeLongInstruction(const char* name, Chunk* chunk, int offset) {
	int constant = 	(chunk->code[offset + 1]) |
									(chunk->code[offset + 2] << 8) |
									(chunk->code[offset + 3] << 16);
	uint8_t argCount = chunk->code[offset + 4];
	printf("%-24s %4d'", name, constant);
	printValue(chunk->constants.values[constant]);
	printf("\n                                   %4d args'\n", argCount);
	return offset + 5;
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
		case OP_DEFINE_GLOBAL_LONG:
			return constantLongInstruction("OP_DEFINE_GLOBAL_LONG", chunk, offset);
		case OP_GET_GLOBAL_LONG:
			return constantLongInstruction("OP_GET_GLOBAL_LONG", chunk, offset);
		case OP_SET_GLOBAL_LONG:
			return constantLongInstruction("OP_SET_GLOBAL_LONG", chunk, offset);
		case OP_GET_PROPERTY_LONG:
			return constantLongInstruction("OP_GET_PROPERTY_LONG", chunk, offset);
		case OP_SET_PROPERTY_LONG:
			return constantLongInstruction("OP_SET_PROPERTY_LONG", chunk, offset);
		case OP_GET_PROP_SAFE_LONG:
			return constantLongInstruction("OP_GET_PROP_SAFE_LONG", chunk, offset);
		// case OP_SET_PROP_SAFE_LONG:
		// 	return constantLongInstruction("OP_SET_PROP_SAFE_LONG", chunk, offset);
		case OP_DEFINE_CONST_LONG:
			return constantLongInstruction("OP_DEFINE_CONST_LONG", chunk, offset);
		case OP_CONSTANT_LONG:
			return constantLongInstruction("OP_CONSTANT_LONG", chunk, offset);
		case OP_GET_PROP_SAFE:
			return constantInstruction("OP_GET_PROP_SAFE", chunk, offset);
		case OP_CLASS_LONG:
			return constantLongInstruction("OP_CLASS_LONG", chunk, offset);
		case OP_METHOD_LONG:
			return constantLongInstruction("OP_METHOD_LONG", chunk, offset);
		case OP_CLOSURE_LONG: {
			offset++;
			uint32_t constant = (chunk->code[offset + 0]) |
													(chunk->code[offset + 1] << 8) |
													(chunk->code[offset + 2] << 16); // Reassemble number from the 3 parts
			offset = offset + 3;
			printf("%-24s %4d ", "OP_CLOSURE_LONG", constant);
			printValue(chunk->constants.values[constant]);
			printf("\n");

			ObjFunction* function = AS_FUNCTION(chunk->constants.values[constant]);
			for (int j = 0; j < function->upvalueCount; j++) {
				int isLocal = chunk->code[offset++];
				int index = chunk->code[offset++];
				// 'local' means MY local is referenced by someone else. 'upvalue' means SOMEONE ELSE'S local is referenced.
				printf("%04d    |                             %s %d\n", offset - 2, isLocal ? "local" : "upvalue", index);
			}
			return offset;
		}
#ifdef USE_STACK_PROPERTY_DELETE
		case OP_DEL_PROPERTY:
			return simpleInstruction("OP_DEL_PROPERTY", offset);
#else
		case OP_DEL_PROPERTY:
			return constantInstruction("OP_DEL_PROPERTY", chunk, offset);
		case OP_DEL_PROPERTY_LONG:
			return constantLongInstruction("OP_DEL_PROPERTY_LONG", chunk, offset);
#endif
		// case OP_SET_PROP_SAFE:
		// 	return constantInstruction("OP_SET_PROP_SAFE", chunk, offset);
		case OP_CONDITIONAL:
			return simpleInstruction("OP_CONDITIONAL", offset);
		case OP_OPTIONAL:
			return simpleInstruction("OP_OPTIONAL", offset);
		case OP_POPN:
			return byteInstruction("OP_POPN", chunk, offset);
		case OP_POPREPL:
			return simpleInstruction("OP_POPREPL", offset);
		case OP_DUP:
			return simpleInstruction("OP_DUP", offset);
		case OP_DEFINE_CONST:
			return constantInstruction("OP_DEFINE_CONST", chunk, offset);
		case OP_INVOKE_SAFE:
			return invokeInstruction("OP_INVOKE_SAFE", chunk, offset);
		case OP_INVOKE_LONG:
			return invokeLongInstruction("OP_INVOKE_LONG", chunk, offset);
		case OP_INVOKE_SAFE_LONG:
			return invokeLongInstruction("OP_INVOKE_SAFE_LONG", chunk, offset);
			
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
		case OP_EQUAL:
			return simpleInstruction("OP_EQUAL", offset);
		case OP_GREATER:
			return simpleInstruction("OP_GREATER", offset);
		case OP_LESSER:
			return simpleInstruction("OP_LESSER", offset);
		case OP_CONSTANT:
			return constantInstruction("OP_CONSTANT", chunk, offset);
		case OP_NEGATE:
			return simpleInstruction("OP_NEGATE", offset);
		case OP_NOT:
			return simpleInstruction("OP_NOT", offset);
		case OP_TRUE:
			return simpleInstruction("OP_TRUE", offset);
		case OP_FALSE:
			return simpleInstruction("OP_FALSE", offset);
		case OP_NIL:
			return simpleInstruction("OP_NIL", offset);
		case OP_PRINT:
			return simpleInstruction("OP_PRINT", offset);
		case OP_POP:
			return simpleInstruction("OP_POP", offset);
		case OP_DEFINE_GLOBAL:
			return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
		case OP_GET_GLOBAL:
			return constantInstruction("OP_GET_GLOBAL", chunk, offset);
		case OP_SET_GLOBAL:
			return constantInstruction("OP_SET_GLOBAL", chunk, offset);
		case OP_GET_LOCAL:
			return byteInstruction("OP_GET_LOCAL", chunk, offset);
		case OP_SET_LOCAL:
			return byteInstruction("OP_SET_LOCAL", chunk, offset);
		case OP_JUMP:
			return jumpInstruction("OP_JUMP", 1, chunk, offset);
		case OP_JUMP_IF_FALSE:
			return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
		case OP_LOOP:
			return jumpInstruction("OP_LOOP", -1, chunk, offset);
		case OP_CALL:
			return byteInstruction("OP_CALL", chunk, offset);
		case OP_CLOSURE: {
			offset++;
			uint8_t constant = chunk->code[offset++];
			printf("%-24s %4d ", "OP_CLOSURE", constant);
			printValue(chunk->constants.values[constant]);
			printf("\n");

			ObjFunction* function = AS_FUNCTION(chunk->constants.values[constant]);
			for (int j = 0; j < function->upvalueCount; j++) {
				int isLocal = chunk->code[offset++];
				int index = chunk->code[offset++];
				// 'local' means MY local is referenced by someone else. 'upvalue' means SOMEONE ELSE'S local is referenced.
				printf("%04d    |                             %s %d\n", offset - 2, isLocal ? "local" : "upvalue", index);
			}
			return offset;
		}
		case OP_GET_UPVALUE:
			return byteInstruction("OP_GET_UPVALUE", chunk, offset);
		case OP_SET_UPVALUE:
			return byteInstruction("OP_SET_UPVALUE", chunk, offset);
		case OP_CLOSE_UPVALUE:
			return simpleInstruction("OP_CLOSE_UPVALUE", offset);
		case OP_CLASS:
			return constantInstruction("OP_CLASS", chunk, offset);
		case OP_GET_PROPERTY:
			return constantInstruction("OP_GET_PROPERTY", chunk, offset);
		case OP_SET_PROPERTY:
			return constantInstruction("OP_SET_PROPERTY", chunk, offset);
		case OP_METHOD:
			return constantInstruction("OP_METHOD", chunk, offset);
		case OP_INVOKE:
			return invokeInstruction("OP_INVOKE", chunk, offset);
		case OP_INHERIT:
			return simpleInstruction("OP_INHERIT", offset);
		case OP_GET_SUPER:
			return constantInstruction("OP_GET_SUPER", chunk, offset);
		case OP_SUPER_INVOKE:
			return invokeInstruction("OP_SUPER_INVOKE", chunk, offset);
		default:
			printf("Unknown opcode %d\n", instruction);
			return offset + 1;
	}
}

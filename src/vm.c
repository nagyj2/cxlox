#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "common.h"
#include "vm.h"
#include "debug.h"
#include "compiler.h" // Needed?
#include "object.h"
#include "memory.h"

// Declare the VM in global scope. Prevents needing to pass it as an argument everywhere.
// By passing the VM as a pointer to functions, it is easier to have multiple VMs and pass them around in a host language.
VM vm;

//~ VM Initialization and Deinitialization

/** Modifies the VM stack to be empty.
 * 
 */
void resetStack() {
	// Doesnt matter if stack elements are freed b/c they are still in v.stack's allocation AND vn.stackTop will just overwrite new values.
	vm.stackTop = vm.stack;
}

void initVM() {
	resetStack();
	vm.objects = NULL;
}

void freeVM() {
	freeObjects();
}

//~ Error Reporting

/** Prints an error message to stderr. Also resets the stack.
 * 
 * @param[in] format Format string to print to stderr.
 * @param[in] ... The arguments to put into the format string.
 */
static void runtimeError(const char* format, ...) {
	// Print the message to stderr
	va_list args;
	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	fputs("\n", stderr);

	// Display the line that caused the error
	size_t instruction = vm.ip - vm.chunk->code - 1;
	int line = vm.chunk->lines[instruction];
	fprintf(stderr, "[line %d] in script\n", line);

	// Reset the stack
	resetStack();
}

//~ Stack Management

void push(Value value) {
	*vm.stackTop = value; // Dereference top and place a value there. Remember, stackTop points PAST the last element
	vm.stackTop++; 				// Increment the size of the stack 
}

Value pop() {
	vm.stackTop--;
	return *vm.stackTop;
}

/** Returns the value at a given index from the top of the stack.
 * @pre there is at least one element in the stack.
 *
 * @param[in] distance How far down the stack to look.
 * @return Value at the given position from the top in the stack.
 */
static Value peek(int distance) {
	return vm.stackTop[-1 - distance];
}

//~ Lox Semantics

/** Returns whether the input value is falsey in lox.
 * 
 * @param[in] value Value to test.
 * @return true if the value is truthy.
 * @return false if the value is falsey.
 */
static bool isFalsey(Value value) {
	return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value)); // Use shortcircuiting for bool conversion
}

static void concatenate() {
	ObjString* b = AS_STRING(pop());
	ObjString* a = AS_STRING(pop());

	int length = a->length + b->length;
	char* chars = ALLOCATE(char, length + 1);
	memcpy(chars, a->chars, a->length);
	memcpy(chars + a->length, b->chars, b->length);
	chars[length] = '\0';

	ObjString* result = takeString(chars, length);
	push(OBJ_VAL(result));
}

//~ VM Execution

static InterpretResult run() {
	// Define returns the next byte while incrementing the counter
#define READ_BYTE() (*vm.ip++)
	// Read a constant from the bytecode by taking the index and then looking it up in the constant pool
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
	// Pop two elements from the stack, add them and then place the result back. Remember, left arg is placed first
	// Uses a do loop to allow multiple lines AND a culminating semicolon
#define BINARY_OP(valueType, op) \
	do { \
		if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
			runtimeError("Operands must be numbers."); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
		double b = AS_NUMBER(pop()); \
		double a = AS_NUMBER(pop()); \
		push(valueType(a op b)); \
	} while (false)

	for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
		// Print the stack contents before disassembling the instruction
		printf("        ");
		for (Value *slot = vm.stack; slot < vm.stackTop; slot++) {
			printf("[ ");
			printValue(*slot);
			printf(" ]");
		}
		printf("\n");
		disassembleInstruction(vm.chunk, (int) (vm.ip - vm.chunk->code)); // Perform some math to get offset
#endif
		uint8_t instruction;
		switch (instruction = READ_BYTE()) {
			case OP_ADD: {
				if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
					concatenate();
				} else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
					double b = AS_NUMBER(pop());
					double a = AS_NUMBER(pop());
					push(NUMBER_VAL(a + b));
				} else {
					runtimeError("Operands must be two numbers or two strings.");
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			case OP_SUBTRACT: {
				BINARY_OP(NUMBER_VAL, -);
				break;
			}
			case OP_MULTIPLY: {
				BINARY_OP(NUMBER_VAL, *);
				break;
			}
			case OP_DIVIDE: {
				BINARY_OP(NUMBER_VAL, /);
				break;
			}
			case OP_GREATER: {
				BINARY_OP(BOOL_VAL, >);
				break;
			}
			case OP_LESSER: {
				BINARY_OP(BOOL_VAL, <);
				break;
			}
			case OP_RETURN: {
				printValue(pop());
				printf("\n");
				return INTERPRET_OK;
			}
			case OP_CONSTANT: {
				Value constant = READ_CONSTANT();
				push(constant);
				break;
			}
			case OP_NEGATE: {
				// Ensure stack is a number.
				if (!IS_NUMBER(peek(0))) {
					runtimeError("Operand must be a number.");
					return INTERPRET_RUNTIME_ERROR;
				}
				push(NUMBER_VAL(-AS_NUMBER(pop())));
				break;
			}
			case OP_TRUE: {
				push(BOOL_VAL(true));
				break;
			}
			case OP_FALSE: {
				push(BOOL_VAL(false));
				break;
			}
			case OP_NIL: {
				push(NIL_VAL);
				break;
			}
			case OP_NOT: {
				push(BOOL_VAL(isFalsey(pop())));
				break;
			}
			case OP_EQUAL: {
				Value b = pop();
				Value a = pop();
				push(BOOL_VAL(valuesEqual(a, b)));
				break;
			}
		}
	}

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
	
}

InterpretResult interpret(const char* source) {
	// Create chunk to store source
	Chunk chunk;
	initChunk(&chunk);

	// If the chunk did not compile, return the error
	if (!compile(source, &chunk)) {
		freeChunk(&chunk);
		return INTERPRET_COMPILE_ERROR;
	}

	// Setup the VM's references to the chunk
	vm.chunk = &chunk;
	vm.ip = vm.chunk->code;

	// Run the VM
	InterpretResult result = run();

	freeChunk(&chunk);
	return result;
}


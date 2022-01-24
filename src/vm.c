#include <stdio.h>

#include "common.h"
#include "vm.h"
#include "debug.h"

// Declare the VM in global scope. Prevents needing to pass it as an argument everywhere.
// By passing the VM as a pointer to functions, it is easier to have multiple VMs and pass them around in a host language.
VM vm;

/** Modifies the VM stack to be empty.
 * 
 */
void resetStack() {
	vm.stackTop = vm.stack; // Creates an empty stack
}

void initVM() {
	resetStack();
}

void freeVM() {
	
}

void push(Value value) {
	*vm.stackTop = value; // Dereference top and place a value there. Remember, stackTop points PAST the last element
	vm.stackTop++; 				// Increment the size of the stack 
}

Value pop() {
	vm.stackTop--;
	return *vm.stackTop;
}

static InterpretResult run() {
	// Define returns the next byte while incrementing the counter
#define READ_BYTE() (*vm.ip++)
	// Read a constant from the bytecode by taking the index and then looking it up in the constant pool
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
	// Pop two elements from the stack, add them and then place the result back. Remember, left arg is placed first
#define BINARY_OP(op) \
	do { \
		double b = pop(); \
		double a = pop(); \
		push(a op b); \
	} while (false)

	for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
		// Print the stack contents before disassembling the instruction
		printf("           ");
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
				BINARY_OP(+);
				break;
			}
			case OP_SUBTRACT: {
				BINARY_OP(-);
				break;
			}
			case OP_MULTIPLY: {
				BINARY_OP(*);
				break;
			}
			case OP_DIVIDE: {
				BINARY_OP(/);
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
				printf("\n");
				break;
			}
			case OP_NEGATE: {
				push(-pop()); // Will need to be changes once Value gets more complex
				break;
			}
		}
	}

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
	
}

InterpretResult interpret(const char* source) {
	compile(source);
	return INTERPRET_OK;
}


#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "common.h"
#include "vm.h"
#include "compiler.h"
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
	initTable(&vm.strings); // Initialize the string internment table.
	initTable(&vm.globals);
	initTable(&vm.constants);
}

void freeVM() {
	freeObjects();
	freeTable(&vm.strings);
	freeTable(&vm.globals);
	freeTable(&vm.constants);
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
	size_t instruction = vm.ip - vm.chunk->code - 1; // Instruction offset
	int line = getLine(vm.chunk, instruction);
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
	// Reads a constant which has a 24 bit address
#define READ_CONSTANT_LONG() (vm.chunk->constants.values[(READ_BYTE()) | (READ_BYTE() << 8) | (READ_BYTE() << 16)])
	// Read a constant from the bytecode and convert it to a string
#define READ_STRING() AS_STRING(READ_CONSTANT())
	// Read a constant from the bytecode and convert it to a string using a 24 bit address
#define READ_STRING_LONG() AS_STRING(READ_CONSTANT_LONG())
	// Read a 2 byte chunk of code
#define READ_SHORT() (vm.ip += 2, (uint16_t)((vm.ip[-2] << 8) | vm.ip[-1]))
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
	// Raise a runtime error if the seeked variable is a constant
#define ERROR_IF_CONST(name) \
	do { \
		if (tableGet(&vm.constants, name, &name)) { \
			runtimeError("Cannot redefine constant"); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
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
				if (IS_STRING(peek(1)) || IS_STRING(peek(0))) {
					ObjString* b = toObjString(pop()); // Convert to strings. Not neccesary for strings
					ObjString* a = toObjString(pop());
					push(OBJ_VAL(a)); // Push as string values to the stack in inverse order to popping
					push(OBJ_VAL(b));
					concatenate();
				// } else if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
				// 	concatenate();
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
				BINARY_OP(NUMBER_VAL, / );
				break;
			}
			case OP_GREATER: {
				BINARY_OP(BOOL_VAL, > );
				break;
			}
			case OP_LESSER: {
				BINARY_OP(BOOL_VAL, < );
				break;
			}
			case OP_RETURN: {
				// Exit interpreter
				return INTERPRET_OK;
			}
			case OP_CONSTANT: {
				Value constant = READ_CONSTANT();
				push(constant);
				break;
			}
			case OP_CONSTANT_LONG: {
				Value constant = READ_CONSTANT_LONG();
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
			case OP_PRINT: {
				printValue(pop());
				printf("\n");
				break;
			}
			case OP_POP: {
				pop();
				break;
			}
			case OP_DEFINE_GLOBAL: {
				Value name = READ_CONSTANT();
				ERROR_IF_CONST(name);
				tableSet(&vm.globals, name, peek(0)); // Place the value into the hash table BEFORE popping it so it doesnt get picked up by garbage collection
				pop();
				break;
			}
			case OP_DEFINE_GLOBAL_LONG: {
				Value name = READ_CONSTANT_LONG();
				ERROR_IF_CONST(name);
				tableSet(&vm.globals, name, peek(0)); // Place the value into the hash table BEFORE popping it so it doesnt get picked up by garbage collection
				pop();
				break;
			}
			case OP_DEFINE_CONST: {
				Value name = READ_CONSTANT();
				ERROR_IF_CONST(name);
				tableSet(&vm.constants, name, peek(0)); // Place the value into the hash table BEFORE popping it so it doesnt get picked up by garbage collection
				pop();
				break;
			}
			case OP_DEFINE_CONST_LONG: {
				Value name = READ_CONSTANT_LONG();
				ERROR_IF_CONST(name);
				tableSet(&vm.constants, name, peek(0)); // Place the value into the hash table BEFORE popping it so it doesnt get picked up by garbage collection
				pop();
				break;
			}
			case OP_GET_GLOBAL: {
				Value name = READ_CONSTANT(); // Will always be a string
				Value value; // Output value
				if (!(tableGet(&vm.globals, name, &value) || tableGet(&vm.constants, name, &value))) { // Unlike string internment, globals is simply indexed by strings.
					runtimeError("Undefined variable '%s'.", AS_STRING(name)->chars);
					return INTERPRET_RUNTIME_ERROR;
				}
				push(value);
				break;
			}
			case OP_GET_GLOBAL_LONG: {
				Value name = READ_CONSTANT_LONG(); // Will always be a string
				Value value;
				if (!(tableGet(&vm.globals, name, &value) || tableGet(&vm.constants, name, &value))) { // If neither return a value, error
					runtimeError("Undefined variable '%s'.", AS_STRING(name)->chars);
					return INTERPRET_RUNTIME_ERROR;
				}
				push(value);
				break;
			}
			case OP_SET_GLOBAL: {
				Value name = READ_CONSTANT();
				ERROR_IF_CONST(name);
				if (tableSet(&vm.globals, name, peek(0))) { // If this was a new entry, the variable didnt exist
					tableDelete(&vm.globals, name); // Roll back change (important for REPL)
					runtimeError("Undefined variable '%s'.", AS_STRING(name)->chars);
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}								
			case OP_SET_GLOBAL_LONG: {
				Value name = READ_CONSTANT_LONG();
				ERROR_IF_CONST(name);
				if (tableSet(&vm.globals, name, peek(0))) { // If this was a new entry, the variable didnt exist
					tableDelete(&vm.globals, name); // Roll back change (important for REPL)
					runtimeError("Undefined variable '%s'.", AS_STRING(name)->chars);
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			case OP_CONDITIONAL: { // a ? b evaluates to b if a is truthy, otherwise it evaluates to nil
				Value b = pop(); // False branch value
				Value a = pop(); // True branch value
				push(isFalsey(a) ? NIL_VAL : b);
				break;
			}							
			case OP_OPTIONAL: { // a : b evaluates to a if a is not nil, otherwise it evaluates to b
				Value b = pop(); // False branch value
				Value a = pop(); // True branch value
				push(valuesEqual(a, NIL_VAL) ? b : a);
				break;
			}
			case OP_POPN: {
				int n = (int) READ_BYTE();
				while (n > 0) {
					pop();
					n--;
				}
				break;
			}
			case OP_POPREPL: {
				if (vm.isREPL) {
					printValue(pop());
					printf("\n");
				} else {
					pop();
				}
				break;
			}
			case OP_GET_LOCAL: {
				uint8_t slot = READ_BYTE();
				push(vm.stack[slot]);
				break;
			}
			case OP_SET_LOCAL: {
				uint8_t slot = READ_BYTE();
				vm.stack[slot] = peek(0); // redefines element already in the stack
				break;
			}
			case OP_JUMP: {
				uint16_t offset = READ_SHORT();
				vm.ip += offset;
				break;
			}
			case OP_JUMP_IF_FALSE: {
				uint16_t offset = READ_SHORT();
				if (isFalsey(peek(0)))
					vm.ip += offset;
				break;
			}
			case OP_LOOP: {
				uint16_t offset = READ_SHORT();
				vm.ip -= offset;
				break;
			}
		}
	}

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_CONSTANT_LONG
#undef READ_STRING
#undef READ_STRING_LONG
#undef READ_SHORT
#undef BINARY_OP
#undef ERROR_IF_CONST
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


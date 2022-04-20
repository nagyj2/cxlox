#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "common.h"
#include "vm.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "xstdlib.h"

/*
 ~ When adding new instructions, modify:
 ~ 	disassembleInstruction()
 ~	run()
 */

// Declare the VM in global scope. Prevents needing to pass it as an argument everywhere.
// By passing the VM as a pointer to functions, it is easier to have multiple VMs and pass them around in a host language.
VM vm;

//~ VM Initialization and Deinitialization

/** Resets the VM's stack state.
 */
void resetStack() {
	// Doesnt matter if stack elements are freed b/c they are still in v.stack's allocation AND vn.stackTop will just overwrite new values.
	vm.stackTop = vm.stack;
	vm.frameCount = 0;
	vm.openUpvalues = NULL;
}

void initVM() {
	resetStack();
	vm.objects = NULL;
	initTable(&vm.strings); // Initialize the string internment table.
	initTable(&vm.globals);
	initTable(&vm.constants);

	vm.grayCount = 0;
	vm.grayCapacity = 0;
	vm.grayStack = NULL;

	vm.bytesAllocated = 0; // Start with nothing allocated
	vm.nextGC = 1024 * 1024; // Default starting size for the first GC. ~1MB
	vm.rainyDay = malloc(RAINY_DAY_MEMORY); // Allocate a portion of memory to use in case the GC needs to allocate memory for the graystack but cannot. ~0.5MB
	if (vm.rainyDay == NULL) {
		printf("Failed to allocate backup memory.\n");
		exit(1);
	}

#ifdef DEBUG_LOG_GC
	printf("%p allocate %zu for rainy day\n", vm.rainyDay, RAINY_DAY_MEMORY);
#endif
	
	// Create all native functions
#ifdef DEBUG_LOAD_STDLIB
	loadStdlib();
#endif
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

	// Print the trace of the error
	// todo change to opposite order in xlox
	for (int i = vm.frameCount - 1; i >= 0; i--) {
		CallFrame* frame = &vm.frames[i];
		ObjFunction* function = frame->closure->function;
		size_t instruction = frame->ip - function->chunk.code - 1;
		fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
		if (function->name == NULL) {
			fprintf(stderr, "script\n");
		} else {
			fprintf(stderr, "%s()\n", function->name->chars);
		}
	}

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
	ObjString* b = AS_STRING(peek(0));
	ObjString* a = AS_STRING(peek(1));

	int length = a->length + b->length;
	char* chars = ALLOCATE(char, length + 1);
	memcpy(chars, a->chars, a->length);
	memcpy(chars + a->length, b->chars, b->length);
	chars[length] = '\0';

	ObjString* result = takeString(chars, length); // May cause GC, so we want to keep arguments on the stack
	pop(); pop(); // Leave on stack until done with them for GC reasons
	push(OBJ_VAL(result));
}

/** Constructs a new call frame for the VM. Effectively enters a new Lox function by altering the current frame
 * @details
 * Places the new call frame on top of the frame stack.
 * @param[in] closure The closure being called.
 * @param[in] argCount The number of arguments the function was called with.
 * @return true The frame was successfully created.
 * @return false The frame could not be created.
 */
static bool call(ObjClosure* closure, int argCount) {
	// Lox cannot have functions with arbitrary number of arguments.
	if (argCount != closure->function->arity) {
		runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);
		return false;
	}
	if (vm.frameCount == FRAMES_MAX) {
		runtimeError("Stack overflow.");
		return false;
	}
	// Get next frame from vm and fill in the details
	// run() uses the top of the frame stack to determine execution, so altering the top will cause execution to move to the new code
	CallFrame* frame = &vm.frames[vm.frameCount++];
	frame->closure = closure;
	frame->ip = closure->function->chunk.code;
	frame->slots = vm.stackTop - argCount - 1;
	return true;
}

/** Performs the actual call to a function.
 * @param[in] callee Who is being called.
 * @param[in] argCount The number of arguments passed to the function.
 * @return true 
 * @return false 
 */
static bool callValue(Value callee, int argCount) {
	if (IS_OBJ(callee)) {
		switch (OBJ_TYPE(callee)) {
			// case OBJ_FUNCTION: // Functions arent directly accessible b/c they are all enclosed by closures
			// 	return call(AS_FUNCTION(callee), argCount);
			case OBJ_CLOSURE:
				return call(AS_CLOSURE(callee), argCount);
			case OBJ_NATIVE: {
				ObjNative* loxcallee = AS_NATIVE(callee);
				// Ensure number of input arguments is correct. -1 indicates the function accepts any number of arguments.
				if (loxcallee->arity != -1 && argCount != loxcallee->arity) {
					runtimeError("Expected %d arguments but got %d.", loxcallee->arity, argCount);
					return false;
				}
				NativeFn native = AS_CNATIVE(callee);
				Value result = native(argCount, vm.stackTop - argCount);
				vm.stackTop -= argCount + 1; // Reduce by # of args + callee reference
				push(result);
				return true; // success
			}
			case OBJ_CLASS: {
				ObjClass* klass = AS_CLASS(callee);
				vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass)); // Create instance where caller's name was
				// Allows instance to be referenced by initializer func (b/c the instance is already on the stack)
				return true;
			}
			default:
				break; // Non callable object
		}
	}
	runtimeError("Can only call functions and classes.");
	return false;
}

/** Takes a value from the stack and creates an upvalue for it.
 * @param[in] local The variable to close over.
 * @return ObjUpvalue* pointer to the upvalue object.
 */
static ObjUpvalue* captureUpvalue(Value* local) {
	ObjUpvalue* prevUpvalue = NULL;
	ObjUpvalue* upvalue = vm.openUpvalues;
	// Iterate through the existing upvalues based on location. Stops iterating where the looked for upvalue should be.
	while (upvalue != NULL && upvalue->location > local) {
		prevUpvalue = upvalue; // Found an upvalue below the element we want. Keep looking
		upvalue = upvalue->next;
	}

	// Check to see if upvalue is where it should be
	if (upvalue != NULL && upvalue->location == local) {
		return upvalue; // Reuse existing upvalue
	}

	// If not found, create a new upvalue
	ObjUpvalue* createdUpvalue = newUpvalue(local);
	// Add it to the linked list. We already are in its sorted spot due to the check above
	createdUpvalue->next = upvalue;
	if (prevUpvalue == NULL) {
		vm.openUpvalues = createdUpvalue;
	} else {
		prevUpvalue->next = createdUpvalue;
	}
	return createdUpvalue;
}

/** Closes all upvalues equal to or before the given stack pointer.
 * @details
 * If an upvalue is pointed to a position before the range we are attempting to close, we close it.
 * @param[in] last The pointer position of the lowest upvalue to close.
 */
static void closeUpvalues(Value* last) {
	while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
		ObjUpvalue* upvalue = vm.openUpvalues;
		upvalue->closed = *upvalue->location; // Copy variable value. Provides connection to where the value lives on the heap
		upvalue->location = &upvalue->closed; // Point the location to its heap location. That way we either point to the stack locale or heap locale using 'location' attribute
		vm.openUpvalues = upvalue->next; // 
	}
}

//~ VM Execution

static InterpretResult run() {
	// Get the top-most call frame
	CallFrame* frame = &vm.frames[vm.frameCount - 1];
	register uint8_t* ip = frame->ip; // Load the instruction pointer into a register-preferred variable
	// Define returns the next byte while incrementing the counter
#define READ_BYTE() (*ip++)
	// Read a constant from the bytecode by taking the index and then looking it up in the constant pool
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
	// Reads a constant which has a 24 bit address
#define READ_CONSTANT_LONG() (frame->closure->function->chunk.constants.values[(READ_BYTE()) | (READ_BYTE() << 8) | (READ_BYTE() << 16)])
	// Read a constant from the bytecode and convert it to a string
#define READ_STRING() AS_STRING(READ_CONSTANT())
	// Read a constant from the bytecode and convert it to a string using a 24 bit address
#define READ_STRING_LONG() AS_STRING(READ_CONSTANT_LONG())
	// Read a 2 byte chunk of code
#define READ_SHORT() (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
	// Pop two elements from the stack, add them and then place the result back. Remember, left arg is placed first
	// Uses a do loop to allow multiple lines AND a culminating semicolon
	// Note: Safe to pop mathematical values from the stack b/c non-string primitives dont have heap data
#define BINARY_OP(valueType, op) \
	do { \
		if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
			frame->ip = ip; \
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
			frame->ip = ip; \
			runtimeError("Cannot redefine constant"); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
	} while (false)

	for (;;) {
#ifdef DEBUG_TRACE_STACK
		// Print the stack contents before disassembling the instruction
		printf("        ");
		for (Value *slot = vm.stack; slot < vm.stackTop; slot++) {
			printf("[ ");
			printValue(*slot);
			printf(" ]");
		}
		printf("\n");
#endif
#ifdef DEBUG_TRACE_EXECUTION
		disassembleInstruction(&frame->closure->function->chunk, (int) (ip - frame->closure->function->chunk.code)); // Perform some math to get offset
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
					frame->ip = ip;
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
				Value result = pop(); // Remove the script element from the stack
				closeUpvalues(frame->slots); // Implicitly close all open upvalues for the entire frame
				vm.frameCount--; // Remove the call frame from the stack
				if (vm.frameCount == 0) {
					// If there are no more frames, we are done
					pop();
					return INTERPRET_OK;
				}

				vm.stackTop = frame->slots; // Reset stack to the base of the frame. Effectively removes all locals
				push(result); // Push the result back onto the stack. Allows for returns
				frame = &vm.frames[vm.frameCount - 1]; // Update the frame pointer
				ip = frame->ip; // Restore ip
				break;
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
					frame->ip = ip;
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
					frame->ip = ip;
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
					frame->ip = ip;
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
					frame->ip = ip;
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
					frame->ip = ip;
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
			case OP_DUP: {
				push(peek(0));
				break;
			}
			case OP_GET_LOCAL: {
				uint8_t slot = READ_BYTE();
				push(frame->slots[slot]);
				break;
			}
			case OP_SET_LOCAL: {
				uint8_t slot = READ_BYTE();
				frame->slots[slot] = peek(0); // redefines element already in the stack
				break;
			}
			case OP_JUMP: {
				uint16_t offset = READ_SHORT();
				ip += offset;
				break;
			}
			case OP_JUMP_IF_FALSE: {
				uint16_t offset = READ_SHORT();
				if (isFalsey(peek(0)))
					ip += offset;
				break;
			}
			case OP_LOOP: {
				uint16_t offset = READ_SHORT();
				ip -= offset;
				break;
			}
			case OP_CALL: {
				int argCount = READ_BYTE(); // Number of stack elements to 'consume' as arguments
				frame->ip = ip; // Same the current ip position. Allows the call to use the register variable
				if (!callValue(peek(argCount), argCount)) { // Add new frame to the frame stack and if that fails, error
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm.frames[vm.frameCount - 1]; // Update frame pointer to the new frame on the stack
				ip = frame->ip; // Restore the old ip to the register variable
				break;
			}
			case OP_CLOSURE: {
				ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
				ObjClosure* closure = newClosure(function);
				push(OBJ_VAL(closure));
				// Walk through each captured element and place them into the closure's upvalues
				for (int i = 0; i < closure->upvalueCount; i++) {
					uint8_t isLocal = READ_BYTE();
					uint8_t index = READ_BYTE(); // Location of the captured variable (if local, stack position. Otherwise, the saved location from compilation)
					if (isLocal) {
						// Capture an element on the stack with given index
						closure->upvalues[i] = captureUpvalue(frame->slots + index); // Find the upvalue source from the frame base and read index
					} else {
						// Capture from the surrounding function (this one), so use index to grab from my array
						closure->upvalues[i] = frame->closure->upvalues[index]; // Retrieve stored position from the upvalues
					}
				}
				break;
			}
			case OP_CLOSURE_LONG: {
				ObjFunction* function = AS_FUNCTION(READ_CONSTANT_LONG());
				ObjClosure* closure = newClosure(function);
				push(OBJ_VAL(closure));
				// Walk through each captured element and place them into the closure's upvalues
				for (int i = 0; i < closure->upvalueCount; i++) {
					uint8_t isLocal = READ_BYTE();
					uint8_t index = READ_BYTE(); // Location of the captured variable (if local, stack position. Otherwise, the saved location from compilation)
					if (isLocal) {
						// Capture an element on the stack with given index
						closure->upvalues[i] = captureUpvalue(frame->slots + index); // Find the upvalue source from the frame base and read index
					} else {
						// Capture from the surrounding function (this one), so use index to grab from my array
						closure->upvalues[i] = frame->closure->upvalues[index]; // Retrieve stored position from the upvalues
					}
				}
				break;
			}
			case OP_GET_UPVALUE: {
				uint8_t slot = READ_BYTE();
				push(*frame->closure->upvalues[slot]->location);
				break;
			}
			case OP_SET_UPVALUE: {
				uint8_t slot = READ_BYTE();
				*frame->closure->upvalues[slot]->location = peek(0);
				break;
			}
			case OP_CLOSE_UPVALUE: {
				closeUpvalues(vm.stackTop - 1); // Save top stack element
				pop(); // Remove the value from the stack
				break;
			}
			case OP_CLASS: {
				push(OBJ_VAL(newClass(READ_STRING())));
				break;
			}
#ifdef USE_STACK_PROPERTY_DELETE
			case OP_DEL_PROPERTY: {
				if (!IS_INSTANCE(peek(1))) {
					runtimeError("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				ObjInstance* instance = AS_INSTANCE(peek(1));
				ObjString* name = AS_STRING(peek(0));

				tableDelete(&instance->fields, OBJ_VAL(name));
				pop(); // Pop after in case GC runs
				pop();
				break;
			}
#else
			case OP_DEL_PROPERTY: {
				if (!IS_INSTANCE(peek(0))) {
					runtimeError("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				ObjInstance* instance = AS_INSTANCE(peek(0));
				ObjString* name = READ_STRING();

				tableDelete(&instance->fields, OBJ_VAL(name));
				pop(); // Pop after in case GC runs
				break;
			}
			case OP_DEL_PROPERTY_LONG: {
				if (!IS_INSTANCE(peek(0))) {
					runtimeError("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				ObjInstance* instance = AS_INSTANCE(peek(0));
				ObjString* name = READ_STRING_LONG();
				
				tableDelete(&instance->fields, OBJ_VAL(name));
				pop(); // Pop after in case GC runs
				break;
			}
#endif
			case OP_GET_PROPERTY: {
				// Ensure a valid recipient is on the top of the stack
				// This instruction ONLY operates on the SPECIFIC instance on the top of the stack
				if (!IS_INSTANCE(peek(0))) {
					runtimeError("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				ObjInstance* instance = AS_INSTANCE(peek(0));
				ObjString* name = READ_STRING();

				Value value;
				// If property exists, replace the top of the stack with it
				if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
					pop();
					push(value);
					break;
				}

				runtimeError("Undefined property '%s'.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			case OP_GET_PROPERTY_LONG: {
				if (!IS_INSTANCE(peek(0))) {
					runtimeError("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				ObjInstance* instance = AS_INSTANCE(peek(0));
				ObjString* name = READ_STRING_LONG();

				Value value;
				// If property exists, replace the top of the stack with it
				if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
					pop();
					push(value);
					break;
				}

				runtimeError("Undefined property '%s'.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			case OP_SET_PROPERTY: {
				// Ensure a valid recipient is under the top element of the stack
				// This instruction ONLY operates on the SPECIFIC instance on the top of the stack
				if (!IS_INSTANCE(peek(1))) {
					runtimeError("Only instances have fields.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				/** Stack shape
				 * VALUE 			(value to assign)
				 * INSTANCE		(instance to modify)
				 * ....
				 */

				ObjInstance* instance = AS_INSTANCE(peek(1)); 				// Instance to set the property on
				// ObjString* name = READ_STRING();										// Name of the property to set -> Can be inlined, like in _LONG version
				
				tableSet(&instance->fields, OBJ_VAL(READ_STRING()), peek(0)); 	// Set the proeprty indexed by the opcode to the value on the top of the stack
				Value value = pop(); // property
				pop(); // instance
				push(value); // put the value assigned back on the stack
				break;
			}
			case OP_SET_PROPERTY_LONG: {
				if (!IS_INSTANCE(peek(1))) {
					runtimeError("Only instances have fields.");
					return INTERPRET_RUNTIME_ERROR;
				}

				ObjInstance* instance = AS_INSTANCE(peek(1)); 				// Instance to set the property on
				tableSet(&instance->fields, OBJ_VAL(READ_STRING_LONG()), peek(0)); 	// Set the proeprty indexed by the opcode to the value on the top of the stack
				Value value = pop(); // property
				pop(); // instance
				push(value); // put the value assigned back on the stack
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
	// Compile the source code. This is the top level function
	ObjFunction* function = compile(source);
	if (function == NULL)
		return INTERPRET_COMPILE_ERROR;

	// Simulate the main script as a function call
	push(OBJ_VAL(function));
	ObjClosure* closure = newClosure(function);
	pop();
	push(OBJ_VAL(closure));
	call(closure, 0); // Set up first frame

	// Run the VM
	return run();
}


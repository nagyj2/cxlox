#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "vm.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "optionals/xstdlib.h"
#include "util.h"

/*
 ~ When adding new instructions, modify:
 ~ 	disassembleInstruction()
 ~	run()
 */

//~ VM Initialization and Deinitialization

/** Resets the VM's stack state.
 */
void resetStack(VM* vm) {
	// Doesnt matter if stack elements are freed b/c they are still in v.stack's allocation AND vn.stackTop will just overwrite new values.
	vm->stackTop = vm->stack;
	vm->frameCount = 0;
	vm->openUpvalues = NULL;
	vm->compiler = NULL;
}

VM* initVM() {
	VM* vm = malloc(sizeof(*vm));
	if (!vm) {
		printf("Error: Could not allocate memory for VM.\n");
		exit(1);
	}

	resetStack(vm);
	vm->objects = NULL;
	initTable(&vm->strings); // Initialize the string internment table.
	initTable(&vm->globals);
	initTable(&vm->constants);
	initTable(&vm->modules);

	vm->grayCount = 0;
	vm->grayCapacity = 0;
	vm->grayStack = NULL;

	vm->isREPL = false; // Not repl by default

	vm->bytesAllocated = 0; // Start with nothing allocated
	vm->nextGC = 1024 * 1024; // Default starting size for the first GC. ~1MB
	vm->rainyDay = malloc(RAINY_DAY_MEMORY); // Allocate a portion of memory to use in case the GC needs to allocate memory for the graystack but cannot. ~0.5MB
	if (vm->rainyDay == NULL) {
		printf("Failed to allocate backup memory.\n");
		exit(1);
	}
	vm->usedRainyDay = false;

#ifdef DEBUG_LOG_GC
	printf("%p allocate %d for rainy day\n", vm->rainyDay, RAINY_DAY_MEMORY);
#endif
	
	vm->initString = NULL; // copyString() can allocate, thus causing GC which will read uninitializer vm->initString. For safety, NULL it
	vm->initString = copyString(vm, "init", 4);

	// Create all native functions
#ifdef DEBUG_LOAD_STDLIB
	// createStdlibModule();
#endif
	return vm;
}

void freeVM(VM* vm) {
#ifdef DEBUG_LOG_GC
	printf("== Freeing VM ==\n");
#endif
	
	freeTable(vm, &vm->modules);
	freeTable(vm, &vm->globals);
	freeTable(vm, &vm->constants);
	freeTable(vm, &vm->strings);

	vm->initString = NULL;
	freeObjects(vm);
#ifdef DEBUG_LOG_GC
	printf("%p freed rainy day\n", vm->rainyDay);
#endif
	free(vm->rainyDay);
	vm->rainyDay = NULL;

	free(vm);
}

//~ Error Reporting

void runtimeError(VM* vm, const char* format, ...) {
	// Print the message to stderr
	va_list args;
	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	fputs("\n", stderr);

	// Print the trace of the error
	// todo change to opposite order in xlox
	for (int i = vm->frameCount - 1; i >= 0; i--) {
		CallFrame* frame = &vm->frames[i];
		ObjFunction* function = frame->closure->function;
		size_t instruction = frame->ip - function->chunk.code - 1;
		fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction].line);
		if (function->name == NULL) {
			fprintf(stderr, "script\n");
		} else {
			fprintf(stderr, "%s()\n", function->name->chars);
		}
	}

	// Reset the stack
	resetStack(vm);
}

//~ Stack Management

void push(VM* vm, Value value) {
	*vm->stackTop = value; // Dereference top and place a value there. Remember, stackTop points PAST the last element
	vm->stackTop++; 				// Increment the size of the stack 
}

Value pop(VM* vm) {
	vm->stackTop--;
	return *vm->stackTop;
}

Value popn(VM* vm, int n) {
	vm->stackTop -=n ;
	return *vm->stackTop;
}

/** Returns the value at a given index from the top of the stack.
 * @pre there is at least one element in the stack.
 *
 * @param[in] distance How far down the stack to look.
 * @return Value at the given position from the top in the stack.
 */
static Value peek(VM* vm, int distance) {
	return vm->stackTop[-1 - distance];
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

static void concatenate(VM* vm) {
	ObjString* b = AS_STRING(peek(vm, 0));
	ObjString* a = AS_STRING(peek(vm, 1));

	int length = a->length + b->length;
	char* chars = ALLOCATE(vm, char, length + 1);
	memcpy(chars, a->chars, a->length);
	memcpy(chars + a->length, b->chars, b->length);
	chars[length] = '\0';

	ObjString* result = takeString(vm, chars, length); // May cause GC, so we want to keep arguments on the stack
	pop(vm); pop(vm); // Leave on stack until done with them for GC reasons
	push(vm, OBJ_VAL(result));
}

/** Add a new frame to the top of the frame stack and fill in its details
 * @param[in] closure The new function to start executing
 * @param[in] argCount Backwards stack offset to place the frame's stack at
 */
static void createFrame(VM* vm, ObjClosure* closure, int argCount) {
	// Get next frame from vm and fill in the details
	// run() uses the top of the frame stack to determine execution, so altering the top will cause execution to move to the new code
	CallFrame* frame = &vm->frames[vm->frameCount++];
	frame->closure = closure;
	frame->ip = closure->function->chunk.code;
	frame->slots = vm->stackTop - argCount - 1;
}

/** Constructs a new call frame for the VM. Effectively enters a new Lox function by altering the current frame
 * @details
 * Places the new call frame on top of the frame stack.
 * @param[in] closure The closure being called.
 * @param[in] argCount The number of arguments the function was called with.
 * @return true The frame was successfully created.
 * @return false The frame could not be created.
 */
static bool call(VM* vm, ObjClosure* closure, int argCount) {
	// Lox cannot have functions with arbitrary number of arguments.
	if (argCount != closure->function->arity) {
		runtimeError(vm, "Expected %d arguments but got %d.", closure->function->arity, argCount);
		return false;
	}
	if (vm->frameCount == FRAMES_MAX) {
		runtimeError(vm, "Stack overflow.");
		return false;
	}
	createFrame(vm, closure, argCount);
	return true;
}

/** Provides the setup logic for calling any value. May pass call to secondary functions if needed.
 * @param[in] callee The value being called.
 * @param[in] argCount The number of arguments on top of the stack (how far back to set frame - frame includes them as locals).
 * @return true if the call was successful.
 * @return false if the call was unsuccessful.
 */
static bool callValue(VM* vm, Value callee, int argCount) {
	if (IS_OBJ(callee)) {
		switch (OBJ_TYPE(callee)) {
			// case OBJ_FUNCTION: // Functions arent directly accessible b/c they are all enclosed by closures
			// 	return call(vm, AS_FUNCTION(callee), argCount);
			case OBJ_CLOSURE:
				return call(vm, AS_CLOSURE(callee), argCount);
			case OBJ_NATIVE: {
				ObjNative* loxcallee = AS_NATIVE(callee);
				// Ensure number of input arguments is correct. -1 indicates the function accepts any number of arguments.
				if (loxcallee->arity != -1 && argCount != loxcallee->arity) {
					runtimeError(vm, "Expected %d arguments but got %d.", loxcallee->arity, argCount);
					return false;
				}
				NativeFn native = AS_CNATIVE(callee);
				Value result = native(argCount, vm->stackTop - argCount);
				vm->stackTop -= argCount + 1; // Reduce by # of args + callee reference
				push(vm, result);
				return true; // success
			}
			case OBJ_CLASS: {
				ObjClass* klass = AS_CLASS(callee);
				vm->stackTop[-argCount - 1] = OBJ_VAL(newInstance(vm, klass)); // Create instance where caller's name was
				// ^ Allows instance to be referenced by initializer func (b/c the instance is already on the stack)

				Value initializer;
				if (tableGet(&klass->methods, OBJ_VAL(vm->initString), &initializer)) {
					return call(vm, AS_CLOSURE(initializer), argCount); // call(vm, ) handles arity checking
				} else if (argCount != 0) { // No initializer, so implicitly require 0 args
					runtimeError(vm, "Expected 0 arguments but got %d.", argCount);
					return false;
				}

				return true;
			}
			case OBJ_BOUND_METHOD: {
				ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
				// initCompiler sets aside slot 1 on methods for the instance and binds it to 'this', 
				// so we need to place a reference to the instance there
					vm->stackTop[-argCount - 1] = bound->receiver;
				return call(vm, bound->method, argCount);
			}
			default:
				break; // Non callable object
		}
	}
	runtimeError(vm, "Can only call functions and classes.");
	return false;
}

/** Takes a value from the stack and creates an upvalue for it.
 * @param[in] local The variable to close over.
 * @return ObjUpvalue* pointer to the upvalue object.
 */
static ObjUpvalue* captureUpvalue(VM* vm, Value* local) {
	ObjUpvalue* prevUpvalue = NULL;
	ObjUpvalue* upvalue = vm->openUpvalues;
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
	ObjUpvalue* createdUpvalue = newUpvalue(vm, local);
	// Add it to the linked list. We already are in its sorted spot due to the check above
	createdUpvalue->next = upvalue;
	if (prevUpvalue == NULL) {
		vm->openUpvalues = createdUpvalue;
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
static void closeUpvalues(VM* vm, Value* last) {
	while (vm->openUpvalues != NULL && vm->openUpvalues->location >= last) {
		ObjUpvalue* upvalue = vm->openUpvalues;
		upvalue->closed = *upvalue->location; // Copy variable value. Provides connection to where the value lives on the heap
		upvalue->location = &upvalue->closed; // Point the location to its heap location. That way we either point to the stack locale or heap locale using 'location' attribute
		vm->openUpvalues = upvalue->next;
	}
}

/** Binds a method to a class' methods table.
 * @param[in] name The property name to bind the method under.
 */
static void defineMethod(VM* vm, ObjString* name) {
	Value method = peek(vm, 0);
	ObjClass* klass = AS_CLASS(peek(vm, 1));
	tableSet(vm, &klass->methods, OBJ_VAL(name), method);
	pop(vm); // Remove method from stack
}

/** Places a bounded method on the top of the stack.
 * @details
 * Expects an instance to be on the top of the stack and uses that instance's class and the input property name
 * to find the method. Once found, the instance and method (closure) are combined into a bound method object
 * which then replaces the instance on top of the stack.
 * @param[in] klass The class to look for the method in
 * @param[in] name The method name to find in the class
 * @return true If the method was found
 * @return false If the method was not found
 */
static bool bindMethod(VM* vm, ObjClass* klass, ObjString* name) {
	Value method;
	if (!tableGet(&klass->methods, OBJ_VAL(name), &method)) { // If the method cannot be found, return errpr
		runtimeError(vm, "Undefined property '%s'.", name->chars);
		return false;
	}

	ObjBoundMethod* bound = newBoundMethod(vm, peek(vm, 0), AS_CLOSURE(method));
	pop(vm); // instance
	push(vm, OBJ_VAL(bound));
	return true;
}

/** Invokes a method directly from a class.
 * @param[in] klass The class to look for the method in.
 * @param[in] name The method to call.
 * @param[in] argCount The number of arguments being passed to the method.
 * @return true If the call is successful.
 * @return false If the call is unsuccessful.
 */
static bool invokeFromClass(VM* vm, ObjClass* klass, ObjString* name, int argCount) {
	Value method;
	if (!tableGet(&klass->methods, OBJ_VAL(name), &method)) {
		runtimeError(vm, "Undefined property '%s'.", name->chars);
		return false;
	}

	return call(vm, AS_CLOSURE(method), argCount);
}

/** Looks back on the stack to determine if a instance can be called upon.
 * If so, it gets called.
 * @param[in] name The method to call from the instance.
 * @param[in] argCount The number of arguments being passed to the method.
 * @return true if the call succeeded.
 * @return false if the call failed.
 */
static bool invoke(VM* vm,  ObjString* name, int argCount) {
	Value receiver = peek(vm, argCount); // instance we are calling on
	if (!IS_OBJ(receiver)) {
		
	} else {
		switch (OBJ_TYPE(receiver)) {
			case OBJ_INSTANCE: {
				ObjInstance* instance = AS_INSTANCE(receiver);

				// Check if there is a callable field first
				Value value;
				if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
					vm->stackTop[-argCount - 1] = value; // Replace same spot as OP_GET_PROPERTY
					return callValue(vm, value, argCount);
				}
							
				return invokeFromClass(vm, instance->klass, name, argCount);
			}
			case OBJ_MODULE: {
				ObjModule *module = AS_MODULE(receiver);

				Value value;
				if (!tableGet(&module->values, OBJ_VAL(name), &value)) {
						runtimeError(vm, "Undefined property '%s'.", name->chars);
						return false;
				}
				return callValue(vm, value, argCount);
			}
			default:
				break;
		}
	}
	
	runtimeError(vm, "Only instances have methods.");
		return false;
}

static inline bool ensureValidArrayAccess(VM *vm, Value list, Value index) {
	if (!IS_LIST(list)) {
		runtimeError(vm, "Only lists can be indexed.");
		return false;
	}
	if (!IS_INTEGER(index)) {
		runtimeError(vm, "List index must be an integer.");
		return false;
	}
	if (AS_NUMBER(index) >= AS_LIST(list)->entries.count || AS_NUMBER(index) < 0) {
		runtimeError(vm, "List index out of bounds.");
		return false;
	}
	return true;
}

//~ VM Execution

static InterpretResult run(VM* vm) {
	// Get the top-most call frame
	CallFrame* frame = &vm->frames[vm->frameCount - 1];
	register uint8_t* ip = frame->ip; // Load the instruction pointer into a register-preferred variable
	// Define returns the next byte while incrementing the counter
#define READ_BYTE() 	(*ip++)
#define READ_SHORT() 	(ip += 2, (uint16_t) ((ip[-2] << 8) | ip[-1]))
#define READ_LONG() 	(ip += 3, (uint32_t) ((ip[-3] << 16) | (ip[-2] << 8) | ip[-1]))
	// Read a constant from the bytecode by taking the index and then looking it up in the constant pool
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
	// Reads a constant which has a 24 bit address
#define READ_CONSTANT_LONG() (frame->closure->function->chunk.constants.values[READ_LONG()])
	// Read a constant from the bytecode and convert it to a string
#define READ_STRING() AS_STRING(READ_CONSTANT())
	// Read a constant from the bytecode and convert it to a string using a 24 bit address
#define READ_STRING_LONG() AS_STRING(READ_CONSTANT_LONG())
	// Pop two elements from the stack, add them and then place the result back. Remember, left arg is placed first
	// Uses a do loop to allow multiple lines AND a culminating semicolon
	// Note: Safe to pop mathematical values from the stack b/c non-string primitives dont have heap data
#define BINARY_OP(vm, valueType, op) \
	do { \
		if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) { \
			frame->ip = ip; \
			runtimeError(vm, "Operands must be numbers."); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
		double b = AS_NUMBER(pop(vm)); \
		double a = AS_NUMBER(pop(vm)); \
		push(vm, valueType(a op b)); \
	} while (false)
	// Raise a runtime error if the seeked variable is a constant
#define ERROR_IF_CONST(name) \
	do { \
		if (tableGet(&vm->constants, name, &name)) { \
			frame->ip = ip; \
			runtimeError(vm, "Cannot redefine constant"); \
			return INTERPRET_RUNTIME_ERROR; \
		} \
	} while (false)
	// Store the frame for proper error reporting
#define RUNTIME_ERROR(...) \
	do { \
		frame->ip = ip; \
		runtimeError(vm, __VA_ARGS__); \
		return INTERPRET_RUNTIME_ERROR; \
	} while (false)

	for (;;) {
#ifdef DEBUG_TRACE_STACK
		// Print the stack contents before disassembling the instruction
		printf("        ");
		for (Value *slot = vm->stack; slot < vm->stackTop; slot++) {
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
				if (IS_STRING(peek(vm, 1)) || IS_STRING(peek(vm, 0))) {
					ObjString* b = toObjString(vm, pop(vm)); // Convert to strings. Not neccesary for strings
					ObjString* a = toObjString(vm, pop(vm));
					push(vm, OBJ_VAL(a)); // Push as string values to the stack in inverse order to popping
					push(vm, OBJ_VAL(b));
					concatenate(vm);
				// } else if (IS_STRING(peek(vm, 0)) && IS_STRING(peek(vm, 1))) {
				// 	concatenate();
				} else if (IS_NUMBER(peek(vm, 0)) && IS_NUMBER(peek(vm, 1))) {
					double b = AS_NUMBER(pop(vm));
					double a = AS_NUMBER(pop(vm));
					push(vm, NUMBER_VAL(a + b));
				} else {
					RUNTIME_ERROR("Operands must be two numbers or two strings.");
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			case OP_SUBTRACT: {
				BINARY_OP(vm, NUMBER_VAL, -);
				break;
			}
			case OP_MULTIPLY: {
				BINARY_OP(vm, NUMBER_VAL, *);
				break;
			}
			case OP_DIVIDE: {
				BINARY_OP(vm, NUMBER_VAL, / );
				break;
			}
			case OP_GREATER: {
				BINARY_OP(vm, BOOL_VAL, > );
				break;
			}
			case OP_LESSER: {
				BINARY_OP(vm, BOOL_VAL, < );
				break;
			}
			case OP_RETURN: {
				// Exit interpreter
				Value result = pop(vm); // Remove the script element from the stack
				closeUpvalues(vm, frame->slots); // Implicitly close all open upvalues for the entire frame
				vm->frameCount--; // Remove the call frame from the stack
				if (vm->frameCount == 0) {
					// If there are no more frames, we are done
					pop(vm);
					return INTERPRET_OK;
				}

				vm->stackTop = frame->slots; // Reset stack to the base of the frame. Effectively removes all locals
				push(vm, result); // Push the result back onto the stack. Allows for returns
				//~ Restores frame (returns execution back to the caller)
				frame = &vm->frames[vm->frameCount - 1]; // Update the frame pointer
				ip = frame->ip; // Restore ip
				break;
			}
			case OP_CONSTANT:
			case OP_CONSTANT_LONG: {
				Value constant = (instruction == OP_CONSTANT) ? READ_CONSTANT() : READ_CONSTANT_LONG();
				push(vm, constant);
				break;
			}
			case OP_NEGATE: {
				// Ensure stack is a number.
				if (!IS_NUMBER(peek(vm, 0))) {
					RUNTIME_ERROR("Operand must be a number.");
					return INTERPRET_RUNTIME_ERROR;
				}
				push(vm, NUMBER_VAL(-AS_NUMBER(pop(vm))));
				break;
			}
			case OP_TRUE: {
				push(vm, BOOL_VAL(true));
				break;
			}
			case OP_FALSE: {
				push(vm, BOOL_VAL(false));
				break;
			}
			case OP_NIL: {
				push(vm, NIL_VAL);
				break;
			}
			case OP_NOT: {
				push(vm, BOOL_VAL(isFalsey(pop(vm))));
				break;
			}
			case OP_EQUAL: {
				Value b = pop(vm);
				Value a = pop(vm);
				push(vm, BOOL_VAL(valuesEqual(a, b)));
				break;
			}
			case OP_PRINT: {
				printValue(pop(vm));
				printf("\n");
				break;
			}
			case OP_POP: {
				pop(vm);
				break;
			}
			case OP_DEFINE_GLOBAL:
			case OP_DEFINE_GLOBAL_LONG: {
				Value name = (instruction == OP_DEFINE_GLOBAL) ? READ_CONSTANT() : READ_CONSTANT_LONG();
				Value value = peek(vm, 0);
				ERROR_IF_CONST(name);
				tableSet(vm, &vm->globals, name, value); // Place the value into the hash table BEFORE popping it so it doesnt get picked up by garbage collection
				pop(vm);
				break;
			}
			case OP_DEFINE_CONST:
			case OP_DEFINE_CONST_LONG: {
				Value name = (instruction == OP_DEFINE_CONST) ? READ_CONSTANT() : READ_CONSTANT_LONG();
				ERROR_IF_CONST(name);
				tableSet(vm, &vm->constants, name, peek(vm, 0)); // Place the value into the hash table BEFORE popping it so it doesnt get picked up by garbage collection
				pop(vm);
				break;
			}
			case OP_GET_GLOBAL: 
			case OP_GET_GLOBAL_LONG: {
				Value name = ((instruction == OP_GET_GLOBAL) ? READ_CONSTANT() : READ_CONSTANT_LONG()); // Will always be a string
				Value value; // Output value
				if (!(tableGet(&vm->globals, name, &value) || tableGet(&vm->constants, name, &value))) { // Unlike string internment, globals is simply indexed by strings.
					RUNTIME_ERROR("Undefined variable '%s'.", AS_STRING(name)->chars);
					return INTERPRET_RUNTIME_ERROR;
				}
				push(vm, value);
				break;
			}
			case OP_SET_GLOBAL:
			case OP_SET_GLOBAL_LONG: {
				Value name = (instruction == OP_SET_GLOBAL) ? READ_CONSTANT() : READ_CONSTANT_LONG();
				ERROR_IF_CONST(name);
				if (tableSet(vm, &vm->globals, name, peek(vm, 0))) { // If this was a new entry, the variable didnt exist
					tableDelete(vm, &vm->globals, name); // Roll back change (important for REPL)
					RUNTIME_ERROR("Undefined variable '%s'.", AS_STRING(name)->chars);
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			case OP_CONDITIONAL: { // a ? b evaluates to b if a is truthy, otherwise it evaluates to nil
				Value b = pop(vm); // False branch value
				Value a = pop(vm); // True branch value
				push(vm, isFalsey(a) ? NIL_VAL : b);
				break;
			}							
			case OP_OPTIONAL: { // a : b evaluates to a if a is not nil, otherwise it evaluates to b
				Value b = pop(vm); // False branch value
				Value a = pop(vm); // True branch value
				push(vm, valuesEqual(a, NIL_VAL) ? b : a);
				break;
			}
			case OP_POPN: {
				int n = (int) READ_BYTE();
				while (n > 0) {
					pop(vm);
					n--;
				}
				break;
			}
			case OP_POPREPL: {
				if (vm->isREPL) {
					printValue(pop(vm));
					printf("\n");
				} else {
					pop(vm);
				}
				break;
			}
			case OP_DUP: {
				push(vm, peek(vm, 0));
				break;
			}
			case OP_GET_LOCAL: {
				uint8_t slot = READ_BYTE();
				push(vm, frame->slots[slot]);
				break;
			}
			case OP_SET_LOCAL: {
				uint8_t slot = READ_BYTE();
				frame->slots[slot] = peek(vm, 0); // redefines element already in the stack
				break;
			}
			case OP_JUMP: {
				uint16_t offset = READ_SHORT();
				ip += offset;
				break;
			}
			case OP_JUMP_IF_FALSE: {
				uint16_t offset = READ_SHORT();
				if (isFalsey(peek(vm, 0)))
					ip += offset;
				break;
			}
			case OP_JUMP_IF_NIL: {
				uint16_t offset = READ_SHORT();
				if (IS_NIL(peek(vm, 0)))
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
				if (!callValue(vm, peek(vm, argCount), argCount)) { // Add new frame to the frame stack and if that fails, error
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm->frames[vm->frameCount - 1]; // Update frame pointer to the new frame on the stack
				ip = frame->ip; // Restore the old ip to the register variable
				break;
			}
			case OP_CLOSURE:
			case OP_CLOSURE_LONG: {
				Value constant = (instruction == OP_CLOSURE) ? READ_CONSTANT() : READ_CONSTANT_LONG();
				ObjFunction* function = AS_FUNCTION(constant);
				ObjClosure* closure = newClosure(vm, function);
				push(vm, OBJ_VAL(closure));
				// Walk through each captured element and place them into the closure's upvalues
				for (int i = 0; i < closure->upvalueCount; i++) {
					uint8_t isLocal = READ_BYTE();
					uint8_t index = READ_BYTE(); // Location of the captured variable (if local, stack position. Otherwise, the saved location from compilation)
					if (isLocal) {
						// Capture an element on the stack with given index
						closure->upvalues[i] = captureUpvalue(vm, frame->slots + index); // Find the upvalue source from the frame base and read index
					} else {
						// Capture from the surrounding function (this one), so use index to grab from my array
						closure->upvalues[i] = frame->closure->upvalues[index]; // Retrieve stored position from the upvalues
					}
				}
				break;
			}
			case OP_GET_UPVALUE: {
				uint8_t slot = READ_BYTE();
				push(vm, *frame->closure->upvalues[slot]->location);
				break;
			}
			case OP_SET_UPVALUE: {
				uint8_t slot = READ_BYTE();
				*frame->closure->upvalues[slot]->location = peek(vm, 0);
				break;
			}
			case OP_CLOSE_UPVALUE: {
				closeUpvalues(vm, vm->stackTop - 1); // Save top stack element
				pop(vm); // Remove the value from the stack
				break;
			}
			case OP_CLASS:
			case OP_CLASS_LONG: {
				push(vm, OBJ_VAL(newClass(vm, (instruction == OP_CLASS) ? READ_STRING() : READ_STRING_LONG())));
				break;
			}
#ifdef USE_STACK_PROPERTY_DELETE
			case OP_DEL_PROPERTY: {
				if (!IS_INSTANCE(peek(vm, 1))) {
					RUNTIME_ERROR("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				ObjInstance* instance = AS_INSTANCE(peek(vm, 1));
				ObjString* name = AS_STRING(peek(vm, 0));

				tableDelete(vm, &instance->fields, OBJ_VAL(name));
				pop(vm); // Pop after in case GC runs
				pop(vm);
				break;
			}
#else
			case OP_DEL_PROPERTY: {
				if (!IS_INSTANCE(peek(vm, 0))) {
					RUNTIME_ERROR("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				ObjInstance* instance = AS_INSTANCE(peek(vm, 0));
				ObjString* name = READ_STRING();

				tableDelete(&instance->fields, OBJ_VAL(name));
				pop(vm); // Pop after in case GC runs
				break;
			}
			case OP_DEL_PROPERTY_LONG: {
				if (!IS_INSTANCE(peek(vm, 0))) {
					RUNTIME_ERROR("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				ObjInstance* instance = AS_INSTANCE(peek(vm, 0));
				ObjString* name = READ_STRING_LONG();
				
				tableDelete(&instance->fields, OBJ_VAL(name));
				pop(vm); // Pop after in case GC runs
				break;
			}
#endif
			case OP_GET_PROPERTY:
			case OP_GET_PROPERTY_LONG: {
				// Ensure a valid recipient is on the top of the stack
				// This instruction ONLY operates on the SPECIFIC instance on the top of the stack
				if (!IS_INSTANCE(peek(vm, 0))) {
					RUNTIME_ERROR("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				ObjInstance* instance = AS_INSTANCE(peek(vm, 0));
				ObjString* name = (instruction == OP_GET_PROPERTY) ? READ_STRING() : READ_STRING_LONG();

				Value value;
				// If property exists, replace the top of the stack with it. Higher precidence than a method call
				if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
					pop(vm); // Instance
					push(vm, value);
					break;
				}

				if (!bindMethod(vm, instance->klass, name)) {
					RUNTIME_ERROR("Undefined property '%s'.", name->chars);
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			// case OP_GET_PROP_SAFE: 
			// case OP_GET_PROP_SAFE_LONG: {
			// 	// b/c safe, we want '?.' to be chainable. If a non-instance is on top of the stack, we replace it with nil;
			// 	if (!IS_INSTANCE(peek(vm, 0))) {
			// 		(instruction == OP_GET_PROP_SAFE) ? READ_STRING() : READ_STRING_LONG(); // Skip the string. We dont use it, but it needs to be passed over by the VM
			// 		if (!IS_NIL(peek(vm, 0))) {  // Avoiding this op does give a performance boost
			// 			pop(vm); // Remove top and replace with nil
			// 			push(vm, NIL_VAL);
			// 		}
			// 		break;
			// 	}
			// 	ObjInstance* instance = AS_INSTANCE(peek(vm, 0));
			// 	ObjString* name = (instruction == OP_GET_PROP_SAFE) ? READ_STRING() : READ_STRING_LONG();
			// 	Value value;
			// 	if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
			// 		pop(vm); // Keep until tableGet is done to prevent GC from snatching it
			// 		push(vm, value);
			// 		break;
			// 	}
			// 	pop(vm);
			// 	push(vm, NIL_VAL);
			// 	break;
			// }
			case OP_SET_PROPERTY:
			case OP_SET_PROPERTY_LONG: {
				// Ensure a valid recipient is under the top element of the stack
				// This instruction ONLY operates on the SPECIFIC instance on the top of the stack
				if (!IS_INSTANCE(peek(vm, 1))) {
					RUNTIME_ERROR("Only instances have fields.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				/** Stack shape
				 * VALUE 			(value to assign)
				 * INSTANCE		(instance to modify)
				 * ....
				 */

				ObjInstance* instance = AS_INSTANCE(peek(vm, 1)); // Instance to set the property on
				ObjString* name = (instruction == OP_SET_PROPERTY) ? READ_STRING() : READ_STRING_LONG();
				tableSet(vm, &instance->fields, OBJ_VAL(name), peek(vm, 0)); 	// Set the proeprty indexed by the opcode to the value on the top of the stack
				Value value = pop(vm); // property
				pop(vm); // instance
				push(vm, value); // put the value assigned back on the stack
				break;
			}
			case OP_METHOD: {
				defineMethod(vm, READ_STRING());
				break;
			}
			case OP_METHOD_LONG: {
				defineMethod(vm, READ_STRING_LONG());
				break;
			}
			case OP_INVOKE:
			case OP_INVOKE_LONG: {
				ObjString* method = (instruction == OP_INVOKE) ? READ_STRING() : READ_STRING_LONG();
				int argCount = READ_BYTE();
				frame->ip = ip;
				if (!invoke(vm, method, argCount)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				break;
			}
			case OP_INHERIT: {
				Value superclass = peek(vm, 1);
				ObjClass* subclass = AS_CLASS(peek(vm, 0));
				if (!IS_CLASS(superclass)) {
					RUNTIME_ERROR("Superclass must be a class.");
					return INTERPRET_RUNTIME_ERROR;
				}
				tableAddAll(vm, &AS_CLASS(superclass)->methods, &subclass->methods);
				pop(vm); // subclass
				break;
			}
			case OP_GET_SUPER:
			case OP_GET_SUPER_LONG: {
				ObjString* name = (instruction == OP_GET_SUPER) ? READ_STRING() : READ_STRING_LONG(); // Name of the method to call
				ObjClass* superclass = AS_CLASS(pop(vm)); // Class to call method on -> use superclass instead of instance->klass

				// Instance is on the top of the stack b/c thats the standard call behaviour
				if (!bindMethod(vm, superclass, name)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			case OP_SUPER_INVOKE: 
			case OP_SUPER_INVOKE_LONG: {
				ObjString* method = (instruction == OP_SUPER_INVOKE) ? READ_STRING() : READ_STRING_LONG();
				int argCount = READ_BYTE();
				ObjClass* superclass = AS_CLASS(pop(vm));
				frame->ip = ip;
				if (!invokeFromClass(vm, superclass, method, argCount)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm->frames[vm->frameCount - 1]; // end call frame
				ip = frame->ip;
				break;
			}
			case OP_IMPORT: {
				ObjString* fileName = AS_STRING(pop(vm)); // We check for string type in compiler
				Value moduleVal;

				// If already imported, skip
				if (tableGet(&vm->modules, OBJ_VAL(fileName), &moduleVal)) {
					// vm->lastModule = AS_MODULE(moduleVal); // ?
					push(vm, NIL_VAL);
					break;
				}

				//fix assume found
				// Find the file
				char* path = fileName->chars;
				// if (!resolvePath(frame->closure->function->module->path->chars, fileName->chars, path)) {
				// 	printf("%s\n", frame->closure->function->module->path->chars);
				// 	printf("%s\n", fileName->chars);
				// 	printf("%s\n", path);
				// 	RUNTIME_ERROR("Could not find module file.");
				// }

				char* source = readFile(vm, fileName->chars);

				if (source == NULL) {
					RUNTIME_ERROR("Could not open file '%s'.", fileName->chars);
				}

				ObjString* pathObj = copyString(vm, path, strlen(path));
				push(vm, OBJ_VAL(pathObj));
				ObjModule* module = newModule(vm, pathObj);
				module->path = dirname(vm, path, strlen(path));
				// vm->lastModule = module; // ?
				pop(vm);

				push(vm, OBJ_VAL(module));
				ObjFunction *function = compile(vm, module, source);
				pop(vm);

				// Strings are coupled to source
				// FREE_ARRAY(vm, char, source, strlen(source) + 1);

				if (function == NULL) return INTERPRET_COMPILE_ERROR;
				push(vm, OBJ_VAL(function));
				ObjClosure *closure = newClosure(vm, function);
				pop(vm);
				push(vm, OBJ_VAL(closure));

				frame->ip = ip;
				call(vm, closure, 0);
				frame = &vm->frames[vm->frameCount - 1];
				ip = frame->ip;
				break;
			}
			case OP_INCLUDE: {
				push(vm, OBJ_VAL(READ_STRING()));
				pop(vm);
				break;
				// Ensure value is a string
				// Value name = pop(vm);
				// frame->ip = ip; // Update frame ip for possible errors
				// if (!IS_STRING(name)) {
				// 	RUNTIME_ERROR("Expected library name.");
				// 	return INTERPRET_RUNTIME_ERROR;
				// }

				// // Ensure ".lox" or ".xlox" prefix
				// const char* xloxsuffix = &AS_CSTRING(name)[AS_STRING(name)->length - 5];
				// if (!((AS_STRING(name)->length >= 4 && strcmp(xloxsuffix + 1, ".lox" ) == 0) ||
				// 		 ( AS_STRING(name)->length >= 5 && strcmp(xloxsuffix,     ".xlox") == 0))) {
				// 	RUNTIME_ERROR("Library name must have 'lox' or 'xlox' file extension.");
				// 	return INTERPRET_RUNTIME_ERROR;
				// }

				// char* source = readFile(AS_STRING(name)->chars);

				// ObjFunction* function = compile(vm, ??, source);
				// if (function == NULL) {
				// 	printValue(name);
				// 	RUNTIME_ERROR(" failed to compile.");
				// 	return INTERPRET_COMPILE_ERROR;
				// }
			
				// // Simulate the main script as a function call
				// push(vm, OBJ_VAL(function));
				// ObjClosure* closure = newClosure(vm, function);
				// pop(vm);

				// frame = &vm->frames[vm->frameCount++];
				// frame->ip = closure->function->chunk.code;
				// frame->closure = closure;
				// frame->slots = vm->stackTop;
				// ip = frame->ip;
				// break;

				// Actually run the code
				// createFrame(closure, 1);
			}
			case OP_NIL_LIST: {
				Value size = peek(vm, 0); // Keep on stack b/c newList may run GC
				frame->ip = ip;
				if (!IS_INTEGER(size)) {
					RUNTIME_ERROR("Expected integer for list size.");
					return INTERPRET_RUNTIME_ERROR;
				}
				ObjList* list = newList(vm, NULL, 0); // Create empty list
				for (int i = 0; i < AS_INTEGER(size); i++) {
					writeValueArray(vm, &list->entries, NIL_VAL);
				}
				pop(vm); // size
				push(vm, OBJ_VAL(list));
				break;
			}
			case OP_CREATE_LIST: {
				Value count = pop(vm); // READ_BYTE();
				frame->ip = ip;
				if (!IS_INTEGER(count)) {
					RUNTIME_ERROR("Expected integer for list size.");
					return INTERPRET_RUNTIME_ERROR;
				}
				Value* values = vm->stackTop - AS_INTEGER(count);
				ObjList* list = newList(vm, values, AS_INTEGER(count));
				popn(vm, AS_INTEGER(count));
				push(vm, OBJ_VAL(list));
				break;
			}
			case OP_GET_LIST: {
				Value index = pop(vm);
				Value list = pop(vm);
				frame->ip = ip;
				if (!ensureValidArrayAccess(vm, list, index)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				push(vm, AS_LIST(list)->entries.values[(int) AS_NUMBER(index)]);
				break;
			}
			case OP_SET_LIST: {
				Value expr = pop(vm);
				Value index = pop(vm);
				Value list = pop(vm);
				frame->ip = ip;
				ERROR_IF_CONST(list);
				if (!ensureValidArrayAccess(vm, list, index)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				AS_LIST(list)->entries.values[(int) AS_NUMBER(index)] = expr;
				push(vm, expr);
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

InterpretResult interpret(VM* vm, const char* moduleName, const char* source) {
	ObjString* name = copyString(vm, moduleName, strlen(moduleName));

	// Create a module entity
	push(vm, OBJ_VAL(name));
	ObjModule* module = newModule(vm, name);
	pop(vm);

	// Assign the module's path
	push(vm, OBJ_VAL(module));
	module->path = getDirectory(vm, moduleName);
	pop(vm);

	ObjFunction* function = compile(vm, module, source);
	if (function == NULL)
		return INTERPRET_COMPILE_ERROR;

	// Simulate the main script as a function call
	push(vm, OBJ_VAL(function));
	ObjClosure* closure = newClosure(vm, function);
	pop(vm);
	push(vm, OBJ_VAL(closure));
	callValue(vm, OBJ_VAL(closure), 0); // Set up first frame

	// Run the VM
	return run(vm);
}


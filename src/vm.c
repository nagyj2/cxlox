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
	
	vm.initString = NULL; // copyString() can allocate, thus causing GC which will read uninitializer vm.initString. For safety, NULL it
	vm.initString = copyString("init", 4);
	
	// Create all native functions
#ifdef DEBUG_LOAD_STDLIB
	loadStdlib();
#endif
}

void freeVM() {
	vm.initString = NULL;
	freeTable(&vm.strings);
	freeTable(&vm.globals);
	freeTable(&vm.constants);
	freeObjects();
}

//~ Error Reporting

/** Prints an error message to stderr. Also resets the stack.
 * To prevent nonsense line methods, `frame->ip = ip;` must preceed this function call in run()
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

Value popn(int n) {
	vm.stackTop -=n ;
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

/** Add a new frame to the top of the frame stack and fill in its details
 * @param[in] closure The new function to start executing
 * @param[in] argCount Backwards stack offset to place the frame's stack at
 */
static void createFrame(ObjClosure* closure, int argCount) {
	// Get next frame from vm and fill in the details
	// run() uses the top of the frame stack to determine execution, so altering the top will cause execution to move to the new code
	CallFrame* frame = &vm.frames[vm.frameCount++];
	frame->closure = closure;
	frame->ip = closure->function->chunk.code;
	frame->slots = vm.stackTop - argCount - 1;
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
	createFrame(closure, argCount);
	return true;
}

/** Provides the setup logic for calling any value. May pass call to secondary functions if needed.
 * @param[in] callee The value being called.
 * @param[in] argCount The number of arguments on top of the stack (how far back to set frame - frame includes them as locals).
 * @return true if the call was successful.
 * @return false if the call was unsuccessful.
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
				// ^ Allows instance to be referenced by initializer func (b/c the instance is already on the stack)

				Value initializer;
				if (tableGet(&klass->methods, OBJ_VAL(vm.initString), &initializer)) {
					return call(AS_CLOSURE(initializer), argCount); // call() handles arity checking
				} else if (argCount != 0) { // No initializer, so implicitly require 0 args
					runtimeError("Expected 0 arguments but got %d.", argCount);
					return false;
				}

				return true;
			}
			case OBJ_BOUND_METHOD: {
				ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
				// initCompiler sets aside slot 1 on methods for the instance and binds it to 'this', 
				// so we need to place a reference to the instance there
					vm.stackTop[-argCount - 1] = bound->receiver;
				return call(bound->method, argCount);
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

/** Binds a method to a class' methods table.
 * @param[in] name The property name to bind the method under.
 */
static void defineMethod(ObjString* name) {
	Value method = peek(0);
	ObjClass* klass = AS_CLASS(peek(1));
	tableSet(&klass->methods, OBJ_VAL(name), method);
	pop(); // Remove method from stack
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
static bool bindMethod(ObjClass* klass, ObjString* name) {
	Value method;
	if (!tableGet(&klass->methods, OBJ_VAL(name), &method)) { // If the method cannot be found, return errpr
		runtimeError("Undefined property '%s'.", name->chars);
		return false;
	}

	ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method));
	pop(); // instance
	push(OBJ_VAL(bound));
	return true;
}

static void resolveSafeCall(int argCount) {
	popn(argCount + 1);
	push(NIL_VAL);
}

/** Invokes a method directly from a class.
 * @param[in] klass The class to look for the method in.
 * @param[in] name The method to call.
 * @param[in] argCount The number of arguments being passed to the method.
 * @param[in] safe If true and the method is not found, nil will be placed on the stack instead of an error.
 * @return true If the call is successful.
 * @return false If the call is unsuccessful.
 */
static bool invokeFromClass(ObjClass* klass, ObjString* name, int argCount, bool safe) {
	Value method;
	if (!tableGet(&klass->methods, OBJ_VAL(name), &method)) {
		if (safe) {
			resolveSafeCall(argCount);
			return true;
		}
		runtimeError("Undefined property '%s'.", name->chars);
		return false;
	}

	return call(AS_CLOSURE(method), argCount);
}

/** Looks back on the stack to determine if a instance can be called upon.
 * If so, it gets called.
 * @param[in] name The method to call from the instance.
 * @param[in] argCount The number of arguments being passed to the method.
 * @param[in] safe If true and the method is not found, nil will be placed on the stack instead of an error.
 * @return true if the call succeeded.
 * @return false if the call failed.
 */
static bool invoke(ObjString* name, int argCount, bool safe) {
	Value receiver = peek(argCount); // instance we are calling on
	if (!IS_INSTANCE(receiver)) {
		printf("Safety: %d", safe);
		if (safe) {
			resolveSafeCall(argCount);
			return true;
		}
		runtimeError("Only instances have methods.");
		return false;
	}
	
	ObjInstance* instance = AS_INSTANCE(receiver);

	// Check if there is a callable field first
	Value value;
	if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
		vm.stackTop[-argCount - 1] = value; // Replace same spot as OP_GET_PROPERTY
		return callValue(value, argCount);
	}
				
	return invokeFromClass(instance->klass, name, argCount, safe);
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
				//~ Restores frame (returns execution back to the caller)
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
					frame->ip = ip;
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
					frame->ip = ip;
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
					frame->ip = ip;
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
					frame->ip = ip;
					runtimeError("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				ObjInstance* instance = AS_INSTANCE(peek(0));
				ObjString* name = READ_STRING();

				Value value;
				// If property exists, replace the top of the stack with it. Higher precidence than a method call
				if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
					pop(); // Instance
					push(value);
					break;
				}

				if (!bindMethod(instance->klass, name)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			case OP_GET_PROPERTY_LONG: {
				if (!IS_INSTANCE(peek(0))) {
					frame->ip = ip;
					runtimeError("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}
				ObjInstance* instance = AS_INSTANCE(peek(0));
				ObjString* name = READ_STRING_LONG();
				Value value;
				if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
					pop();
					push(value);
					break;
				}
				frame->ip = ip;
				runtimeError("Undefined property '%s'.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			case OP_GET_PROP_SAFE: {
				// b/c safe, we want '?.' to be chainable. If a non-instance is on top of the stack, we replace it with nil;
				if (!IS_INSTANCE(peek(0))) {
					READ_STRING(); // Skip the string. We dont use it, but it needs to be passed over by the VM
					if (!IS_NIL(peek(0))) {  // Avoiding this op does give a performance boost
						pop(); // Remove top and replace with nil
						push(NIL_VAL);
					}
					break;
				}
				ObjInstance* instance = AS_INSTANCE(peek(0));
				ObjString* name = READ_STRING();
				Value value;
				if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
					pop(); // Keep until tableGet is done to prevent GC from snatching it
					push(value);
					break;
				}
				pop();
				push(NIL_VAL);
				break;
			}
			case OP_GET_PROP_SAFE_LONG: {
				if (!IS_INSTANCE(peek(0))) {
					READ_STRING_LONG();
					if (!IS_NIL(peek(0))) {
						pop();
						push(NIL_VAL);
					}
					break;
				}
				ObjInstance* instance = AS_INSTANCE(peek(0));
				ObjString* name = READ_STRING_LONG();
				Value value;
				if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
					pop();
					push(value);
					break;
				}
				pop();
				push(NIL_VAL);
				break;
			}
			case OP_SET_PROPERTY: {
				// Ensure a valid recipient is under the top element of the stack
				// This instruction ONLY operates on the SPECIFIC instance on the top of the stack
				if (!IS_INSTANCE(peek(1))) {
					frame->ip = ip;
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
					frame->ip = ip;
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
			case OP_METHOD: {
				defineMethod(READ_STRING());
				break;
			}
			case OP_INVOKE: {
				ObjString* method = READ_STRING();
				int argCount = READ_BYTE();
				frame->ip = ip;
				if (!invoke(method, argCount, false)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm.frames[vm.frameCount - 1];
				ip = frame->ip;
				break;
			}
			case OP_INVOKE_SAFE: {
				ObjString* method = READ_STRING();
				int argCount = READ_BYTE();
				frame->ip = ip;
				if (!invoke(method, argCount, true)) {
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm.frames[vm.frameCount - 1];
				ip = frame->ip;
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


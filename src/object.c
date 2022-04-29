#include <stdlib.h> // snprintf
#include <stdio.h>
#include <string.h>
#include <math.h> // floor, log10, abs 

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

// Maximum string length when converting a number to a string
#define MAX_INT_STRLEN 50

// Allocates a new object with the given object prototype. Allows to type as input instead of raw size to support additions to the struct easily.
#define ALLOCATE_OBJ(type, objectType) \
	(type*) allocateObject(sizeof(type), objectType)

//~ Helper

/** Generates the FNV-1a hash value for a given character array and length.
 * 
 * @param[in] key A key character array.
 * @param[in] length The length of the input character array.
 * @return uint32_t hash value.
 */
static uint32_t hashString(const char* key, int length) {
	uint32_t hash = 2166136261u;
	for (int i = 0; i < length; i++) {
		hash ^= (uint8_t) key[i];	// 'Mix' in the input
		hash *= 16777619;					// 'Scramble' the bits
	}
	return hash;
}

//~ String Initialization and Setup

/** Allocates memory for a lox object based on the size and assigns an object type.
 * Also initializes the base object fields.
 *
 * @param[in] size The size of the object to allocate in bytes.
 * @param[in] type The type to assign to the new object.
 * @return Obj pointer to the newly allocated object.
 */
static Obj* allocateObject(size_t size, ObjType type) {
	Obj* object = (Obj*) reallocate(NULL, 0, size);
	object->type = type;

	// Set the new object list head to the new object
	object->next = vm.objects;
	object->isMarked = false;
	vm.objects = object;

#ifdef DEBUG_LOG_GC
	printf("%p allocate %zu for ", (void*) object, size);
	switch (type) {
		case OBJ_STRING: 				printf("string\n"); break;
		case OBJ_FUNCTION: 			printf("function\n"); break;
		case OBJ_CLOSURE: 			printf("closure\n"); break;
		case OBJ_NATIVE: 				printf("native\n"); break;
		case OBJ_UPVALUE: 			printf("upvalue\n"); break;
		case OBJ_CLASS: 				printf("class\n"); break;
		case OBJ_INSTANCE: 			printf("instance\n"); break;
		case OBJ_BOUND_METHOD:	printf("bound method\n"); break;
		case OBJ_LIST:					printf("list\n"); break;
	}
#endif

	return object;
}

ObjFunction* newFunction() {
	ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
	function->arity = 0;
	function->upvalueCount = 0;
	function->name = NULL;
	initChunk(&function->chunk);
	return function;
}

ObjNative* newNative(NativeFn function, int arity) {
	ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
	native->function = function;
	native->arity = arity;
	return native;
}

ObjClosure* newClosure(ObjFunction* function) {
	ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);
	for (int i = 0; i < function->upvalueCount; i++) {
		upvalues[i] = NULL;
	}
	ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = function->upvalueCount;
	return closure;
}

ObjUpvalue* newUpvalue(Value* slot) {
	ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
	upvalue->location = slot;
	upvalue->closed = NIL_VAL;
	upvalue->next = NULL;
	return upvalue;
}

ObjClass* newClass(ObjString* name) {
	ObjClass* klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
	klass->name = name;
	initTable(&klass->methods);
	return klass;
}

ObjInstance* newInstance(ObjClass* klass) {
	ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
	instance->klass = klass;
	initTable(&instance->fields);
	return instance;
}

ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method) {
	ObjBoundMethod* boundMethod = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
	boundMethod->receiver = receiver;
	boundMethod->method = method;
	return boundMethod;
}

ObjList* newList(Value* values, int count) {
	ObjList* list = ALLOCATE_OBJ(ObjList, OBJ_LIST);
	initValueArray(&list->entries);
	for (int i = 0; i < count; i++) {
		writeValueArray(&list->entries, values[i]);
	}
	return list;
}

/** Create a lox string object from a character array and length. The string is only pointed to by this object, so it must be freed by the string. 
 * Assumes that the input char array is null terminated and is not shared with any other pointer.
 *
 * @param[in] chars The characters to use in the string.
 * @param[in] length The length of the string.
 * @return ObjString* representing the lox string.
 */
static ObjString* allocateString(char* chars, int length, uint32_t hash) {
	// Allocate a single string object. The char array input has been previously allocated.
	ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
	string->length = length;
	string->chars = chars;
	string->hash = hash;
	
	push(OBJ_VAL(string)); // Push for GC
	tableSet(&vm.strings, OBJ_VAL(string), NIL_VAL); // Intern the string for future lookups
	pop();
	
	return string;
}

ObjString* takeString(char* chars, int length) {
	// Find the hash of the string
	uint32_t hash = hashString(chars, length);
	// Check to see if the string has been interned. If so, free the given string and return the existing pointer
	ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
	if (interned != NULL) {
		FREE_ARRAY(char, chars, length + 1);
		return interned;
	}
	// Create and return the new string with the input char array
	return allocateString(chars, length, hash);
}

ObjString* copyString(const char* chars, int length) {
	// Calculate the string hash
	uint32_t hash = hashString(chars, length);
	// 'Cheat' by returning interned strings before allocating if possible
	ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
	if (interned != NULL) return interned;
	// Allocate space for the copy of the chars plus the terminator. This will give the string object sole ownership of the string
	char* heapChars = ALLOCATE(char, length + 1);
	// Copy the source string lexeme into the new buffer
	memcpy(heapChars, chars, length);
	// Add the terminator
	heapChars[length] = '\0';
	// Create new ObjString object
	return allocateString(heapChars, length, hash);
}

//~ Utility Functions

static void printFunction(ObjFunction* function) {
	if (function->name == NULL) {
		printf("<script>");
		return;
	}
	printf("<fn %s>", function->name->chars);
}

static void printList(ObjList* list) {
	printf("[");
	if (list->entries.count > 0) {
		printValue(list->entries.values[0]);
		for (int count = 1; count < list->entries.count; count++) {
			printf(",");
			printValue(list->entries.values[count]);
		}
	}
	printf("]");
}

void printObject(Value value) {
	switch (OBJ_TYPE(value)) {
		case OBJ_FUNCTION:
			printFunction(AS_FUNCTION(value));
			break;
		case OBJ_STRING:
			printf("%s", AS_CSTRING(value));
			break;
		case OBJ_NATIVE:
			printf("<native fn>");
			break;
		case OBJ_CLOSURE:
			printFunction(AS_CLOSURE(value)->function);
			break;
		case OBJ_UPVALUE: // Not possible b/c upvalues arent user accessible
			printf("upvalue");
			break;
		case OBJ_CLASS:
			printf("%s", AS_CLASS(value)->name->chars);
			break;
		case OBJ_INSTANCE:
			printf("%s instance", AS_INSTANCE(value)->klass->name->chars);
			break;
		case OBJ_BOUND_METHOD:
			printFunction(AS_BOUND_METHOD(value)->method->function);
			break;
		case OBJ_LIST:
			printList(AS_LIST(value));
			break;
	}
}

ObjString* toObjString(Value value) {
#ifdef NAN_BOXING
	if (IS_NUMBER(value)) {
		int nchars = 1; // Default to 1 character for 0
		if (AS_NUMBER(value) != 0) {
			nchars = floor(log10(fabs(AS_NUMBER(value))));
		}
		if (AS_NUMBER(value) < 0) nchars++; // Add 1 for negative sign

		// Prepend ++ to allow for the terminating null character
		char* buffer = malloc(sizeof(char) * ++nchars); // allocate room for new text
		// todo : memory leak?
		int size = snprintf(buffer, nchars, "%g", AS_NUMBER(value)); // copy over number text

		return takeString(buffer, size); //take b/c we are allocating the string here
	} else if (IS_BOOL(value)) {
		if (AS_BOOL(value))
			return copyString("true", 4);
		else
			return copyString("false", 5);
	} else if (IS_NIL(value)) {
		return copyString("nil", 3);
	} else if (IS_OBJ(value)) {
		switch (OBJ_TYPE(value)) {
			case OBJ_STRING:
				return AS_STRING(value);
			default: // Impossible
				printf("Cannot convert to a string: %d\n'", OBJ_TYPE(value));
				return copyString("<not stringable>", 16);
		}
	} else {
			printf("Expected a string, but got ");
			printValue(value);
			printf("\n");
			return NULL;
	}
#else
	switch (value.type) {
		case VAL_NUMBER: {
			int nchars = 1; // Default to 1 character for 0
			if (AS_NUMBER(value) != 0) {
				nchars = floor(log10(fabs(AS_NUMBER(value))));
			}
			if (AS_NUMBER(value) < 0) nchars++; // Add 1 for negative sign

			// Prepend ++ to allow for the terminating null character
			char* buffer = malloc(sizeof(char) * ++nchars); // allocate room for new text
			// todo : memory leak?
			int size = snprintf(buffer, nchars, "%g", AS_NUMBER(value)); // copy over number text

			return takeString(buffer, size); //take b/c we are allocating the string here
		}
		case VAL_BOOL: {
			if (AS_BOOL(value))
				return copyString("true", 4);
			else
				return copyString("false", 5);
		}
		case VAL_NIL:
			return copyString("nil", 3);
		case VAL_OBJ: {
			switch (OBJ_TYPE(value)) {
				case OBJ_STRING:
					return AS_STRING(value);
				default: // Impossible
					printf("Cannot convert to a string: %d\n'", OBJ_TYPE(value));
					return copyString("<not stringable>", 16);
			}
		}
		default:
			printf("Expected a string, but got ");
			printValue(value);
			printf("\n");
			return NULL;
	}
#endif
}

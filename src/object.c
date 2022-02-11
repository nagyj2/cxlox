#include <stdlib.h> // snprintf
#include <stdio.h>
#include <string.h>

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
	vm.objects = object;

	return object;
}

ObjFunction* newFunction() {
	ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
	function->arity = 0;
	function->name = NULL;
	initChunk(&function->chunk);
	return function;
}

ObjNative* newNative(NativeFn function) {
	ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
	native->function = function;
	return native;
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
	tableSet(&vm.strings, OBJ_VAL(string), NIL_VAL); // Intern the string for future lookups
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
	}
}

ObjString* toObjString(Value value) {
	switch (value.type) {
		case VAL_NUMBER: {
			// todo: currently assumes largest required
			char* buffer = malloc(sizeof(char) * MAX_INT_STRLEN); // allocate room for new text
			// todo : memory leak?
			int size = snprintf(buffer, MAX_INT_STRLEN, "%g", AS_NUMBER(value)); // copy over number text

			return takeString(buffer, size);
		}
		case VAL_BOOL: {
			if (AS_BOOL(value))
				return takeString("true", 4);
			else
				return takeString("false", 5);
		}
		case VAL_NIL:
			return takeString("nil", 3);
		case VAL_OBJ: {
			switch (OBJ_TYPE(value)) {
				case OBJ_STRING:
					return AS_STRING(value);
				default: // Impossible
					printf("Cannot convert to a string: %d\n'", OBJ_TYPE(value));
					return takeString("??", 2);
			}
		}
		default:
			printf("Expected a string, but got ");
			printValue(value);
			printf("\n");
			return NULL;
	}
}

#include <stdlib.h> // snprintf
#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

// Maximum string length when converting a number to a string
#define MAX_INT_STRLEN 50

// Allocates a new object with the given object prototype. Allows to type as input instead of raw size to support additions to the struct easily.
#define ALLOCATE_OBJ(type, objectType) \
	(type*) allocateObject(sizeof(type), objectType)

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

/** Create a lox string object from a character array and length. The string is only pointed to by this object, so it must be freed by the string. 
 * Assumes that the input char array is null terminated and is not shared with any other pointer.
 *
 * @param[in] chars The characters to use in the string.
 * @param[in] length The length of the string.
 * @return ObjString* representing the lox string.
 */
static ObjString* allocateString(char* chars, int length) {
	// Allocate a single string object. The char array input has been previously allocated.
	ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
	string->length = length;
	string->chars = chars;
	return string;
}

ObjString* takeString(char* chars, int length) {
	// Create a new object and set the pointer to input char array
	return allocateString(chars, length);
}

ObjString* copyString(const char* chars, int length) {
	// Allocate space for the copy of the chars plus the terminator. This will give the string object sole ownership of the string
	char* heapChars = ALLOCATE(char, length + 1);
	// Copy the source string lexeme into the new buffer
	memcpy(heapChars, chars, length);
	// Add the terminator
	heapChars[length] = '\0';
	// Create new ObjString object
	return allocateString(heapChars, length);
}

void printObject(Value value) {
	switch (OBJ_TYPE(value)) {
		case OBJ_STRING:
			printf("%s", AS_CSTRING(value));
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

#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

// Returns the type of an object.
#define OBJ_TYPE(value)	(AS_OBJ(value)->type)

// Returns whether the object is a string.
#define IS_STRING(value)	isObjType(value, OBJ_STRING)

// Convert a lox value to a lox string. Returns ObjString pointer.
#define AS_STRING(value)	((ObjString*) AS_OBJ(value))
// Convert a lox string to a c char array. Returns the character array from ObjString.
#define AS_CSTRING(value)	(((ObjString*) AS_OBJ(value))->chars)
// Convert an arbitrary value to a lox string
#define TO_STRING(value)	toString(value);

/* Available types for lox objects. */
typedef enum {
	OBJ_STRING,
} ObjType;

/* Heap allocated lox object. Base 'class' for lox values. Typedef-ed in 'value.h'. */
struct Obj {
	ObjType type; // Type of the object.
	struct Obj* next; // Next object in the linked list.
};

/* Lox, heap allocated string. */
struct ObjString {
	Obj obj;				//* State inherited from Obj. Allows safe casting and calling of its type.
	int length;			//* Length of the string.
	char* chars;		//* Pointer to the string's characters.
	uint32_t hash;	//* Hash of the string.
};

//~ Semantics

/** Returns whether or not a value is an object of a specific type. 
 * Called from the macro IS_OBJ to prevent multiple executions of @p value argument.
 *
 * @param[in] value The value to test.
 * @param[in] type The type to test the value against.
 * @return true if @p value is of object type @p type.
 * @return false if @p value is not of object type @p type.
 */
static inline bool isObjType(Value value, ObjType type) {
	return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

/** Convert an arbitrary lox value to a lox string.
 * 
 * @param[in] value The value to convert.
 * @return ObjString* pointer to a newly allocated lox string.
 */
ObjString* toObjString(Value value);

//~ Helpers

/** Create a new lox string value by 'taking ownership' of the input character array.
 * @details
 * This function assumes that it can take ownership of the input chararacter array.
 * If support sequences were supported, this function would call another to translate them.
 *
 * @param[in] chars The start of the string.
 * @param[in] length The number of characters in @p chars.
 * @return ObjString* representing a lox string.
 */
ObjString* takeString(char* chars, int length);

/** Copies a string from the input source and places them into a lox value object.
 * @details
 * If support sequences were supported, this function would call another to translate them.
 *
 * @param[in] chars The start of the string to copy.
 * @param[in] length The number of characters to copy from @p chars.
 * @return ObjString* representing a lox string.
 */
ObjString* copyString(const char* chars, int length);

/** Prints an input object value to stdout.
 * 
 * @param[in] value The object to display.
 */
void printObject(Value value);

#endif /* clox_object_h */

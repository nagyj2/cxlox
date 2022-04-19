#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"
#include "chunk.h"
#include "table.h"

/*
 ~ When creating a new object type, modify:
 ~	IS_<> Macro
 ~	AS_<> Macro
 ~	new<>()
 ~	memory.c functions
 ~  printObject()
 */

// Returns the type of an object.
#define OBJ_TYPE(value)	(AS_OBJ(value)->type)

// Returns whether the object is a string.
#define IS_STRING(value) isObjType(value, OBJ_STRING)
// Returns whether the object is a function.
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
// Returns whether the object is a native function.
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
// Returns whether the object is a function closure.
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
// Returns whether the object is a function closure.
#define IS_UPVALUE(value) isObjType(value, OBJ_UPVALUE)
// Returns whether the object is a class.
#define IS_CLASS(value) isObjType(value, OBJ_CLASS)
// Returns whether the object is a class instance.
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)

// Convert a lox value to a lox string. Returns ObjString pointer.
#define AS_STRING(value) ((ObjString*) AS_OBJ(value))
// Convert a lox string to a c char array. Returns the character array from ObjString.
#define AS_CSTRING(value)	(((ObjString*) AS_OBJ(value))->chars)
// Convert an arbitrary value to a lox string
#define TO_STRING(value)	toString(value);
// Convert a lox value to a lox function. Returns a ObjFunction pointer.
#define AS_FUNCTION(value) ((ObjFunction*) AS_OBJ(value))
// Convert a lox value to a lox native function object. Returns a ObjNative pointer.
#define AS_NATIVE(value) ((ObjNative*) AS_OBJ(value))
// Convert a lox value to a native C function. Returns a C function.
#define AS_CNATIVE(value) (((ObjNative*) AS_OBJ(value))->function)

// // Convert a lox value to a native function. Returns a C function.
// #define AS_NATIVE(value) (((ObjNative*) AS_OBJ(value))->function)

// Convert a lox value into a closure object. Returns a ObjClosure pointer.
#define AS_CLOSURE(value) ((ObjClosure*) AS_OBJ(value))
// Convert a lox value into a an upvalue. Returns a Upvalue pointer.
#define AS_UPVALUE(value) ((ObjUpvalue*) AS_OBJ(value))
// Convert a lox value into a class. Returns a Class pointer.
#define AS_CLASS(value) ((ObjClass*) AS_OBJ(value))
// Convert a lox value into an instance. Returns a class instance pointer.
#define AS_INSTANCE(value) ((ObjInstance*) AS_OBJ(value))

/* Available types for lox objects. */
typedef enum {
	OBJ_FUNCTION,
	OBJ_NATIVE,
	OBJ_STRING,
	OBJ_CLOSURE,
	OBJ_UPVALUE,
	OBJ_CLASS,
	OBJ_INSTANCE,
} ObjType;

/* Heap allocated lox object. Base 'class' for lox values. Typedef-ed in 'value.h'. */
struct Obj {
	ObjType type; 		// Type of the object.
	bool isMarked; 		// Whether the object is marked for GC.
	struct Obj* next; // Next object in the linked list.
};

/* Internal representation of a Lox function. Functions are first class (can be passed around), so they are objects. */
typedef struct {
	Obj obj;					//* Holds type of object and pointer to the next/
	int arity;				//* Number of arguments the function accepts.
	int upvalueCount;	//* Number of upvalues the function has.
	Chunk chunk;			//* The function body.
	ObjString* name;	//* Name of the function.
} ObjFunction;

/* Function signature for all native functions. */
typedef Value(*NativeFn)(int argCount, Value* args);

/* Native C function executable from within lox. */
typedef struct {
	Obj obj;
	int arity; 					//* Number of arguments the function accepts.
	NativeFn function;	//* The C function to execute.
} ObjNative;

/* Lox, heap allocated string. */
struct ObjString {
	Obj obj;				//* State inherited from Obj. Allows safe casting and calling of its type.
	int length;			//* Length of the string.
	char* chars;		//* Pointer to the string's characters.
	uint32_t hash;	//* Hash of the string.
};

typedef struct ObjUpvalue ObjUpvalue;
/** Represents an upvalue at runtime. Points to a variable which may or may not be on the stack so that it can be
 * accessed even when off the stack.
 */
struct ObjUpvalue {
	Obj obj;
	Value* location;	//* Points to the closed over variable. Uses a pointer to refer to the value, resulting in aliasing (useful for closing).
	Value closed;			//* The memory location the value occupies AFTER it has been closed (removed from the stack). After closing, this value is aliased by 'location'.
	ObjUpvalue* next;	//* Next upvalue in the linked list.
};

/** Representation of a function closure. Stores variables which the function accesses which arent within its own scope.
 * To the user, they are exactly the same as functions. Runtime construct representing the environment of a called function.
 */
typedef struct {
	Obj obj;
	ObjFunction* function;	//* The function the closure wraps over.
	ObjUpvalue** upvalues;	//* An array of upvalue pointers the closure maintains.
	int upvalueCount;				//* The number of upvalues the closure maintains.
} ObjClosure;

/** Representation of a class. Used as factories to produce instances which are then used in the runtime of lox
 * The user defines classes.
 */
typedef struct {
	Obj obj;
	ObjString* name;				//* User visible name of the class
} ObjClass;

/** Representation of a class instance at runtime.
 */
typedef struct {
	Obj obj;
	ObjClass* klass;				//* The class which the instance represents.
	Table fields;						//* A hashmap containing the attributes of the instance.
} ObjInstance;

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

//~ Helpers

/** Convert an arbitrary lox value to a lox string.
 * 
 * @param[in] value The value to convert.
 * @return ObjString* pointer to a newly allocated lox string.
 */
ObjString* toObjString(Value value);

/** Creates and returns a new Lox function struct pointer.
 * 
 * @return ObjFunction* of the newly compiled function.
 */
ObjFunction* newFunction();

/** Wraps a native C function into a object value wrapper for lox.
 * @param[in] function 
 * @return ObjNative* pointer to a object which encapsulates the native function.
 */
ObjNative* newNative(NativeFn function, int arity);

/** Creates a new function closure.
 * @param[in] function The function the closure will emcompass.
 * @return ObjClosure* pointer to the newly created closure structure.
 */
ObjClosure* newClosure(ObjFunction* function);

/** Creates a new runtime upvalue object. 
 * @param[] slot 
 * @return ObjUpvalue* 
 */
ObjUpvalue* newUpvalue(Value* slot);

/** Creates a new class object.
 * @param[in] name The name to call the class
 * @return ObjClass* pointer to a newly created class struct.
 */
ObjClass* newClass(ObjString* name);

/** Creates a new class instance.
 * @param[in] class The class to create an instance of
 * @return ObjInstance* pointer to a newly created class instance.
 */
ObjInstance* newInstance(ObjClass* klass);

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

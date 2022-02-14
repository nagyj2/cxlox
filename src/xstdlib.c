#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>

#ifdef __APPLE__
// Apple clock function
#elif __WIN32
// Windows clock function
#else
#error "Unknown platform."
#endif

#include "stdlib.h"
#include "object.h"
#include "vm.h"

// Maximum length for a input string
#define MAX_STRING_LEN 256

//~ Helper Functions

void defineNative(const char* name, NativeFn function, int arity) {
	push(OBJ_VAL(copyString(name, (int) strlen(name))));
	push(OBJ_VAL(newNative(function, arity)));
	tableSet(&vm.globals, vm.stack[0], vm.stack[1]);
	pop();
	pop();
}

//~ Native Implementations
//! args is a pointer/ array, so it can be indexed at -1. This is where the calling function is placed. Return values can also be placed there

/** Returns the time elapsed since the start of the program.
 * @return Number of elapsed seconds since the program started execution.
 */
static Value elapsedNative(int argCount, Value* args) {
	return NUMBER_VAL((double) clock() / CLOCKS_PER_SEC);
}

/** Returns the current date and time.
 * @return String representation of the current date and time. 
 */
static Value dateNative(int argCount, Value* args) {
	time_t rawtime;
	time(&rawtime);
	struct tm  *timeinfo = localtime (&rawtime);
	char datetime[MAX_STRING_LEN];
	strftime(datetime, sizeof(datetime)-1, "%d.%m.%y %H:%M:%S", timeinfo);
	return OBJ_VAL(copyString(datetime, (int) strlen(datetime)));
}

/** Returns the current time in milliseconds since the Epoch.
 * @return Number of milliseconds since Epoch. 
 */
static Value millisNative(int argCount, Value* args) {
	long ms; 		// Milliseconds
	time_t s;  	// Seconds
	struct timespec spec;

	clock_gettime(CLOCK_REALTIME, &spec);

	s  = spec.tv_sec;
	ms = floor(spec.tv_nsec / 1.0e6); // Convert nanoseconds to milliseconds
	ms += s * 1000;
	return NUMBER_VAL(ms);
}

/** Returns the current time in seconds since the Epoch.
 * @return Number of seconds since Epoch. 
 */
static Value timeNative(int argCount, Value* args) {
	Value result = millisNative(argCount, args);
	return NUMBER_VAL(AS_NUMBER(result) / 1000);
}

/** Finds the characters in a string which occur before the first instance of a divider.
 * @param string The string to search.
 * @param divider The divider to search for.
 * @return String containing the characters before the divider.
 */
static Value strBeforeNative(int argCount, Value* args) {
	char* str = AS_CSTRING(args[0]);
	char* divider = AS_CSTRING(args[1]);
	char* pos = strstr(str, divider); // Find all chars before first divider
	if (!pos) {
		return NIL_VAL;
	}
	char newstr[MAX_STRING_LEN];
	memcpy(newstr, str, pos - str);
	return OBJ_VAL(copyString(newstr, (int) strlen(newstr)));
}

/** Finds the characters in a string which occur after the first instance of a divider.
 * @param string The string to search.
 * @param divider The divider to search for.
 * @return String containing the characters after the divider.
 */
static Value strAfterNative(int argCount, Value* args) {
	char* str = AS_CSTRING(args[0]);
	char* divider = AS_CSTRING(args[1]);
	char* pos = strstr(str, divider); // Find all chars before first divider
	if (!pos) {
		return NIL_VAL;
	}
	char newstr[MAX_STRING_LEN];
	memcpy(newstr, pos + 1, (str + strlen(str)) - pos);
	return OBJ_VAL(copyString(newstr, (int) strlen(newstr)));
}

static Value isNumberNative(int argCount, Value* args) {
	return BOOL_VAL(IS_NUMBER(args[0]));
}

static Value isBooleanNative(int argCount, Value* args) {
	return BOOL_VAL(IS_BOOL(args[0]));
}

static Value isNilNative(int argCount, Value* args) {
	return BOOL_VAL(IS_NIL(args[0]));
}

static Value isStringNative(int argCount, Value* args) {
	if (!IS_OBJ(args[0])) {
		return BOOL_VAL(false);
	}
	switch (OBJ_TYPE(args[0])) {
		case OBJ_STRING:
			return BOOL_VAL(true);
		default:
			return BOOL_VAL(false);
	}
}

static Value isFunctionNative(int argCount, Value* args) {
	if (!IS_OBJ(args[0])) {
		return BOOL_VAL(false);
	}
	switch (OBJ_TYPE(args[0])) {
		case OBJ_NATIVE:
		case OBJ_FUNCTION:
			return BOOL_VAL(true);
		default:
			return BOOL_VAL(false);
	}
}

/** Reads a string from the standard input.
 * @return String read from standard input without the newline.
 */
static Value readStringNative(int argCount, Value* args) {
	char input[MAX_STRING_LEN];
	fgets(input, sizeof(input), stdin);
	input[strcspn(input, "\n")] = 0; // Get input without the newline
	return OBJ_VAL(copyString(input, (int) strlen(input)));
}

/** Reads a number from standard input. Can accept integer and double values.
 * @return Number read from standard input.
 */
static Value readNumberNative(int argCount, Value* args) {
	double input;
	if (scanf("%lf", &input) != 1) {
		return NIL_VAL;
	}
	return NUMBER_VAL(input);
}

//~ Load Standard Library

void loadStdlib() {
	//~ Timing Functions
	defineNative("ticks", elapsedNative, 0);
	defineNative("date", dateNative, 0);
	defineNative("millis", millisNative, 0);
	defineNative("time", timeNative, 0);

	//~ String Manipulation Functions
	defineNative("strBefore", strBeforeNative, 2);
	defineNative("strAfter", strAfterNative, 2);

	//~ Type Tests
	defineNative("isnum", isNumberNative, 1);
	defineNative("isbool", isBooleanNative, 1);
	defineNative("isnil", isNilNative, 1);
	defineNative("isstr", isStringNative, 1);
	defineNative("isfun", isFunctionNative, 1);
	
	//~ Type Conversions
	// defineNative("num", convNumberNative, 1);
	// defineNative("bool", convBooleanNative, 1);
	// defineNative("str", convStringNative, 1);

	//~ Input Functions
	defineNative("strin", readStringNative, 0);
	defineNative("numin", readNumberNative, 0);
}

#ifndef clox_optionals_h
#define clox_optionals_h

#include "common.h"
#include "value.h"
#include "vm.h"

#include "math.h"

typedef Value(*BuiltinModule)(VM* vm);

typedef struct {
	const char* name;
	BuiltinModule module;
	bool CSource;
} BuiltinModules;

/** 
 * @param[out] vm 
 * @param[in] index The index in the BuiltinModules array to import
 * @return Value containing a BuiltinModule object
 */
Value importBuiltinModule(VM* vm, int index);

/** Find the index of a builtin module by name.
 * @param[in] name The name of the module to import.
 * @param[in] length The length of the name string.
 * @param[in] CSource Whether the module is a C source module.
 * @return int index of the module in the BuiltinModules array.
 */
int findBuiltinModule(char* name, int length, bool* CSource);

#endif /* clox_optionals_h */

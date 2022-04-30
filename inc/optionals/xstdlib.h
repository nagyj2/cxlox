#ifndef clox_stdlib_h
#define clox_stdlib_h

#include "common.h"
#include "optionals/optionals.h"
#include "object.h"
#include "vm.h"

/** Simulate defining a function, but assign a native function value instead of a lox function.
 * 
 * @param[in] name The native function's name.
 * @param[in] function The function implementation.
 */
void defineNative(const char* name, NativeFn function, int arity);

/** Loads the standard library into the VM's environment.
 */
void loadStdlib();

Value createStdlibModule(VM* vm);

#endif /* clox_stdlib_h */

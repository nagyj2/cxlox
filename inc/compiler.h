#ifndef clox_compiler_h
#define clox_compiler_h

#include "vm.h"

/** Tokenizes, parses and writes bytecode for the input source code and placed it into a chunk.
 * @param[in] source The source code to compile.
 * @return ObjFunction* The compiled code corresponding to the script or function.
 */
ObjFunction* compile(const char* source);

#endif /* clox_compiler_h */

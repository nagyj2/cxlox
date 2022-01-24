#ifndef clox_compiler_h
#define clox_compiler_h

#include "vm.h"

/** Tokenizes, parses and writes bytecode for the input source code and placed it into a chunk.
 * 
 * @param[in] source The source code to compile.
 * @param[out] chunk The chunk to place bytecode in.
 * @return true if there was no compilation error.
 * @return false if an error occured.
 */
bool compile(const char* source, Chunk* chunk);

#endif /* clox_compiler_h */

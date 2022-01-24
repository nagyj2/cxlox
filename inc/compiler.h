#ifndef clox_compiler_h
#define clox_compiler_h

/** tokenizes and compiles input source text to lox bytecode for immediate execution by the lox VM.
 * 
 * @param[in] source Source code to be compiled.
 */
void compile(const char* source);

#endif /* clox_compiler_h */

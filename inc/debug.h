#ifndef cloc_debug_h
#define clox_debug_h

#include "chunk.h"

/** Prints the bytecode contents of a chunk to stdout. Does not print metadata.
 * 
 * @param[in] chunk The chunk to be decoded.
 * @param[in] name The title to place before the chunk being decoded.
 */
void disassembleChunk(Chunk *chunk, const char *name);

/** Display an individual instruction from a chunk to stdout.
 * 
 * @param[in] chunk The chunk to index.
 * @param[in] offset A byte offset to the instruction to disassemble.
 * @return int An offset to the next instruction in the chunk.
 */
int disassembleInstruction(Chunk *chunk, int offset);

#endif /* cloc_debug_h */

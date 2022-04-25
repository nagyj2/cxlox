#include <stdio.h> // fprintf
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "chunk.h"
#include "vm.h"
#include "debug.h"
#include "util.h"

/** Start the REPL mode of the interpreter.
 * 
 */
static void repl() {
	char line[1024]; // Stores the input line
	for (;;) {
		printf("> ");

		// If the input is null, return.
		if (!fgets(line, sizeof(line), stdin)) {
			printf("\n");
			break;
		}

		interpret(line);
	}
}

/** Runs a lox source file. Dynamically allocates memory.
 * 
 * @param[in] path The path to the source file to be interpreted.
 */
static void runFile(const char* path) {
	char* source = readFile(path);
	InterpretResult result = interpret(source);
	free(source); // b/c readFile allocated memory AND the tokens point to the source, so they all need to be deleted before the source text.

	if (result == INTERPRET_COMPILE_ERROR) exit(65);
	if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}
	

int main(int argc, const char* argv[]) {
	initVM();
	
	if (argc == 1) {
		vm.isREPL = true;
		repl();
	} else if (argc == 2) {
		vm.isREPL = false;
		runFile(argv[1]);
	} else {
		fprintf(stderr, "Usage: clox [path]\n");
		exit(64);
	}
	
	freeVM();

	return 0;
}

#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

/** Scans and compiles a lox source file to bytecode.
 * 
 * @param[in] source The source code to transform to bytecode.
 */
void compile(const char* source) {
	initScanner(source);
	int line = -1;
	for (;;) {
		Token token = scanToken();
		if (token.line != line) {
			printf("%d ", token.line);
			line = token.line;
		} else {
			printf("   | ");
		}

		//~ '%.*s' allows for a precision (number of chars) as first input and chars as second
		printf("%2d '%.*s'\n", token.type, token.length, token.start);
		if (token.type == TOKEN_EOF)
			break;
	}
}

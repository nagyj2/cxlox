#ifndef clox_scanner_h
#define clox_scanner_h

/* Represents all available types of tokens available the compiler. */
typedef enum {
	// Single-char tokens
	TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
	TOKEN_LEFT_CURLY, TOKEN_RIGHT_CURLY,
	TOKEN_COMMA, TOKEN_DOT, TOKEN_SEMICOLON,
	TOKEN_QUESTION, TOKEN_COLON,

	// One or two char tokens
	TOKEN_BANG, TOKEN_BANG_EQUAL,
	TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
	TOKEN_GREATER, TOKEN_GREATER_EQUAL,
	TOKEN_LESSER, TOKEN_LESSER_EQUAL,
	TOKEN_MINUS, TOKEN_MINUS_EQUAL,
	TOKEN_PLUS, TOKEN_PLUS_EQUAL,
	TOKEN_STAR, TOKEN_STAR_EQUAL,
	TOKEN_SLASH, TOKEN_SLASH_EQUAL, 

	// Literals
	TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,

	// Keywords
	TOKEN_AND, TOKEN_BREAK,
	TOKEN_CLASS, TOKEN_CONTINUE, TOKEN_ELSE,
	TOKEN_FALSE, TOKEN_FOR, TOKEN_FUN, TOKEN_IF,
	TOKEN_LET, TOKEN_NIL, TOKEN_OR, TOKEN_PRINT,
	TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS, TOKEN_TRUE,
	TOKEN_VAR, TOKEN_WHILE,

	// Utility Tokens
	TOKEN_ERROR, // The scanner itself cannot 'throw' an error, so a synthetic error token is required.
	TOKEN_EOF // End of file.
} TokenType;

/* All information to represent a lexical token from the source code. Contains token type, source code line, start position and length. Lexeme points to source code allocation, so must be deleted before the original source code. */
typedef struct {
	TokenType type; 		//* The type of token being represented.
	const char* start; 	//* The index of the char in the source which starts the token's lexeme.
	int length; 				//* The length of the token's lexeme.
	int line; 					//* The line the token appeared in.
} Token;

/** Initialize the scanner's state, including the starting position, current position and line number. Resets the global scanner state.
 * 
 * @param[in] source The source code for the scanner to initialize with.
 */
void initScanner(const char* source);

/** Scans the currently set input source and returns the next token in that text stream. Reads the global scanner state.
 * 
 * @return Token The token which was read.
 */
Token scanToken();

#endif /* clox_scanner_h */

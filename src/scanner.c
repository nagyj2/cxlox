#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct {
	const char* start; 		//* Marks the beginning of the current lexeme.
	const char* current; 	//* marks the current char being examined.
	int line; 						//* The source line being read.
} Scanner;

// Like the VM, initialize a global scanner
Scanner scanner;

//~ Helper Functions

static bool isAtEnd() {
	return *scanner.current == '\0';
}

/** Turns the currently pointed to position in the scanner into a token and returns it.
 * 
 * @param[in] type The type to assign to the token.
 * @return Token The token representing the current source lexeme.
 */
static Token makeToken(TokenType type) {
	Token token;
	token.type = type;
	token.start = scanner.start;
	token.length = (int) (scanner.current - scanner.start);
	token.line = scanner.line;
	return token;
}

/** Create an error token with a lexeme matching the input message.
 * 
 * @param[in] message The message the token will point to.
 * @return Token An error token representing the error.
 */
static Token errorToken(const char* message) {
	Token token;
	token.type = TOKEN_ERROR;
	token.start = message;
	token.length = (int) strlen(message);
	token.line = scanner.line;
	return token;
}

/** Scan the next character in the source code and return it.
 * 
 * @return char The next character in the source code.
 */
static char advance() {
	scanner.current++;
	return scanner.current[-1];
}

/** Return whether the current char matches the input char.
 * 
 * @param[in] expected 
 * @return true If the current char is the expected char. Will also consume the char.
 * @return false If the current char is not the expected char.
 */
static bool match(char expected) {
	if (isAtEnd()) return false;
	if (*scanner.current != expected) return false;
	scanner.current++;
	return true;
}

/** Return the currently pointed to char.
 * 
 * @return char The current char being pointed to by the scanner.
 */
static char peek() {
	return *scanner.current;
}

/** Returns the character after the current char.
 * 
 * @return char The character ahead of the the current char.
 */
static char peekNext() {
	if (isAtEnd()) return '\0';
	return scanner.current[1];
}

/** Consume all whitespace characters and comments until a non-whitespace character is encountered.
 * 
 */
static void skipWhitespace() {
	for (;;) {
		char c = peek();
		switch (c) {
			case ' ':
			case '\r':
			case '\t':
				advance();
				break;
			case '\n':
				scanner.line++;
				advance();
				break;
			case '/':
				if (peekNext() == '/') { // Ensure it is '//' and not '/'
					// Comment goes until the end of the line.
					while (peek() != '\n' && !isAtEnd())
						advance();
				} else {
					return;
				}
			default:
				return;
		}
	}
}

/** Returns whether the current char in the source code is a digit.
 * 
 * @param[in] c The char to check.
 * @return true if the char is a digit.
 * @return false if the char is not a digit.
 */
static bool isDigit(char c) {
	return c >= '0' && c <= '9';
}

/** Returns whether the current char in the source code is a letter.
 * 
 * @param[in] c The char to check.
 * @return true if the char is a letter.
 * @return false if the char is not a letter.
 */
static bool isAlpha(char c) {
	return  (c >= 'a' && c <= 'z') ||
					(c >= 'A' && c <= 'Z') ||
					 c == '_';
}

/** Returns the token type of an examined keyword or the IDENTIFIER token.
 * Compares scanner's current minus start position to the input keyword to determine if there is a match
 *
 * @param[in] start The number of characters already compared at the string start.
 * @param[in] length The number of characters left to compare in the keyword.
 * @param[in] rest The keyword chaarcters left to compare themselves.
 * @param[in] type The type of keyword to return if a match is found.
 * @return TokenType of the input keyword or IDENTIFIER.
 */
static TokenType checkKeyword(int start, int length, const char* rest, TokenType type) {
	if (scanner.current - scanner.start == start + length &&
		memcmp(scanner.start + start, rest, length) == 0) {
		return type;
	}

	return TOKEN_IDENTIFIER;
}

static TokenType identifierType() {
	switch (scanner.start[0]) {
		case 'a': return checkKeyword(1, 2, "nd", TOKEN_AND);
		case 'c': return checkKeyword(1, 4, "lass", TOKEN_CLASS);
		case 'e': return checkKeyword(1, 3, "lse", TOKEN_ELSE);
		case 'f': {
			// Examine next char (if present) to know which words to search for.
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'a': return checkKeyword(2, 3, "lse", TOKEN_FALSE);
					case 'o': return checkKeyword(2, 1, "r", TOKEN_FOR);
					case 'u': return checkKeyword(2, 1, "n", TOKEN_FUN);
				}
			}
			break;
		}
		case 'i': return checkKeyword(1, 1, "f", TOKEN_IF);
		case 'n': return checkKeyword(1, 2, "il", TOKEN_NIL);
		case 'o': return checkKeyword(1, 1, "r", TOKEN_OR);
		case 'p': return checkKeyword(1, 4, "rint", TOKEN_PRINT);
		case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
		case 's': return checkKeyword(1, 4, "uper", TOKEN_SUPER);
		case 't': {
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'h': return checkKeyword(2, 2, "is", TOKEN_THIS);
					case 'r': return checkKeyword(2, 2, "ue", TOKEN_TRUE);
				}
			}
			break;
		}
		case 'v': return checkKeyword(1, 2, "ar", TOKEN_VAR);
		case 'w': return checkKeyword(1, 4, "hile", TOKEN_WHILE);
	}
	return TOKEN_IDENTIFIER;
}

//~ Literal Functions

/** Parses a string and returns the corresponding string token. The token points to the position in the scanner source code memory where the lexeme is.
 * 
 * @return Token representing the string.
 */
static Token string() {
	// This loop will put the entire string into the buffer between scanner start and current.
	while (peek() != '"' && !isAtEnd()) {
		if (peek() == '\n')
			scanner.line++;
		advance();
	}

	if (isAtEnd())
		return errorToken("Unertminated string.");

	advance(); // The terminating '"'
	return makeToken(TOKEN_STRING);
}

/** Parse a number with an optional decimal point and returns the corresponding number token. The token points to the position in the scanner source code memory where the lexeme is.
 * 
 * @return Token representing the number.
 */
static Token number() {
	while (isDigit(peek()))
		advance();

	// Look for a fractional part.
	if (peek() == '.' && isDigit(peekNext())) {
		// Consume '.'
		advance();
		while (isDigit(peek()))
			advance();
	}

	return makeToken(TOKEN_NUMBER);
}

/** Parse an identifier (or keyword) and returns the corresponding token. The token points to the position in the scanner source code memory where the lexeme is.
 * 
 * @return Token representing the keyword or identifier.
 */
static Token identifier() {
	while (isAlpha(peek()) || isDigit(peek()))
		advance();
	return makeToken(identifierType());
}

//~ Main Functions

void initScanner(const char* source) {
	scanner.start = source;
	scanner.current = source;
	scanner.line = 1;
}

Token scanToken() {
	skipWhitespace();
	scanner.start = scanner.current; // Remember the start of the token

	if (isAtEnd())
		return makeToken(TOKEN_EOF);

	char c = advance();

	if (isAlpha(c))
		return identifier();

	if (isDigit(c))
		return number();

	switch (c) {
		// Single character tokens
		case '(': return makeToken(TOKEN_LEFT_PAREN);
		case ')': return makeToken(TOKEN_RIGHT_PAREN);
		case '{': return makeToken(TOKEN_LEFT_CURLY);
		case '}': return makeToken(TOKEN_RIGHT_CURLY);
		case ';': return makeToken(TOKEN_SEMICOLON);
		case ',': return makeToken(TOKEN_COMMA);
		case '.': return makeToken(TOKEN_DOT);
		case '-': return makeToken(TOKEN_MINUS);
		case '+': return makeToken(TOKEN_PLUS);
		case '*': return makeToken(TOKEN_STAR);
		case '/': return makeToken(TOKEN_STAR);

		// Multi-character tokens
		case '!':
			return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
		case '=':
			return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
		case '<':
			return makeToken(match('=') ? TOKEN_LESSER_EQUAL : TOKEN_LESSER);
		case '>':
			return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);

		// Literals
		case '"':
			return string();
	}

	return errorToken("Unexpected charaction.");
}

#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

/* Stores the state of the scanner. Scanner is a global singleton */
typedef struct {
	const char* start; 		//* The beginning position of the current lexeme.
	const char* current; 	//* The current char being examined.
	int line; 						//* The source line being read.
} Scanner;

// Like the VM, initialize a global scanner to reduce number of function inputs. "Scanner's lexeme" refers to the characters between the start and current state positions.
Scanner scanner;

//~ Helper Functions

/** Returns whether the scanner is at the end of the input character stream.
 * 
 * @return true if the scanner has finished with the character steam.
 * @return false if there is at least one more character in the stream.
 */
static bool isAtEnd() {
	return *scanner.current == '\0';
}

/** Turns the currently pointed to position in the scanner into a token and returns it. Source lexeme is the characters between the scanner's start position and current position.
 * 
 * @param[in] type The type to assign to the token.
 * @return Token representing the current source lexeme.
 */
static Token makeToken(TokenType type) {
	Token token;
	token.type = type;
	token.start = scanner.start;
	token.length = (int) (scanner.current - scanner.start);
	token.line = scanner.line;
	return token;
}

/** Create an error token with a lexeme matching the input message. Used to indicate an error in the scanning process.
 * 
 * @param[in] message The message the token will point to. Must be a constant string.
 * @return Token representing the error encountered.
 */
static Token errorToken(const char* message) {
	Token token;
	token.type = TOKEN_ERROR;
	token.start = message;
	token.length = (int) strlen(message);
	token.line = scanner.line;
	return token;
}

/** Advance the scanner forward one token and return the character positioned there. Updated the global scanner state.
 * 
 * @return char which followed the current character.
 */
static char advance() {
	scanner.current++;
	return scanner.current[-1];
}

/** Return whether the current character matches the input character. If it does, consume it. Updates the global scanner state.
 * 
 * @param[in] expected The character to test against.
 * @return true if the current character is the expected character. Will also consume the character.
 * @return false if the current character is not the expected character.
 */
static bool match(char expected) {
	if (isAtEnd()) return false;
	if (*scanner.current != expected) return false;
	scanner.current++;
	return true;
}

/** Return the currently pointed to character by the scanner.
 * 
 * @return char being pointed to.
 */
static char peek() {
	return *scanner.current;
}

/** Returns the character after the currently pointed to character.
 *
 * @return char following the current character being pointed to.
 */
static char peekNext() {
	if (isAtEnd()) return '\0';
	return scanner.current[1];
}

/** Parses a block comment. Assumes the '/*' has NOT been consumed.
 * 
 * @return true if the block comment was closed properly.
 * @return false if the block comment is not closed.
 */
static bool skipBlockComment() {
	advance(); // Consume the '/'
	advance(); // Consume the '*'
	int nesting = 1;
	while (true) {
		if (isAtEnd())
			return false;
		char c = advance();

		if (c == '*' && match('/')) nesting--;
		else if (c == '/' && match('*')) nesting++;

		if (c == '\n') 		scanner.line++;
		if (nesting == 0)	return true;
	}
}


/** Consume all whitespace characters and comments until a non-whitespace/ non-comment character is encountered. 
 * Updates the global scanner state.
 */
static void skipWhitespace() {
	// Go until default case is reached
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
					if (peek() == '\n')
						scanner.line++;
					advance(); // Skip the newline
				} else if (peekNext() == '*') {
					skipBlockComment();
				} else {
					return;
				}
				break;
			default:
				return;
		}
	}
}

/** Returns whether the input character is a digit.
 * 
 * @param[in] c The character to check.
 * @return true if the character is a digit.
 * @return false if the character is not a digit.
 */
static bool isDigit(char c) {
	return c >= '0' && c <= '9';
}

/** Returns whether the input character is a letter.
 * 
 * @param[in] c The character to check.
 * @return true if the character is a letter.
 * @return false if the character is not a letter.
 */
static bool isAlpha(char c) {
	return  (c >= 'a' && c <= 'z') ||
					(c >= 'A' && c <= 'Z') ||
					 c == '_';
}

/** Returns the token type of an examined keyword or the IDENTIFIER token. 
 * Compares scanner's current lexeme to the input keyword to determine if there is a match.
 * If there is a match the token type representing the keyword is returned. Otherwise the IDENTIFIER token type is returned.
 *
 * @param[in] start The number of characters already compared from the keyword.
 * @param[in] length The number of characters left to compare in the keyword.
 * @param[in] rest The keyword characters left to compare themselves.
 * @param[in] type The type of keyword to return if a match is found.
 * @return @p type TokenType if a match is found or IDENTIFIER.
 */
static TokenType checkKeyword(int start, int length, const char* rest, TokenType type) {
	if (scanner.current - scanner.start == start + length &&
		memcmp(scanner.start + start, rest, length) == 0) {
		return type;
	}

	return TOKEN_IDENTIFIER;
}

/** Determines if the scanner's lexeme is a keyword and returns the appropriate token type or the IDENTIFIER token type.
 * 
 * @return TokenType if the corresponding keyword or identifier.
 */
static TokenType identifierType() {
	// Split on the first character of the lexeme
	switch (scanner.start[0]) {
		case 'a': return checkKeyword(1, 2, "nd", TOKEN_AND);
		case 'b': return checkKeyword(1, 4, "reak", TOKEN_BREAK);
		case 'c': {
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'a': return checkKeyword(2, 2, "se", TOKEN_CASE);
					case 'l': return checkKeyword(2, 3, "ass", TOKEN_CLASS);
					case 'o': return checkKeyword(2, 6, "ntinue", TOKEN_CONTINUE);
				}
			}
			break;
		}
		case 'd': return checkKeyword(1, 6, "efault", TOKEN_DEFAULT);
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
		case 'l': return checkKeyword(1, 2, "et", TOKEN_LET);
		case 'n': return checkKeyword(1, 2, "il", TOKEN_NIL);
		case 'o': return checkKeyword(1, 1, "r", TOKEN_OR);
		case 'p': return checkKeyword(1, 4, "rint", TOKEN_PRINT);
		case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
		case 's': {
			if (scanner.current - scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'u': return checkKeyword(2, 3, "per", TOKEN_SUPER);
					case 'w': return checkKeyword(2, 4, "itch", TOKEN_SWITCH);
				}
			}
			break;
		}
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
	// If no matches were found, the identifier was scanned
	return TOKEN_IDENTIFIER;
}

//~ Literal Parsing Functions

/** Parses a string and returns a corresponding string token. The token points to the position in the scanner source code memory where the lexeme is.
 * Updates the global scanner state.
 *
 * @return Token representing the string.
 */
static Token string() {
	// This loop will put the entire string into the buffer between scanner start and current.
	char escaped[] = "\\ntr\"";
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

/** Parse a decimal or integer number and returns a corresponding number token. The token points to the position in the scanner source code memory where the lexeme is. 
 * Updates the global scanner state.
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
 * Updates the global scanner state.
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

	char c = advance(); // Get first character

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
		case '?': return makeToken(TOKEN_QUESTION);
		case ':': return makeToken(TOKEN_COLON);

		// Multi-character tokens
		case '-':
			return makeToken(match('=') ? TOKEN_MINUS_EQUAL : TOKEN_MINUS);
		case '+': 
			return makeToken(match('=') ? TOKEN_PLUS_EQUAL : TOKEN_PLUS);
		case '*': 
			return makeToken(match('=') ? TOKEN_STAR_EQUAL : TOKEN_STAR);
		case '/': 
			return makeToken(match('=') ? TOKEN_SLASH_EQUAL : TOKEN_SLASH);
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

	// printf("Unexpected character> '%c'\n", c);
	return errorToken("Unexpected character.");
}

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

/** Currently unused but implemented functions
 * - backTrack()
 */

//~ Helper Functions

void initScanner(Scanner* scanner, const char* source) {
	scanner->start = source;
	scanner->current = source;
	scanner->line = 1;
}

/** Returns whether the input character is a digit.
 * 
 * @param[in] c The character to check.
 * @return true if the character is a digit.
 * @return false if the character is not a digit.
 * @param[in] scanner The scanner to perform the operation on.
 */
static bool isDigit(char c) {
	return c >= '0' && c <= '9';
}

/** Returns whether the input character is a letter.
 * 
 * @param[in] c The character to check.
 * @return true if the character is a letter.
 * @return false if the character is not a letter.
 * @param[in] scanner The scanner to perform the operation on.
 */
static bool isAlpha(char c) {
	return  (c >= 'a' && c <= 'z') ||
					(c >= 'A' && c <= 'Z') ||
					 c == '_';
}

// todo add binary, octal, hex

/** Returns whether the scanner is at the end of the input character stream.
 * 
 * @return true if the scanner has finished with the character steam.
 * @return false if there is at least one more character in the stream.
 * @param[in] scanner The scanner to perform the operation on.
 */
static bool isAtEnd(Scanner* scanner) {
	return *scanner->current == '\0';
}

/** Advance the scanner forward one token and return the character positioned there. Updated the global scanner state.
 * 
 * @return char which followed the current character.
 * @param[in] scanner The scanner to perform the operation on.
 */
static char advance(Scanner* scanner) {
	scanner->current++;
	return scanner->current[-1];
}

/** Return whether the current character matches the input character. If it does, consume it. Updates the global scanner state.
 * 
 * @param[in] expected The character to test against.
 * @return true if the current character is the expected character. Will also consume the character.
 * @return false if the current character is not the expected character.
 * @param[in] scanner The scanner to perform the operation on.
 */
static bool match(Scanner* scanner, char expected) {
	if (isAtEnd(scanner)) return false;
	if (*scanner->current != expected) return false;
	scanner->current++;
	return true;
}

/** Return the currently pointed to character by the scanner.
 * 
 * @return char being pointed to.
 * @param[in] scanner The scanner to perform the operation on.
 */
static char peek(Scanner* scanner) {
	return *scanner->current;
}

/** Returns the character after the currently pointed to character.
 *
 * @return char following the current character being pointed to.
 * @param[in] scanner The scanner to perform the operation on.
 */
static char peekNext(Scanner* scanner) {
	if (isAtEnd(scanner)) return '\0';
	return scanner->current[1];
}

/** Traverse backwards through the scanner's input. Scanner needs to be 'reprimed' afterwards.
 * To go back the length of the last token, use scanner->previous.length.
 * @param[in] scanner The scanner to perform the operation on.
 */
void backTrack(Scanner* scanner) {
	scanner->current--;
}

/** Turns the currently pointed to position in the scanner into a token and returns it. Source lexeme is the characters between the scanner's start position and current position.
 * 
 * @param[in] type The type to assign to the token.
 * @return Token representing the current source lexeme.
 * @param[in] scanner The scanner to perform the operation on.
 */
static Token makeToken(Scanner* scanner, TokenType type) {
	Token token;
	token.type = type;
	token.start = scanner->start;
	token.length = (int) (scanner->current - scanner->start);
	token.line = scanner->line;
	return token;
}

/** Create an error token with a lexeme matching the input message. Used to indicate an error in the scanning process.
 * 
 * @param[in] message The message the token will point to. Must be a constant string.
 * @return Token representing the error encountered.
 * @param[in] scanner The scanner to perform the operation on.
 */
static Token errorToken(Scanner* scanner, const char* message) {
	Token token;
	token.type = TOKEN_ERROR;
	token.start = message;
	token.length = (int) strlen(message);
	token.line = scanner->line;
	return token;
}

/** Parses a block comment. Assumes the '/ *' has NOT been consumed.
 * 
 * @return true if the block comment was closed properly.
 * @return false if the block comment is not closed.
 * @param[in] scanner The scanner to perform the operation on.
 */
static bool skipBlockComment(Scanner* scanner) {
	advance(scanner); // Consume the '/'
	advance(scanner); // Consume the '*'
	int nesting = 1;
	while (true) {
		if (isAtEnd(scanner))
			return false;
		char c = advance(scanner);

		if (c == '*' && match(scanner, '/')) nesting--;
		else if (c == '/' && match(scanner, '*')) nesting++;

		if (c == '\n') 		scanner->line++;
		if (nesting == 0)	return true;
	}
}

/** Consume all whitespace characters and comments until a non-whitespace/ non-comment character is encountered. 
 * @param[in] scanner The scanner to perform the operation on.
 */
static void skipWhitespace(Scanner* scanner) {
	// Go until default case is reached
	for (;;) {
		char c = peek(scanner);
		switch (c) {
			case ' ':
			case '\r':
			case '\t':
				advance(scanner);
				break;
			case '\n':
				scanner->line++;
				advance(scanner);
				break;
			case '/':
				if (peekNext(scanner) == '/') { // Ensure it is '//' and not '/'
					// Comment goes until the end of the line.
					while (peek(scanner) != '\n' && !isAtEnd(scanner))
						advance(scanner);
					if (peek(scanner) == '\n')
						scanner->line++;
					advance(scanner); // Skip the newline
				} else if (peekNext(scanner) == '*') {
					skipBlockComment(scanner);
				} else {
					return;
				}
				break;
			default:
				return;
		}
	}
}

/** Replace all instances of a substring in another string.
 * Will create a new string allocation, so it must be tracked and freed.
 * @details
 * Shamelessly taken from https://github.com/ghost-language/cghost/blob/master/src/scanner.c
 * @param[in] str The string to search through.
 * @param[in] substr The substring to replace.
 * @param[in] replacement What to place where the substring was
 * @param[in] scanner The scanner to perform the operation on.
 */
static void strReplace(char* str, char* substr, char replacement) {
	char* found = NULL; // Track if the substr was found
	int len = strlen(substr);
	int difflen = len - 1;
	// Continually replace the substring
	while ((found = strstr(str, substr)) != NULL) {
		*found = replacement;
		if (len != 1) {
			int left = strlen(str) - (found - str); // Find how much string is left
			memmove(found + 1, found + 1 + difflen, left - (difflen + 1)); // Move the rest of the string over
			memset(str + strlen(str) - difflen, 0, difflen);
		}
	}
}

//~ Keyword Parsing

/** Returns the token type of an examined keyword or the IDENTIFIER token. 
 * Compares scanner's current lexeme to the input keyword to determine if there is a match.
 * If there is a match the token type representing the keyword is returned. Otherwise the IDENTIFIER token type is returned.
 * @param[in] start The number of characters already compared from the keyword.
 * @param[in] length The number of characters left to compare in the keyword.
 * @param[in] rest The keyword characters left to compare themselves.
 * @param[in] type The type of keyword to return if a match is found.
 * @return @p type TokenType if a match is found or IDENTIFIER.
 * @param[in] scanner The scanner to perform the operation on.
 */
static TokenType checkKeyword(Scanner* scanner,int start, int length, const char* rest, TokenType type) {
	if (scanner->current - scanner->start == start + length &&
		memcmp(scanner->start + start, rest, length) == 0) {
		return type;
	}

	return TOKEN_IDENTIFIER;
}

/** Determines if the scanner's lexeme is a keyword and returns the appropriate token type or the IDENTIFIER token type.
 * @return TokenType if the corresponding keyword or identifier.
 * @param[in] scanner The scanner to perform the operation on.
 */
static TokenType identifierType(Scanner* scanner) {
	// Split on the first character of the lexeme
	switch (scanner->start[0]) {
		case 'a': return checkKeyword(scanner, 1, 2, "nd", TOKEN_AND);
		case 'b': return checkKeyword(scanner, 1, 4, "reak", TOKEN_BREAK);
		case 'c': {
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'a': return checkKeyword(scanner, 2, 2, "se", TOKEN_CASE);
					case 'l': return checkKeyword(scanner, 2, 3, "ass", TOKEN_CLASS);
					case 'o': return checkKeyword(scanner, 2, 6, "ntinue", TOKEN_CONTINUE);
				}
			}
			break;
		}
		case 'd':
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'e':
						if (scanner->current - scanner->start > 2) {
							switch (scanner->start[2]) {
								case 'f': return checkKeyword(scanner, 3, 4, "ault", TOKEN_DEFAULT);
								case 'l': return checkKeyword(scanner, 3, 0, "", TOKEN_DEL);
							}
						}
						break;
				}
			}
			break;
		case 'e': return checkKeyword(scanner, 1, 3, "lse", TOKEN_ELSE);
		case 'f': {
			// Examine next char (if present) to know which words to search for.
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'a': return checkKeyword(scanner, 2, 3, "lse", TOKEN_FALSE);
					case 'o': return checkKeyword(scanner, 2, 1, "r", TOKEN_FOR);
					case 'u': return checkKeyword(scanner, 2, 1, "n", TOKEN_FUN);
				}
			}
			break;
		}
		case 'i':
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'f': return checkKeyword(scanner, 2, 0, "", TOKEN_IF);
					case 'n': return checkKeyword(scanner, 2, 5, "clude", TOKEN_IMPORT);
				}
			}
		case 'l': return checkKeyword(scanner, 1, 2, "et", TOKEN_LET);
		case 'n': return checkKeyword(scanner, 1, 2, "il", TOKEN_NIL);
		case 'o': return checkKeyword(scanner, 1, 1, "r", TOKEN_OR);
		case 'p': return checkKeyword(scanner, 1, 4, "rint", TOKEN_PRINT);
		case 'r': return checkKeyword(scanner, 1, 5, "eturn", TOKEN_RETURN);
		case 's': {
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'u': return checkKeyword(scanner, 2, 3, "per", TOKEN_SUPER);
					case 'w': return checkKeyword(scanner, 2, 4, "itch", TOKEN_SWITCH);
				}
			}
			break;
		}
		case 't': {
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'h': return checkKeyword(scanner, 2, 2, "is", TOKEN_THIS);
					case 'r': return checkKeyword(scanner, 2, 2, "ue", TOKEN_TRUE);
				}
			}
			break;
		}
		case 'v': return checkKeyword(scanner, 1, 2, "ar", TOKEN_VAR);
		case 'w': return checkKeyword(scanner, 1, 4, "hile", TOKEN_WHILE);
	}
	// If no matches were found, the identifier was scanned
	return TOKEN_IDENTIFIER;
}

//~ Literal Parsing

/** Parse an identifier (or keyword) and returns the corresponding token. The token points to the position in the scanner source code memory where the lexeme is.
 * @return Token representing the keyword or identifier.
 * @param[in] scanner The scanner to perform the operation on.
 */
static Token identifier(Scanner* scanner) {
	while (isAlpha(peek(scanner)) || isDigit(peek(scanner)))
		advance(scanner);
	return makeToken(scanner, identifierType(scanner));
}

/** Parse a decimal or integer number and returns a corresponding number token. The token points to the position in the scanner source code memory where the lexeme is. 
 * @return Token representing the number.
 * @param[in] scanner The scanner to perform the operation on.
 */
static Token number(Scanner* scanner) {
	while (isDigit(peek(scanner)))
		advance(scanner);

	// Look for a fractional part.
	if (peek(scanner) == '.' && isDigit(peekNext(scanner))) {
		// Consume '.'
		advance(scanner);
		while (isDigit(peek(scanner)))
			advance(scanner);
	}

	return makeToken(scanner, TOKEN_NUMBER);
}

/** Parses a single quoted string and returns a corresponding string token. Interpolation is not permitted.
 * The token points to the position in the scanner source code memory where the lexeme is.
 * @return Token representing the string.
 * @param[in] scanner The scanner to perform the operation on.
 */
static Token singleString(Scanner* scanner) {
	// This loop will put the entire string into the buffer between scanner start and current.
	TokenType type = TOKEN_STRING;
	while (peek(scanner) != '\'' && !isAtEnd(scanner)) {
		if (peek(scanner) == '\n')
			scanner->line++;
		advance(scanner);
	}

	if (isAtEnd(scanner))
		return errorToken(scanner, "Unterminated string.");

	advance(scanner); // The terminating '
	return makeToken(scanner, type);
}

/** Parses a double quoted string and returns a corresponding string token. Interpolation is permitted.
 * The token points to a separately malloced buffer containing the lexeme. It must be disposed of specially.
 * @return Token representing the string.
 * @param[in] scanner The scanner to perform the operation on.
 */
static Token doubleString(Scanner* scanner) {
	// This loop will put the entire string into the buffer between scanner start and current.
	TokenType type = TOKEN_STRING_INTERP;
	while (peek(scanner) != '"' && !isAtEnd(scanner)) {
		if (peek(scanner) == '\n')
			scanner->line++;
		if (peek(scanner) == '\\' && peekNext(scanner) == '"')
			advance(scanner);
		advance(scanner);
	}

	if (isAtEnd(scanner))
		return errorToken(scanner, "Unterminated string.");

	advance(scanner); // The terminating "
	Token token = makeToken(scanner, type);

	// Skip past the " at start and end
	token.start++;
	token.length -= 2;

	char* newStr = (char*) calloc(1, token.length + 1);
	if (newStr == NULL)
		return errorToken(scanner, "Could not assign new string.");
	newStr[token.length] = '\0'; // Terminate the string
	strncpy(newStr, token.start, token.length); // Copy the string into the new string
	strReplace(newStr, "\\\"", '"');
	strReplace(newStr, "\\n", '\n');
	strReplace(newStr, "\\t", '\t');
	strReplace(newStr, "\\r", '\r');

	token.length = strlen(newStr);
	token.start = newStr;
	return token;
}

//~ Main exported function

Token scanToken(Scanner *scanner) {
	skipWhitespace(scanner);
	scanner->start = scanner->current; // Remember the start of the token

	if (isAtEnd(scanner))
		return makeToken(scanner, TOKEN_EOF);

	char c = advance(scanner); // Get first character

	if (isAlpha(c))
		return identifier(scanner);

	if (isDigit(c))
		return number(scanner);
	

	switch (c) {
		// Single character tokens
		case '(': return makeToken(scanner, TOKEN_LEFT_PAREN);
		case ')': return makeToken(scanner, TOKEN_RIGHT_PAREN);
		case '{': return makeToken(scanner, TOKEN_LEFT_CURLY);
		case '}': return makeToken(scanner, TOKEN_RIGHT_CURLY);
		case '[': return makeToken(scanner, TOKEN_LEFT_BRACE);
		case ']': return makeToken(scanner, TOKEN_RIGHT_BRACE);
		case ';': return makeToken(scanner, TOKEN_SEMICOLON);
		case ',': return makeToken(scanner, TOKEN_COMMA);
		case '.': return makeToken(scanner, TOKEN_DOT);
		case ':': return makeToken(scanner, TOKEN_COLON);
		// case '|': return makeToken(scanner, TOKEN_PIPE);

		// Common mistakes
		case '`':  return errorToken(scanner, "Strings use quotes only.");

		// Multi-character tokens
		case '?':
			return makeToken(scanner, match(scanner, '.') ? TOKEN_QUESTION_DOT : TOKEN_QUESTION);
		case '-':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_MINUS_EQUAL : match(scanner, '>') ? TOKEN_MINUS_GREATER : TOKEN_MINUS);
		case '+': 
			return makeToken(scanner, match(scanner, '=') ? TOKEN_PLUS_EQUAL : TOKEN_PLUS);
		case '*': 
			return makeToken(scanner, match(scanner, '=') ? TOKEN_STAR_EQUAL : TOKEN_STAR);
		case '/': 
			return makeToken(scanner, match(scanner, '=') ? TOKEN_SLASH_EQUAL : TOKEN_SLASH);
		case '!':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
		case '=':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_EQUAL_EQUAL : match(scanner, '>') ? TOKEN_EQUAL_GREATER : TOKEN_EQUAL);
		case '<':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_LESSER_EQUAL : match(scanner, '-') ? TOKEN_LESSER_MINUS : TOKEN_LESSER);
		case '>':
			return makeToken(scanner, match(scanner, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);

		// Literals
		case '"':
			return doubleString(scanner);
		case '\'':
			return singleString(scanner);
	}

	// printf("Unexpected character> '%c'\n", c);
	return errorToken(scanner, "Unexpected character.");
}

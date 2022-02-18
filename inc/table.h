#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

/** Maximum load factor. */
#define TABLE_MAX_LOAD 0.75

/** A entry to a hashtable. Stores the key which accesses the variable and the value being stored. */
typedef struct {
	ObjString* key;	//* Key used to access the entry. Assumes that a string will always index the table.
	Value value;		//* Value being stored.
} Entry;

/** Represents an instance of a hash table. */
typedef struct {
	int count;				//* The number of elements currently inside the table.
	int capacity;			//* The maximum number of elements the table can hold. Size of the space allocated for the table.
	Entry* entries;		//* Pointer to an array of entries which makeup the table.
} Table;

/** Initializes a hash table with metadata corresponding to an empty table
 * @details
 * Initializes to a count and capacity of 0. Does NOT allocate memory.
 *
 * @param[out] table Pointer to a table to be initialized.
 */
void initTable(Table* table);

/** Frees the memory allocated for a hash table.
 * 
 * @param[out] table Pointer to the table to be freed.
 */
void freeTable(Table* table);

/** Place a value into a hash table. May enlarge the table.
 * @details
 * If the table's max load factor is reached, the table will be resized and allocate new memory.
 * This table enlarging is checked before the value is placed into the table
 * This function assumes that the key is a string which contains the hash of the string.
 *
 * @param[in,out] table The table to place the value into.
 * @param[in] key The key to place at.
 * @param[in] value The value to place.
 * @return true If the insertion placed the value into the table.
 * @return false If the insertion did not actually insert an element.
 */
bool tableSet(Table* table, ObjString* key, Value value);

/** Retrieves a value from a given table by key.
 * @details
 * This function assumes that the key is a string which contains the hash of the string.
 *
 * @param[in] table The table to search through.
 * @param[in] key The key to find.
 * @param[out] value A pointer to the found value.
 * @return true if the key was found.
 * @return false if the key was not found.
 */
bool tableGet(Table* table, ObjString* key, Value* value);

/** Deletes a value from a table by key.
 * @details
 * Removes the entry by nullifying the key and value location in the table.
 * Performs extra logic to ensure no probe chains were broken.
 *
 * @param[in,out] table The table to remove the key from.
 * @param[in] key The key to the element to remove.
 * @return true if the removal occured.
 * @return false if there was no actual removal.
 */
bool tableDelete(Table* table, ObjString* key);

/** Copies over all contents of one table to another.
 * 
 * @param[in] from The table to copy contents from.
 * @param[out] to The table to copy contents to.
 */
void tableAddAll(Table* from, Table* to);

/** Searches the interned strings and returns a reference to the existing string if it exists.
 * 
 * @param[in] table Table to find the string key in
 * @param[in] chars The characters of the string to find.
 * @param[in] length The length of the input string.
 * @param[in] hash The hash value of the string.
 * @return ObjString* reference to the existing string.
 */
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash);

/** Mark all elements of a table as reachable. Marks the table contents and keys.
 * @param[out] table Table to mark as reachable.
 */
void markTable(Table* table);

/** Traverse the table and free all elements and keys which have not been marked.
 * @details
 * All references to the key indicate a key-value pair reference. By traversing the they keys,
 * if there a white key, the value can be safely freed.
 * @param[out] table The table to traverse.
 */
void tableRemoveWhite(Table* table);

#endif /* clox_table_h */

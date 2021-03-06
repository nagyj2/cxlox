#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

//~ Initialization

void initTable(Table* table) {
	table->count = 0;
	table->capacity = 0;
	table->entries = NULL;
}

void freeTable(Table* table) {
	FREE_ARRAY(Entry, table->entries, table->capacity);
	initTable(table); // Invalidates metadata
}

//~ Table Operations

/** Find a table entry corresponding to the given key.
 * @details
 * Uses linear probing for collision resolution.
 * Assumes there is always an empty entry somewhere in the table.
 * Takes an Entry array instead of a table because this function is called when tables are being resized and therefore dont fully exist yet.
 * Uses tombstones to mark deleted entries and are only visible internally. Tombstones are denoted by a NULL key and lox non-NULL value. Returned empty vs tombstone entries are indistinguishable from the key.
 *
 * @param[in] entries The table's Entry allocation array.
 * @param[in] capacity The capacity of the input @p entries Entry array.
 * @param[in] key Pointer to the lox string object to search for.
 * @return Entry* pointer to the found entry, or NULL if not found. NULL means an empty slot was found.
 */
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
	uint32_t index = key->hash & (capacity - 1); // \1 AND (cap - 1) === \1 MOD cap if \1 is a power of 2
	Entry* tombstone = NULL; // Store the last tombstone
	for (;;) {
		Entry* entry = &entries[index];
		if (entry->key == NULL) {
			// Found an empty entry. Is truly empty or a tombstone?
			if (IS_NIL(entry->value)) {
				// Empty. If we passed a tombstone, return this entry. Otherwise, return the tombstone
				// This allows overwriting and reusing tombstones
				return tombstone == NULL ? entry : tombstone;
			} else {
				// Tombstone. Update the last tombstone
				if (tombstone == NULL)
					tombstone = entry;
			}
		} else if (entry->key == key) {
			// Found the entry
			return entry;
		}

		// Linear probing
		index = (index + 1) & (capacity - 1);
	}
}

static void adjustCapacity(Table* table, int capacity) {
	// Allocate new memory for the table for a given size
	Entry* entries = ALLOCATE(Entry, capacity);
	// Nullify the new memory locations to set them to a valid state
	for (int i = 0; i < capacity; i++) {
		entries[i].key = NULL;
		entries[i].value = NIL_VAL;
	}

	// Reset table capacity
	table->count = 0;
	// Copy the old entries to the new table
	for (int i = 0; i < table->capacity; i++) {
		// See the entry in the old table
		Entry* entry = &table->entries[i];
		// If the entry is empty or tombstone, ignore it
		if (entry->key == NULL)
			continue;

		// Find the position in the new array and fill it
		Entry* dest = findEntry(entries, capacity, table->entries[i].key);
		dest->key = entry->key;
		dest->value = entry->value;
		table->count++; // Increment the new count only if a non-tombstone was found
	}

	// Free the old table
	FREE_ARRAY(Entry, table->entries, table->capacity);
	// Assign the new table allocation and capacity to the table
	table->entries = entries;
	table->capacity = capacity;
}

bool tableSet(Table* table, ObjString* key, Value value) {
	// If the the table gets too full, increase capacity
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity); // Get the next size
		// Need to copy over the old elements to the new allocation, so wrap that logic in a function
		adjustCapacity(table, capacity);
	}
	
	// Get entry from table
	Entry* entry = findEntry(table->entries, table->capacity, key);
	// Check if entry exists
	bool isNewKey = entry->key == NULL;
	// If non-tombstone is returned, increment the count. Tombstones from deletions do not decrement the table count
	if (isNewKey && IS_NIL(entry->value))
		table->count++;

	// Update entry. Entire table has been allocated and therefore initialized, so each entry is guaranteed to be valid.
	entry->key = key;
	entry->value = value;
	return isNewKey;
}

bool tableGet(Table* table, ObjString* key, Value* value) {
	// If there is nothing in the table, return false
	// Also prevents insertion of a NULL array
	if (table->count == 0)
		return false;

	// Find where the key is in the table (either present with some value, or empty; awaiting insertion)
	Entry* entry = findEntry(table->entries, table->capacity, key);
	// If the found slot is empty, return false
	if (entry->key == NULL)
		return false;

	// Set output pointer to the found entry
	*value = entry->value;
	return true;
}

bool tableDelete(Table* table, ObjString* key) {
	// Shortcut if the table is empty
	if (table->count == 0)
		return false;

	// Find the entry in the table and if it isn't there, return false
	Entry* entry = findEntry(table->entries, table->capacity, key);
	if (entry->key == NULL)
		return false;

	// Create tombstone in table
	entry->key = NULL;
	entry->value = BOOL_VAL(true);
	return true;
}

void tableAddAll(Table* from, Table* to) {
	for (int i = 0; i < from->capacity; i++) {
		Entry* entry = &from->entries[i];
		if (entry->key != NULL)
			tableSet(to, entry->key, entry->value);
		
	}
}

ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
	if (table->count == 0)
		return NULL;

	// Set starting index
	uint32_t index = hash & (table->capacity - 1);
	for (;;) {
		// Get the entry at the current index
		Entry* entry = &table->entries[index];
		if (entry->key == NULL) {
			// Stop only if we find an empty non-tombstone entry
			if (IS_NIL(entry->value))
				return NULL;
		} else if (entry->key->length == length
							&& entry->key->hash == hash
							&& memcmp(entry->key->chars, chars, length) == 0) {
			// Found it
			return entry->key;
		}

		index = (index + 1) & (table->capacity - 1);
	}
}

void markTable(Table* table) {
	for (int i = 0; i < table->capacity; i++) {
		Entry* entry = &table->entries[i];
		markObject((Obj*) (entry->key));
		markValue(entry->value);
	}
}

void tableRemoveWhite(Table* table) {
	for (int i = 0; i < table->capacity; i++) {
		Entry* entry = &table->entries[i];
		if (entry->key != NULL && !entry->key->obj.isMarked) {
			tableDelete(table, entry->key);
		}
	}
}

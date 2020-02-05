/**
 * Tridash Wasm32 Runtime Library
 * Copyright (C) 2020  Alexander Gutev
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Linking this library statically or dynamically with other modules is
 * making a combined work based on this library. Thus, the terms and
 * conditions of the GNU General Public License cover the whole
 * combination.
 *
 * As a special exception, the copyright holders of this library give
 * you permission to link this library with independent modules to
 * produce an executable, regardless of the license terms of these
 * independent modules, and to copy and distribute the resulting
 * executable under terms of your choice, provided that you also meet,
 * for each linked independent module, the terms and conditions of the
 * license of that module. An independent module is a module which is
 * not derived from or based on this library. If you modify this
 * library, you may extend this exception to your version of the
 * library, but you are not obliged to do so. If you do not wish to do
 * so, delete this exception statement from your version.
 */

#ifndef TRIDASH_HASH_TABLE_H
#define TRIDASH_HASH_TABLE_H

/**
 * Hash-table data structure and related functions.
 *
 * This is intended only to be used to store object descriptor subnode
 * maps. It is not a general purpose hash table type. This map is
 * constructed directly during compilation and as such no creation
 * functions are provided.
 */

#include <stdint.h>
#include <stddef.h>

#include "strings.h"

/**
 * Hash-table bucket storing a key and associated value.
 *
 * If the key is of size zero (the empty string) then the bucket has
 * no value.
 */
struct hash_bucket {
    struct string *key;
    uintptr_t value;
};

/**
 * Hash table data structure.
 */
struct hash_table {
    size_t num_buckets;
    struct hash_bucket buckets[];
};

/**
 * Lookup a key in a hash table.
 *
 * @param table The hash-table
 * @param key The key
 *
 * @return Pointer to the bucket in which the value is stored if the
 *   table contains an entry for the given key. If the table does not
 *   contain an entry for the given key, NULL is returned.
 */
struct hash_bucket *hash_table_lookup(struct hash_table *table, const struct string *key);

#endif /* TRIDASH_HASH_TABLE_H */

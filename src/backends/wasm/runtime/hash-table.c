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

#include "hash-table.h"

#define HASH_TABLE_SIZE offsetof(struct tridash_object, object.table) + sizeof(struct hash_table)

/**
 * Compute a hash code for a string.
 *
 * @param key The string
 * @return the hash code.
 */
size_t string_hash(const struct string *key);

/**
 * Check whether two strings are byte-equal.
 *
 * @param str1 String 1
 * @param str2 String 2
 *
 * @return True (1) if the strings are equal, false (0) otherwise.
 */
int string_equal(const struct string *str1, const struct string *str2);


struct hash_bucket *hash_table_lookup(struct hash_table *table, const struct string *key) {
    uintptr_t index = string_hash(key) % table->num_buckets;

    struct hash_bucket *bucket = &table->buckets[index];

    while (bucket->key) {
        if (string_equal(bucket->key, key))
            return bucket;

        index = (index + 1) % table->num_buckets;
        bucket = &table->buckets[index];
    }

    return NULL;
}


size_t string_hash(const struct string *key) {
    uintptr_t code = 1;

    for (size_t i = 0; i < key->size; ++i) {
        code = code * 31 + key->data[i];
    }

    return code;
}

int string_equal(const struct string *str1, const struct string *str2) {
    if (str1->size == str2->size) {
        for (size_t i = 0; i < str1->size; ++i) {
            if (str1->data[i] != str2->data[i])
                return 0;
        }

        return 1;
    }

    return 0;
}

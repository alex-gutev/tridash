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

#ifndef TRIDASH_STRINGS_H
#define TRIDASH_STRINGS_H

#include <stddef.h>
#include <stdint.h>

#include "macros.h"

/**
 * Tridash string object
 */
struct string {
    /** String size in bytes */
    size_t size;
    /** String byte data */
    char data[];
};

/**
 * Copy a string object.
 *
 * @param ptr Pointer to the string object
 *
 * @return Pointer to the copied string.
 */
void *gc_copy_string(const void *ptr);

/**
 * Return the pointer immediately following the string object, taking
 * into account the trailing padding bytes.
 *
 * @param str String object.
 *
 * @return The pointer to the first byte after the string object.
 */
void *string_end_ptr(struct string *str);

/**
 * Copy a string object.
 *
 * Unlike gc_copy_string this function can be used safely even if a
 * garbage collection cycle is initiated during copying.
 *
 * @param ptr Pointer to the string object
 *
 * @return Pointer to the copied string.
 */
void *copy_string(const void *ptr);


/**
 * Check whether two strings are byte-equal.
 *
 * @param str1 String 1
 * @param str2 String 2
 *
 * @return True (1) if the strings are equal, false (0) otherwise.
 */
int string_equal(const struct string *str1, const struct string *str2);

#endif /* TRIDASH_STRINGS_H */

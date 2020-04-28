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

#include "conversions.h"

#include "types.h"
#include "thunk.h"
#include "failures.h"
#include "strings.h"
#include "memory.h"

/**
 * Create a boxed integer object.
 *
 * If the integer value can fit in a tagged pointer, a tagged pointer
 * storing the immediate value is returned. Otherwise an integer
 * object is created on the heap.
 *
 * @param value The signed integer value.
 *
 * @return The integer object.
 */
static uintptr_t make_boxed_int(int32_t value);

/**
 * Parse an integer from a string.
 *
 * If the string does not contain a valid integer, a failure of type
 * `Invalid-Integer` is returned.
 *
 * @param str The string.
 *
 * @return The integer object, parsed from the string.
 */
static uintptr_t string_to_int(const struct string *str);


uintptr_t object_to_int(uintptr_t ptr) {
    ptr = resolve(ptr);

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_INT:
    case TAG_TYPE_FAIL:
        return ptr;

    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();
    }

    const struct tridash_object *obj = (void *)ptr;

    switch (obj->type) {
    case TRIDASH_TYPE_INT:
        return ptr;

    case TRIDASH_TYPE_FLOAT:
        return make_boxed_int((int32_t)obj->real);

    case TRIDASH_TYPE_STRING:
        return string_to_int(&obj->string);

    default:
        return make_fail_type_error();
    }
}

uintptr_t make_boxed_int(int32_t value) {
    if (MIN_IMMEDIATE_INT < value && value < MAX_IMMEDIATE_INT) {
        return TAG_INT((uintptr_t)value);
    }

    struct tridash_object *obj = alloc(offsetof(struct tridash_object, integer) + sizeof(int32_t));

    obj->type = TRIDASH_TYPE_INT;
    obj->integer = value;

    return (uintptr_t)obj;
}


/// Converting Strings to Integers

uintptr_t string_to_int(const struct string *str) {
    const char *chr = str->data;
    const char *end = &str->data[str->size];

    if (!str->size) return make_fail_invalid_integer();

    int32_t value = 0;
    int sign = 1;

    if (*chr == '-') {
        sign = -1;
        chr++;
    }

    while ((uintptr_t)chr < (uintptr_t)end) {
        if ('0' <= *chr && *chr <= '9')
            value = value * 10 + (*chr - '0');
        else
            return make_fail_invalid_integer();

        chr++;
    }

    return make_boxed_int(value * sign);
}

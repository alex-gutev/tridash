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

#include "equality.h"

#include "thunk.h"
#include "strings.h"
#include "types.h"
#include "memory.h"

/**
 * Checks whether an object is equivalent to an integer value.
 *
 * Equivalent means that if the object is a numeric value, then the it
 * true is returned if the value is equivalent to @a value regardless
 * of the actual numeric type.
 *
 * @param value The signed integer value.
 * @param obj Tridash Object.
 *
 * @return Tridash Boolean.
 */
static uintptr_t int_eq(int32_t value, uintptr_t obj);

/**
 * Checks whether an object is equivalent to an integer value.
 *
 * Equivalent means that if the object is a numeric value, then the it
 * true is returned if the value is equivalent to @a value regardless
 * of the actual numeric type.
 *
 * @param value The signed integer value.
 * @param obj Tridash Object.
 *
 * @return Tridash Boolean.
 */
static uintptr_t float_eq(float value, uintptr_t obj);


uintptr_t object_eq(uintptr_t a, uintptr_t b) {
    struct tridash_object *obj_a, *obj_b;

    SAVED_TPTR(b, a = resolve(a));
    SAVED_TPTR(a, b = resolve(b));

    switch (PTR_TAG(a)) {
    case TAG_TYPE_INT:
        return int_eq(INT_VALUE(a), b);

    case TAG_TYPE_FUNCREF:
        return TRIDASH_BOOL(a == b);

    case TAG_TYPE_FAIL:
        return a;

    default:
        obj_a = (void *)a;
    }

    switch (PTR_TAG(b)) {
    case TAG_TYPE_INT:
        return int_eq(INT_VALUE(b), a);

    case TAG_TYPE_FAIL:
        return b;

    case TAG_TYPE_FUNCREF:
        return TRIDASH_FALSE;

    default:
        obj_b = (void*)b;
    }

    switch (obj_a->type) {
    case TRIDASH_TYPE_INT:
        return int_eq(obj_a->integer, b);

    case TRIDASH_TYPE_FLOAT:
        return float_eq(obj_a->real, b);

    case TRIDASH_TYPE_STRING:
    case TRIDASH_TYPE_SYMBOL:
        return TRIDASH_BOOL(obj_b->type == obj_a->type && string_equal(&obj_a->string, &obj_b->string));

    case TRIDASH_TYPE_CHAR:
        return TRIDASH_BOOL(obj_b->type == TRIDASH_TYPE_CHAR && obj_a->char_code == obj_b->char_code);

    default:
        return TRIDASH_BOOL(a == b);
    }
}

uintptr_t object_neq(uintptr_t a, uintptr_t b) {
    uintptr_t result = object_eq(a, b);

    return IS_FAIL(result) ? result : result ^ 0x4;
}

uintptr_t symbol_eq(uintptr_t a, uintptr_t b) {
    SAVED_TPTR(b, a = resolve(a));
    SAVED_TPTR(a, b = resolve(b));

    if (IS_FAIL(a)) return a;
    if (IS_FAIL(b)) return b;

    if (PTR_TAG(a) || PTR_TAG(b))
        return TRIDASH_FALSE;

    const struct tridash_object *obj_a = (void *)a;
    const struct tridash_object *obj_b = (void *)b;

    // The following assumes that all symbols are created at
    // compile-time and stored in the constant data section, with a
    // unique symbol object per symbol name.

    return TRIDASH_BOOL(obj_a->type == TRIDASH_TYPE_SYMBOL &&
                        obj_b->type == TRIDASH_TYPE_SYMBOL &&
                        a == b);
}


/// Internal Comparison Functions

uintptr_t int_eq(int32_t value, uintptr_t obj) {
    struct tridash_object *object;

    switch (PTR_TAG(obj)) {
    case TAG_TYPE_PTR:
        object = (void *)obj;
        break;

    case TAG_TYPE_INT:
        return TRIDASH_BOOL(value == INT_VALUE(obj));

    case TAG_TYPE_FAIL:
        return obj;

    default:
        return TRIDASH_FALSE;
    }

    switch (object->type) {
    case TRIDASH_TYPE_INT:
        return TRIDASH_BOOL(value == object->integer);

    case TRIDASH_TYPE_FLOAT:
        return TRIDASH_BOOL(value == object->real);

    default:
        return TRIDASH_FALSE;
    }
}

uintptr_t float_eq(float value, uintptr_t obj) {
    struct tridash_object *object;

    switch (PTR_TAG(obj)) {
    case TAG_TYPE_PTR:
        object = (void *)obj;
        break;

    case TAG_TYPE_INT:
        return TRIDASH_BOOL(value == INT_VALUE(obj));

    case TAG_TYPE_FAIL:
        return obj;

    default:
        return TRIDASH_FALSE;
    }

    switch (object->type) {
    case TRIDASH_TYPE_INT:
        return TRIDASH_BOOL(value == object->integer);

    case TRIDASH_TYPE_FLOAT:
        return TRIDASH_BOOL(value == object->real);

    default:
        return TRIDASH_FALSE;
    }
}

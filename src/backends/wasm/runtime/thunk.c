/**
 * Tridash Wasm32 Runtime Library
 * Copyright (C) 2019-2020  Alexander Gutev
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

#include "thunk.h"
#include "copying.h"
#include "memory.h"

/**
 * Calls a thunk function.
 *
 * Pointer to the first word of the thunk.
 *
 * @return The value returned by the thunk function.
 */
static uintptr_t resolve_thunk(struct tridash_object *thunk);


uintptr_t resolve(uintptr_t value) {
    while (IS_REF(value)) {
        struct tridash_object *object = (void*)value;

        switch (object->type) {
        case TRIDASH_TYPE_RESOLVED_THUNK:
            value = object->object.resolved_value;
            break;

        case TRIDASH_TYPE_THUNK: {
            uintptr_t res = resolve_thunk(object);

            object->type = TRIDASH_TYPE_RESOLVED_THUNK;
            object->object.resolved_value = res;

            value = res;
        } break;

        default:
            return value;
        }
    }

    return value;
}

uintptr_t resolve_thunk(struct tridash_object *object) {
    return object->object.thunk.fn(&object->object.thunk.closure_size);
}


/// Copying

void *copy_thunk(const void *src) {
    const struct tridash_object *object = src;
    size_t size = offsetof(struct tridash_object, object) + sizeof(struct thunk) +
        object->object.thunk.closure_size * sizeof(uintptr_t);

    void *dest = alloc(size);
    memcopy(dest, src, size);

    return dest;
}

void *copy_thunk_result(void *src) {
    struct tridash_object *object = src;

    while (object->type == TRIDASH_TYPE_RESOLVED_THUNK) {
        src = (void*)object->object.resolved_value;

        if (!IS_REF(src))
            return src;

        object = src;
    }

    return copy_object(src);
}

void *copy_thunk_closure(void *ptr) {
    struct tridash_object *object = ptr;
    size_t size = object->object.thunk.closure_size;

    for (size_t i = 0; i < size; i++) {
        object->object.thunk.closure[i] =
            (uintptr_t)copy_object((void*)object->object.thunk.closure[i]);
    }

    return &object->object.thunk.closure[size];
}

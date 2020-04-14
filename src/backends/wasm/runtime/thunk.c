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
#include "lists.h"
#include "meta-nodes.h"

/**
 * Calls a thunk function.
 *
 * Pointer to the first word of the thunk.
 *
 * @return The value returned by the thunk function.
 */
static uintptr_t resolve_thunk(struct tridash_object *thunk);

/**
 * If @a val is a failure value, return the first alternative value,
 * which is not a failure.
 *
 * If all alternative values evaluate to failures, the last
 * alternative value is returned.
 *
 * @param val The primary value.
 *
 * @param first Pointer to the stack element containing the pointer to
 *   the first alternative value (to be considered).
 *
 * @param last Pointer to the stack element containing the pointer to
 *   the last alternative value (to be considered).
 *
 * @return The resulting value.
 */
static uintptr_t handle_failures(uintptr_t val, char ** first, char ** last);


uintptr_t resolve(uintptr_t value) {
    char ** last_alternative = stack_top;
    uintptr_t result;

    while (value && IS_REF(value)) {
        struct tridash_object *object = (void*)value;

        switch (object->type) {
        case TRIDASH_TYPE_RESOLVED_THUNK:
            value = object->resolved_value;
            break;

        case TRIDASH_TYPE_THUNK: {
            uintptr_t res = resolve_thunk(object);

            object->type = TRIDASH_TYPE_RESOLVED_THUNK;
            object->resolved_value = res;

            value = res;
        } break;

        case TRIDASH_TYPE_CATCH_THUNK:
            *stack_top-- = (char *)object->catch_thunk.fail_value;
            *stack_top-- = (char *)object->catch_thunk.test;

            value = object->catch_thunk.value;
            break;

        default:
            goto return_value;
        }
    }

return_value:
    result = handle_failures(value, stack_top+1, last_alternative);
    stack_top = last_alternative;

    return result;
}

uintptr_t resolve_thunk(struct tridash_object *object) {
    return object->thunk.fn(&object->thunk.closure_size);
}

uintptr_t handle_failures(uintptr_t val, char **first, char **last) {
    while (IS_FAIL(val) && ((uintptr_t)first <= (uintptr_t)last)) {
        const struct tridash_object *obj = (void *)(val & ~TAG_MASK);
        uintptr_t test = (uintptr_t)*first++;

        if (!test || resolve(call_meta_node_ref(test, obj->fail_type)) == TRIDASH_TRUE)
            val = resolve((uintptr_t)*first);

        first++;
    }

    return val;
}


/// Copying

void *copy_thunk(const void *src) {
    const struct tridash_object *object = src;
    size_t size = offsetof(struct tridash_object, thunk) + sizeof(struct thunk) +
        object->thunk.closure_size * sizeof(uintptr_t);

    void *dest = alloc(size);
    memcopy(dest, src, size);

    return dest;
}

void *copy_thunk_result(void *src) {
    struct tridash_object *object = src;

    while (object->type == TRIDASH_TYPE_RESOLVED_THUNK) {
        src = (void*)object->resolved_value;

        if (!IS_REF(src))
            return src;

        object = src;
    }

    return copy_object(src);
}

void *copy_thunk_closure(void *ptr) {
    struct tridash_object *object = ptr;
    size_t size = object->thunk.closure_size;

    for (size_t i = 0; i < size; i++) {
        object->thunk.closure[i] =
            (uintptr_t)copy_object((void*)object->thunk.closure[i]);
    }

    return &object->thunk.closure[size];
}


/// Catch Thunks

void *make_catch_thunk(uintptr_t value, uintptr_t fail_value, uintptr_t test) {
    struct tridash_object *thunk =
        alloc(offsetof(struct tridash_object, catch_thunk) + sizeof(struct catch_thunk));

    thunk->type = TRIDASH_TYPE_CATCH_THUNK;

    thunk->catch_thunk.value = value;
    thunk->catch_thunk.fail_value = fail_value;

    thunk->catch_thunk.test = test;

    return thunk;
}

void *copy_catch_thunk(const void *src) {
    const struct tridash_object *object = src;

    return make_catch_thunk(object->catch_thunk.value,
                            object->catch_thunk.fail_value,
                            object->catch_thunk.test);
}

void *copy_catch_thunk_objects(void *ptr) {
    struct tridash_object *object = ptr;

    object->catch_thunk.value = (uintptr_t)copy_object((void *)object->catch_thunk.value);
    object->catch_thunk.fail_value = (uintptr_t)copy_object((void *)object->catch_thunk.fail_value);
    object->catch_thunk.test = (uintptr_t)copy_object((void *)object->catch_thunk.test);

    return &object->catch_thunk.fail_value + 1;
}

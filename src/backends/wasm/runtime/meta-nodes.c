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

#include "meta-nodes.h"

#include "macros.h"
#include "types.h"
#include "memory.h"
#include "failures.h"

/**
 * Meta-Node reference function (without closure) type.
 */
typedef uintptr_t(*meta_node_ref_fn)(void * args);

/**
 * Returns the pointer to the meta-node reference function (without
 * closure).
 *
 * @param ptr Tagged function reference pointer.
 *
 * @return Meta-node reference function pointer.
 */
static meta_node_ref_fn get_ref_fn(uintptr_t ptr);

/**
 * Create an argument list containing a single argument.
 *
 * @param arg The argument.
 *
 * @return The argument list array.
 */
static void *make_arg_list(uintptr_t arg);


uintptr_t call_meta_node_ref(uintptr_t ref, uintptr_t arg) {
    SAVED_TPTR(arg, ref = resolve(ref));

    if (ref) {
        switch (PTR_TAG(ref)) {
        case TAG_TYPE_FUNCREF: {
            meta_node_ref_fn fn = get_ref_fn(ref);
            void *args = make_arg_list(arg);

            return fn(args);
        } break;

        case TAG_TYPE_FAIL:
            return ref;

        case TAG_TYPE_PTR: {
            struct tridash_object *obj = (void*)ref;

            if (obj->type == TRIDASH_TYPE_FUNCREF_ARGS) {
                SAVED_PTR(obj, void *args = make_arg_list(arg));
                void *defaults = &obj->funcref.args[0];

                return obj->funcref.fn(args, defaults);
            }
        } break;
        }
    }

    return make_fail_type_error();
}


meta_node_ref_fn get_ref_fn(uintptr_t ptr) {
    meta_node_ref_fn fn;

    // Inline assembly is used to avoid undefined behaviour of casting
    // a uintptr_t to a function pointer.

    asm("local.get %1;"
        "i32.const 2;"
        "i32.shr_u;"
        "local.set %0;"

        : "=r" (fn)
        : "r" (ptr));

    return fn;
}

void *make_arg_list(uintptr_t arg) {
    save_ptr((void *)arg);
    struct tridash_object *obj = alloc(3 * 4);

    obj->type = TRIDASH_TYPE_ARRAY;
    obj->array.size = 1;
    obj->array.elements[0] = (uintptr_t)restore_ptr();

    return (void *)obj;
}


/// Core Module Functions

/**
 * Convert an argument list to an array.
 *
 * @param args The argument list.
 *
 * @return Array containing all the elements in @a args.
 */
static uintptr_t arg_list_to_array(uintptr_t args);


uintptr_t meta_node_ref_apply(uintptr_t ref, uintptr_t args) {
    SAVED_TPTR(args, ref = resolve(ref));

    switch (PTR_TAG(ref)) {
    case TAG_TYPE_FUNCREF: {
        meta_node_ref_fn fn = get_ref_fn(ref);

        args = arg_list_to_array(args);
        if (IS_FAIL(args)) return args;

        struct tridash_object *arr = (void*)args;
        return fn(arr);
    } break;

    case TAG_TYPE_FAIL:
        return ref;

    case TAG_TYPE_PTR: {
        struct tridash_object *obj = (void*)ref;

        if (obj->type == TRIDASH_TYPE_FUNCREF_ARGS) {
            SAVED_PTR(obj, args = arg_list_to_array(args));
            if (IS_FAIL(args)) return args;

            void *defaults = &obj->funcref.args[0];

            struct tridash_object *arr = (void*)args;
            return obj->funcref.fn(arr, defaults);
        }
    } break;
    }

    return make_fail_type_error();
}

uintptr_t arg_list_to_array(uintptr_t args) {
    size_t length = 0;

    uintptr_t list = args;
    int more = 1;

    while (more) {
        SAVED_TPTR(args, list = resolve(list));

        if (list == empty_list())
            break;

        if (IS_FAIL(list))
            return list;
        else if (!IS_REF(list))
            return make_fail_type_error();

        struct tridash_object *obj = (void*)list;

        switch (obj->type) {
        case TRIDASH_TYPE_ARRAY:
            length += obj->array.size;
            more = 0;
            break;

        case TRIDASH_TYPE_LIST_NODE:
            length++;
            list = obj->list_node.tail;
            break;

        default:
            return make_fail_type_error();
        }
    }

    SAVED_TPTR(args, struct tridash_object *arr = alloc(TRIDASH_ARRAY_SIZE(length)));

    arr->type = TRIDASH_TYPE_ARRAY;
    arr->array.size = length;

    size_t elem = 0;
    more = 1;

    struct tridash_object *list_node = (void*)args;

    while (more) {
        SAVED_PTR(arr, list_node = (void*)resolve((uintptr_t)list_node));

        if ((uintptr_t)list_node == empty_list())
            break;

        switch (list_node->type) {
        case TRIDASH_TYPE_ARRAY:
            memcopy(arr->array.elements + elem, list_node->array.elements, list_node->array.size * sizeof(uintptr_t));
            more = 0;
            break;

        case TRIDASH_TYPE_LIST_NODE:
            arr->array.elements[elem++] = list_node->list_node.head;
            list_node = (void*)list_node->list_node.tail;
            break;
        }
    }

    return (uintptr_t)arr;
}

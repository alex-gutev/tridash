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
    ref = resolve(ref);

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
                void *args = make_arg_list(arg);
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
    struct tridash_object *obj = alloc(3 * 4);

    obj->type = TRIDASH_TYPE_ARRAY;
    obj->array.size = 1;
    obj->array.elements[0] = arg;

    return (void *)&obj->array.size;
}

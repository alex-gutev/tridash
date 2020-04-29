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

#include "copying.h"

#include "memory.h"
#include "types.h"
#include "thunk.h"
#include "strings.h"
#include "failures.h"
#include "arrays.h"
#include "funcrefs.h"
#include "objects.h"
#include "lists.h"

/**
 * Copy an object comprising two 32-bit words.
 *
 * @param src Pointer to the object.
 * @return Pointer to the copied copied object.
 */
static void *copy_dword(const void *src);


/// Generic Copy Function

void *copy_object(void *ptr) {
    if (IS_FAIL(ptr))
        ptr = (void*)((uintptr_t)ptr & ~(uintptr_t)TAG_MASK);

    else if (!IS_REF(ptr))
        return ptr;

    else if (!is_managed(ptr))
        return ptr;

    struct tridash_object *object = ptr;
    void *dest;

    switch(object->type) {
    case TRIDASH_TYPE_THUNK:
        dest = copy_thunk(object);
        break;

    case TRIDASH_TYPE_RESOLVED_THUNK:
        dest = copy_thunk_result(object);
        break;

    case TRIDASH_TYPE_CATCH_THUNK:
        dest = copy_catch_thunk(object);
        break;


    case TRIDASH_TYPE_INT:
    case TRIDASH_TYPE_FLOAT:
    case TRIDASH_TYPE_CHAR:
        dest = copy_dword(object);
        break;

    case TRIDASH_TYPE_STRING:
    case TRIDASH_TYPE_SYMBOL:
        dest = copy_string(object);
        break;

    case TRIDASH_TYPE_FAILURE:
        dest = (void*)((uintptr_t)copy_failure(object) | TAG_TYPE_FAIL);
        break;

    case TRIDASH_TYPE_FUNCREF_ARGS:
        dest = copy_funcref(object);
        break;

    case TRIDASH_TYPE_ARRAY:
    case TRIDASH_TYPE_INT_ARRAY:
        dest = copy_array(object);
        break;

    case TRIDASH_TYPE_OBJECT:
        dest = copy_user_object(object);
        break;

    case TRIDASH_TYPE_LIST_NODE:
        dest = copy_list_node(object);
        break;


    case TRIDASH_TYPE_FORWARD:
        return object->forward_ptr;
    }

    object->type = TRIDASH_TYPE_FORWARD;
    object->forward_ptr = dest;

    return dest;
}

void *copy_referenced_objects(void *ptr) {
    struct tridash_object *object = ptr;

    switch (object->type) {
    case TRIDASH_TYPE_THUNK:
        return copy_thunk_closure(ptr);

    case TRIDASH_TYPE_RESOLVED_THUNK:
        return &object->thunk.closure[object->thunk.closure_size];

    case TRIDASH_TYPE_CATCH_THUNK:
        return copy_catch_thunk_objects(object);


    case TRIDASH_TYPE_INT:
    case TRIDASH_TYPE_FLOAT:
    case TRIDASH_TYPE_CHAR:
        return &object->integer + 1;

    case TRIDASH_TYPE_STRING:
    case TRIDASH_TYPE_SYMBOL:
        return string_end_ptr(&object->string);

    case TRIDASH_TYPE_FAILURE:
        return copy_failure_type(ptr);

    case TRIDASH_TYPE_FUNCREF_ARGS:
        return copy_funcref_args(ptr);

    case TRIDASH_TYPE_ARRAY:
        return copy_array_elements(ptr);

    case TRIDASH_TYPE_OBJECT:
        return copy_user_object_subnodes(ptr);

    case TRIDASH_TYPE_INT_ARRAY:
        return &object->array.elements[object->array.size];

    case TRIDASH_TYPE_LIST_NODE:
        return copy_list_node_objects(object);
    }

    // The object should not be of any other type
    ASSERT_FAIL;

    return NULL;
}


/// Object Copying Functions

void *copy_dword(const void *src) {
    char *dest = alloc(2 * WORD_SIZE);

    // Consider whether this really needs to be implemented in inline
    // ASM

    asm("local.get %1;"
        "local.get %0;"
        "i64.load 0;"
        "i64.store 0;"

        : : "r" (src), "r" (dest));

    return dest;
}

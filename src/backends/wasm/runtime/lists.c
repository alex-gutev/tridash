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

#include "lists.h"

#include "types.h"
#include "memory.h"
#include "copying.h"
#include "thunk.h"
#include "failures.h"
#include "arrays.h"

#define TRIDASH_LIST_NODE_SIZE offsetof(struct tridash_object, list_node) + sizeof(struct list_node)

/**
 * Return the first element of an array.
 *
 * If the array is empty (which should not occur in generated Tridash
 * code), a Type-Error failure is returned.
 *
 * @param array The array.
 *
 * @return The first element.
 */
static uintptr_t array_first(const struct array *array);

/**
 * Return the remaining elements of an array, after the first, in a
 * linked list.
 *
 * If the array is empty (which should not occur in generated Tridash
 * code), a Type-Error failure is returned.
 *
 * @param array The array.
 *
 * @return Linked list of the remainder of the array
 */
static uintptr_t array_tail(const struct tridash_object *obj);


void * make_list_node(uintptr_t head, uintptr_t tail) {
    save_ptr((void *)tail);
    save_ptr((void *)head);

    struct tridash_object *object = alloc(TRIDASH_LIST_NODE_SIZE);

    object->type = TRIDASH_TYPE_LIST_NODE;
    object->list_node.head = (uintptr_t)restore_ptr();
    object->list_node.tail = (uintptr_t)restore_ptr();

    return object;
}


void * gc_copy_list_node(const void * src) {
    const struct tridash_object * node = src;

    return make_list_node(node->list_node.head, node->list_node.tail);
}

void * gc_copy_list_node_objects(void * src) {
    struct tridash_object * node = src;

    node->list_node.head = (uintptr_t)gc_copy_object((void *)node->list_node.head);
    node->list_node.tail = (uintptr_t)gc_copy_object((void *)node->list_node.tail);

    return &node->list_node.tail + 1;
}


/* Empty List */

static const struct tridash_object node_empty_list = {
    .type = TRIDASH_TYPE_NODE,
    .node = {index_empty_list}
};

uintptr_t empty_list(void) {
    return (uintptr_t)&node_empty_list;
}


/* Exported API Functions */

int is_list_node(uintptr_t obj) {
    obj = resolve(obj);

    if (IS_REF(obj)) {
        struct tridash_object *object = (void*)obj;

        return object->type == TRIDASH_TYPE_LIST_NODE;
    }

    return 0;
}

uintptr_t list_node_head(uintptr_t ptr) {
    ptr = resolve(ptr);

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_INT:
    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();

    case TAG_TYPE_FAIL:
        return ptr;
    }

    const struct tridash_object *obj = (void*)ptr;

    if (ptr == empty_list())
        return make_failure(ptr);

    switch (obj->type) {
    case TRIDASH_TYPE_LIST_NODE:
        return obj->list_node.head;

    case TRIDASH_TYPE_ARRAY:
        return array_first(&obj->array);

    default:
        return make_fail_type_error();
    }
}

uintptr_t list_node_tail(uintptr_t ptr) {
    ptr = resolve(ptr);

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_INT:
    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();

    case TAG_TYPE_FAIL:
        return ptr;
    }

    const struct tridash_object *obj = (void*)ptr;

    if (ptr == empty_list())
        return make_failure(ptr);

    switch (obj->type) {
    case TRIDASH_TYPE_LIST_NODE: {
        uintptr_t tail = obj->list_node.tail;

        return is_list_node(tail) || tail == empty_list() || IS_FAIL(tail) ? tail :
            make_fail_type_error();
    }

    case TRIDASH_TYPE_ARRAY:
        return array_tail(obj);

    default:
        return make_fail_type_error();
    }
}


/** Array Elements */

uintptr_t array_first(const struct array *array) {
    if (!array->size)
        return make_fail_type_error();

    return array->elements[0];
}

uintptr_t array_tail(const struct tridash_object *obj) {
    if (!obj->array.size)
        return make_fail_type_error();

    if (obj->array.size == 1)
        return empty_list();

    struct tridash_object *node = (void *)empty_list();

    for (size_t i = obj->array.size - 1; i > 0; i--) {
        SAVED_PTR(
            obj,
            node = make_list_node(obj->array.elements[i], (uintptr_t)node);
            );
    }

    return (uintptr_t)node;
}

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

#ifndef TRIDASH_LISTS_H
#define TRIDASH_LISTS_H

#include <stdint.h>
#include <stddef.h>

#include "macros.h"

/**
 * Linked List Node
 */
struct list_node {
    /** List Element Value */
    uintptr_t head;
    /** Pointer to next node */
    uintptr_t tail;
};

/**
 * Create a linked list node.
 *
 * @param head Value stored in the node.
 * @param tail Pointer to next node.
 */
export void * make_list_node(uintptr_t head, uintptr_t tail);

/**
 * Copy a linked list node.
 *
 * @param node The linked list node.
 *
 * @return The new node.
 */
void * copy_list_node(const void * node);

/**
 * Copy the head (value) and tail (next node pointer) of a linked list
 * node.
 *
 * @param node The linked list node.
 *
 * @return Pointer to the first byte after the linked list node.
 */
void * copy_list_node_objects(void * node);


/* Exported API Functions */

/**
 * Returns a pointer to the value representing the empty list.
 */
uintptr_t empty_list(void);

/**
 * Checks whether @a obj is a linked list node.
 *
 * @returns true if @a obj is a linked list node.
 */
int is_list_node(uintptr_t obj);

/**
 * Returns the element value stored in a linked list node.
 *
 * If @a node is not a linked list node, returns a failure.
 *
 * @param node The node.
 *
 * @return The value.
 */
export uintptr_t list_node_head(uintptr_t node);

/**
 * Returns the next node of a linked list node.
 *
 * If @a node is not a linked list node, returns a failure.
 *
 * @param node The node.
 *
 * @return Pointer to the next node
 */
export uintptr_t list_node_tail(uintptr_t node);

#endif /* TRIDASH_LISTS_H */

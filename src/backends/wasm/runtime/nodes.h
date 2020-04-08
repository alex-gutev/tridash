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

#ifndef TRIDASH_NODES_H
#define TRIDASH_NODES_H

#include <stdint.h>

/**
 * Represents a Tridash Node Object.
 *
 * An object of this type is created when taking a reference of a
 * node.
 */
struct tridash_node {
    /** Unique Node Index */
    uintptr_t index;
};


/* Builtin Type Nodes */

enum builtin_node_index {
    /** Failure Type: No Value */
    index_fail_no_value = 0,
    /** Failure Type: Type Error */
    index_fail_type_error = 1,

    /** Failure Type: Invalid Integer */
    index_fail_invalid_integer = 2,
    /** Failure Type: Invalid Real */
    index_fail_invalid_real = 3,

    /** Failure Type: Arity Error */
    index_fail_arity_error = 4,
    /** Failure Type: Index Out of Bounds */
    index_fail_out_bounds = 5,

    /** Node Representing the empty list */
    index_empty_list = 6
};

#endif /* TRIDASH_NODES_H */

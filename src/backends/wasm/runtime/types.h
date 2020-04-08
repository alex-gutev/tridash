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

#ifndef TRIDASH_TYPES_H
#define TRIDASH_TYPES_H

#include <stddef.h>
#include <stdint.h>

#include "thunk.h"
#include "strings.h"
#include "funcrefs.h"
#include "arrays.h"
#include "objects.h"

/// Object Type Constants

enum tridash_type {
    TRIDASH_TYPE_THUNK = 0,
    TRIDASH_TYPE_RESOLVED_THUNK = 1,

    TRIDASH_TYPE_INT = 2,
    TRIDASH_TYPE_FLOAT = 3,

    TRIDASH_TYPE_STRING = 4,

    TRIDASH_TYPE_FAILURE = 5,

    TRIDASH_TYPE_FUNCREF = 6,
    /* Function reference with optional/outer arguments */
    TRIDASH_TYPE_FUNCREF_ARGS = 7,

    TRIDASH_TYPE_ARRAY = 8,

    TRIDASH_TYPE_SYMBOL = 9,
    TRIDASH_TYPE_CHAR = 10,

    /* User-Defined Object */
    TRIDASH_TYPE_OBJECT = 11,

    /* Integer Array */
    TRIDASH_TYPE_INT_ARRAY = 12,

    /**
     * Indicates object was copied to new heap during garbage
     * collection. The pointer to the copied object is stored in the
     * old object.
     */
    TRIDASH_TYPE_FORWARD
};

/**
 * Tridash Object
 */
struct tridash_object {
    /** Object Type */
    uintptr_t type;

    /**
     * Object Data.
     *
     * Which field is used depends on 'type'.
     */
    union {
        /* Boxed Number Values */
        int32_t integer;
        float real;

        /* Thunks */
        struct thunk thunk;
        uintptr_t resolved_value;

        /* Strings */
        struct string string;

        /* Characters */
        uint32_t char_code;

        /* Failure Values */
        uintptr_t fail_type;

        /* GC Forwarding Pointer */
        void *forward_ptr;


        /* Function References */
        struct funcref funcref;

        /* Arrays */
        struct array array;

        /* User Objects */
        struct user_object object;
    };
};

#endif /* TRIDASH_TYPES_H */

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

#ifndef TRIDASH_OBJECTS_H
#define TRIDASH_OBJECTS_H

/* User Defined Objects */

#include <stdint.h>
#include <stddef.h>

#include "macros.h"
#include "hash-table.h"

/**
 * User-Defined Object Descriptor
 *
 * Stores a map which maps subnode identifiers to the array indices at
 * which their corresponding values are stored.
 */
struct object_descriptor {
    size_t num_fields;
    struct hash_table fields;
};

/**
 * User-Defined Object
 *
 * Contains an object descriptor, containing the indices where the
 * subnode values are stored and array which stores the subnode
 * values.
 */
struct user_object {
    struct object_descriptor *descriptor;
    uintptr_t fields[];
};


/// Copying

/**
 * Copy a user object. Only the references to the subnode values
 * stored in the object are copied, not the values themselves.
 *
 * @param src Pointer to the object
 * @return Pointer to the copied object
 */
void *copy_user_object(const void *src);

/**
 * Copy the subnode values stored in a user defined object and update
 * the references.
 *
 * @param src Pointer to the object
 * @return Pointer to the first byte following the object
 */
void *copy_user_object_subnodes(void *src);


/// Public Interface

/**
 * Retrieve the value corresponding to a subnode of an object.
 *
 * Implements the external `member` meta-node from the builtin module.
 *
 * @param object The object
 * @param field The subnode identifier
 *
 * @return The value. If there is no value corresponding to the
 *   subnode identifier @a field a failure of type `No-Value` is
 *   returned. If @a object is not a user-defined object, or @a field
 *   is not a valid subnode identifier a failure of type `Type-Error`
 *   is returned.
 */
export uintptr_t object_member(uintptr_t object, uintptr_t field);

#endif /* TRIDASH_OBJECTS_H */

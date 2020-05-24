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

#include "arrays.h"

#include "types.h"
#include "memory.h"
#include "copying.h"

#define TRIDASH_ARRAY_OBJ_SIZE offsetof(struct tridash_object, array) + sizeof(struct array)

void *copy_array(const void *src) {
    const struct tridash_object *object = src;
    size_t size = TRIDASH_ARRAY_OBJ_SIZE + object->array.size * sizeof(uintptr_t);

    save_ptr(object);
    void *dest = alloc(size);
    memcopy(dest, restore_ptr(), size);

    return dest;
}

void *gc_copy_array(const void *src) {
    const struct tridash_object *object = src;
    size_t size = TRIDASH_ARRAY_OBJ_SIZE + object->array.size * sizeof(uintptr_t);

    void *dest = alloc(size);
    memcopy(dest, src, size);

    return dest;
}

void *gc_copy_array_elements(void *src) {
    struct tridash_object *object = src;

    size_t size = object->array.size;

    for (size_t i = 0; i < size; ++i) {
        object->array.elements[i] = (uintptr_t)gc_copy_object((void*)object->array.elements[i]);
    }

    return &object->array.elements[size];
}

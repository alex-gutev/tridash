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

#include "funcrefs.h"

#include "memory.h"
#include "copying.h"

#define FUNCREF_SIZE offsetof(struct tridash_object, funcref.fn) + sizeof(struct funcref)

void *gc_copy_funcref(const void *src) {
    const struct tridash_object *object = src;
    size_t size = FUNCREF_SIZE + object->funcref.num_args * sizeof(uintptr_t);

    void *dest = alloc(size);
    memcopy(dest, src, size);

    return dest;
};

void *gc_copy_funcref_args(void *src) {
    struct tridash_object *object = src;
    size_t size = object->funcref.num_args;

    for (size_t i = 0; i < size; ++i) {
        object->funcref.args[i] = (uintptr_t)gc_copy_object((void*)object->funcref.args[i]);
    }

    return &object->funcref.args[size];
};

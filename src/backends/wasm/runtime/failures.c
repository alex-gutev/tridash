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

#include "failures.h"

#include "types.h"
#include "memory.h"
#include "copying.h"

#define TRIDASH_FAIL_SIZE (offsetof(struct tridash_object, fail_type) + sizeof(uintptr_t))

uintptr_t make_failure(uintptr_t type) {
    struct tridash_object *object = alloc(TRIDASH_FAIL_SIZE);

    object->type = TRIDASH_TYPE_FAILURE;
    object->fail_type = type;

    uintptr_t ptr = (uintptr_t)((void*)object);
    return ptr | TAG_TYPE_FAIL;
}

void *copy_failure(const void *src) {
    void *dest = alloc(TRIDASH_FAIL_SIZE);
    memcopy(dest, src, TRIDASH_FAIL_SIZE);

    return dest;
}

void *copy_failure_type(void *src) {
    struct tridash_object *object = src;

    object->fail_type = (uintptr_t)copy_object((void*)object->fail_type);

    return &object->fail_type + 1;
}


/// Builtin Failure Types

uintptr_t fail_type_error(void) {
    /* TODO: Create constant failure type node object */

    // For now function reference index 2 will be used as the failure
    // type

    return make_failure(TAG_VALUE(2, TAG_TYPE_FUNCREF));
}

uintptr_t fail_type_no_value(void) {
    /* TODO: Create constant failure type node object */

    // For now function reference index 3 will be used as the failure
    // type

    return make_failure(TAG_VALUE(3, TAG_TYPE_FUNCREF));
}

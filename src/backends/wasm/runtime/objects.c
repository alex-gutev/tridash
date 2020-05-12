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

#include "objects.h"

#include "macros.h"
#include "types.h"
#include "memory.h"
#include "copying.h"
#include "thunk.h"
#include "failures.h"
#include "memory.h"


#define USER_OBJECT_SIZE offsetof(struct tridash_object, object) + sizeof(struct user_object)

/// Copying

void *gc_copy_user_object(const void *src) {
    const struct tridash_object *object = src;
    size_t size = USER_OBJECT_SIZE + object->object.descriptor->num_fields * sizeof(uintptr_t);

    void *dest = alloc(size);
    memcopy(dest, src, size);

    return dest;
}

void *gc_copy_user_object_subnodes(void *src) {
    struct tridash_object *object = src;
    size_t size = object->object.descriptor->num_fields;

    for (size_t i = 0; i < size; ++i) {
        object->object.fields[i] = (uintptr_t)gc_copy_object((void *)object->object.fields[i]);
    }

    return &object->object.fields[size];
}


/// Public Interface

uintptr_t object_member(uintptr_t object, uintptr_t field) {
    SAVED_TPTR(field, object = resolve(object));

    switch (PTR_TAG(object)) {
    case TAG_TYPE_INT:
    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();

    case TAG_TYPE_FAIL:
        return object;
    }

    SAVED_TPTR(object, field = resolve(field));

    switch (PTR_TAG(field)) {
    case TAG_TYPE_INT:
    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();

    case TAG_TYPE_FAIL:
        return object;
    }

    struct tridash_object *obj = (void*)object;
    struct tridash_object *key = (void*)field;

    if (obj->type != TRIDASH_TYPE_OBJECT &&
        (key->type != TRIDASH_TYPE_SYMBOL &&
         key->type != TRIDASH_TYPE_STRING)) {
        return make_fail_type_error();
    }

    struct hash_bucket *bucket = hash_table_lookup(&obj->object.descriptor->fields, &key->string);

    return bucket ? obj->object.fields[bucket->value] : make_fail_no_value();
}

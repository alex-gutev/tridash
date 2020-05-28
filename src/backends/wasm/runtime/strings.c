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

#include "strings.h"
#include "types.h"
#include "memory.h"
#include "failures.h"

#define CHAR_OBJECT_SIZE (offsetof(struct tridash_object, char_code) + sizeof(uint32_t))
#define STRING_OBJECT_SIZE (offsetof(struct tridash_object, string.data))

void *gc_copy_string(const void *ptr) {
    const struct tridash_object *object = ptr;
    size_t size = offsetof(struct tridash_object, string.data) + object->string.size;

    void *dest = alloc(size);
    memcopy(dest, ptr, size);

    return dest;
}

void *copy_string(const void *ptr) {
    const struct tridash_object *object = ptr;
    size_t size = STRING_OBJECT_SIZE + object->string.size;

    save_ptr(ptr);

    void *dest = alloc(size);
    memcopy(dest, restore_ptr(), size);

    return dest;
}


void *alloc_string(size_t size) {
    struct tridash_object *str = alloc(STRING_OBJECT_SIZE + size);

    str->type = TRIDASH_TYPE_STRING;
    str->string.size = size;

    return str;
}

void *string_end_ptr(struct string *str) {
    void *ptr = &str->data[str->size];
    ptr += ~(uintptr_t)ptr + 1 & TAG_MASK;

    return ptr;
}


/// Core Module String Functions

uintptr_t string_at(uintptr_t ptr, uintptr_t index) {
    SAVED_TPTR(index, ptr = resolve(ptr));

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_INT:
    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();

    case TAG_TYPE_FAIL:
        return ptr;
    }

    struct tridash_object *obj = (void *)ptr;

    if (obj->type != TRIDASH_TYPE_STRING) {
        return make_fail_type_error();
    }

    SAVED_PTR(obj, index = resolve(index));

    int32_t ichr = 0;

    switch (PTR_TAG(index)) {
    case TAG_TYPE_INT:
        ichr = INT_VALUE(index);
        break;

    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();

    case TAG_TYPE_FAIL:
        return index;

    default: {
        struct tridash_object *iobj = (void *)index;

        if (iobj->type != TRIDASH_TYPE_INT)
            return make_fail_type_error();

        ichr = iobj->integer;
    } break;
    }

    if (ichr < 0) {
        return make_fail_index_out_bounds();
    }

    unsigned char *chr = (unsigned char *)obj->string.data;
    unsigned char *end = chr + obj->string.size;

    uint32_t code = 0;

    while (ichr && ((uintptr_t)chr < (uintptr_t)end)) {
        if ((*chr & 0xF8) == 0xF0)
            chr += 4;
        else if ((*chr & 0xF0) == 0xE0)
            chr += 3;
        else if ((*chr & 0xE0) == 0xC0)
            chr += 2;
        else
            chr += 1;

        ichr--;
    }

    if (ichr || ((uintptr_t)chr >= (uintptr_t)end)) {
        return make_fail_index_out_bounds();
    }

    if ((*chr & 0xF8) == 0xF0) {
        code = (*chr & 0x07) << 18;
        code |= (chr[1] & 0x3F) << 12;
        code |= (chr[2] & 0x3F) << 6;
        code |= (chr[3] & 0x3F);
    }
    else if ((*chr & 0xF0) == 0xE0) {
        code = (chr[0] & 0x0F) << 12;
        code |= (chr[1] & 0x3F) << 6;
        code |= (chr[2] & 0x3F);
    }
    else if ((*chr & 0xE0) == 0xC0) {
        code = (chr[0] & 0x1F) << 6;
        code |= (chr[1] & 0x3F);
    }
    else {
        code = *chr;
    }

    struct tridash_object *chr_obj = alloc(CHAR_OBJECT_SIZE);
    chr_obj->type = TRIDASH_TYPE_CHAR;
    chr_obj->char_code = code;

    return (uintptr_t)chr_obj;
}

uintptr_t string_concat(uintptr_t ptr1, uintptr_t ptr2) {
    // Resolve and check type of Object 1

    SAVED_TPTR(ptr2, ptr1 = resolve(ptr1));

    switch (PTR_TAG(ptr1)) {
    case TAG_TYPE_INT:
    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();

    case TAG_TYPE_FAIL:
        return ptr1;
    }

    struct tridash_object *obj1 = (void*)ptr1;

    if (obj1->type != TRIDASH_TYPE_STRING)
        return make_fail_type_error();


    // Resolve and check type of Object 2

    SAVED_PTR(obj1, ptr2 = resolve(ptr2));

    switch (PTR_TAG(ptr2)) {
    case TAG_TYPE_INT:
    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();

    case TAG_TYPE_FAIL:
        return ptr2;
    }

    struct tridash_object *obj2 = (void*)ptr2;

    if (obj2->type != TRIDASH_TYPE_STRING)
        return make_fail_type_error();


    // Allocate string buffer

    save_ptr(obj1);
    save_ptr(obj2);

    struct tridash_object *str = alloc_string(obj1->string.size + obj2->string.size);


    // Copy string data

    obj2 = restore_ptr();
    obj1 = restore_ptr();

    memcopy(str->string.data, obj1->string.data, obj1->string.size);
    memcopy(str->string.data + obj1->string.size, obj2->string.data, obj2->string.size);

    return (uintptr_t)str;
}

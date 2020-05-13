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

#include "conversions.h"

#include "types.h"
#include "thunk.h"
#include "failures.h"
#include "strings.h"
#include "memory.h"

#define INT_OBJECT_SIZE (offsetof(struct tridash_object, integer) + sizeof(int32_t))

/**
 * Create a boxed integer object.
 *
 * If the integer value can fit in a tagged pointer, a tagged pointer
 * storing the immediate value is returned. Otherwise an integer
 * object is created on the heap.
 *
 * @param value The signed integer value.
 *
 * @return The integer object.
 */
static uintptr_t make_boxed_int(int32_t value);

/**
 * Parse an integer from a string.
 *
 * If the string does not contain a valid integer, a failure of type
 * `Invalid-Integer` is returned.
 *
 * @param str The string.
 *
 * @return The integer object, parsed from the string.
 */
static uintptr_t string_to_int(const struct string *str);


/// Object To Integer

uintptr_t object_to_int(uintptr_t ptr) {
    ptr = resolve(ptr);

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_INT:
    case TAG_TYPE_FAIL:
        return ptr;

    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();
    }

    const struct tridash_object *obj = (void *)ptr;

    switch (obj->type) {
    case TRIDASH_TYPE_INT:
        return ptr;

    case TRIDASH_TYPE_FLOAT:
        return make_boxed_int((int32_t)obj->real);

    case TRIDASH_TYPE_STRING:
        return string_to_int(&obj->string);

    default:
        return make_fail_type_error();
    }
}

uintptr_t make_boxed_int(int32_t value) {
    if (MIN_IMMEDIATE_INT < value && value < MAX_IMMEDIATE_INT) {
        return TAG_INT((uintptr_t)value);
    }

    struct tridash_object *obj = alloc(INT_OBJECT_SIZE);

    obj->type = TRIDASH_TYPE_INT;
    obj->integer = value;

    return (uintptr_t)obj;
}


/// Converting Strings to Integers

uintptr_t string_to_int(const struct string *str) {
    const char *chr = str->data;
    const char *end = &str->data[str->size];

    if (!str->size) return make_fail_invalid_integer();

    int32_t value = 0;
    int sign = 1;

    if (*chr == '-') {
        sign = -1;
        chr++;
    }

    while ((uintptr_t)chr < (uintptr_t)end) {
        if ('0' <= *chr && *chr <= '9')
            value = value * 10 + (*chr - '0');
        else
            return make_fail_invalid_integer();

        chr++;
    }

    return make_boxed_int(value * sign);
}


/// Integer to String

/**
 * Count the number of decimal digits required to represent an
 * integer.
 *
 * @param value The integer value. Should be positive.
 *
 * @return The number of digits.
 */
static int count_digits(int32_t value);

/**
 * Fill the a given string buffer with the string representation of a
 * given integer value.
 *
 * @param str Pointer to the last byte of a string buffer, which
 *   should be large enough to store all the digits required to
 *   represent @a value.
 *
 * @param value The integer value.
 */
static void int_string_digits(char *str, int32_t value);


uintptr_t int_to_string(uintptr_t ptr) {
    ptr = resolve(ptr);
    int32_t value;

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_INT:
        value = INT_VALUE(ptr);
        break;

    case TAG_TYPE_FAIL:
        return ptr;

    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();

    case TAG_TYPE_PTR: {
        struct tridash_object *obj = (void*)ptr;

        if (obj->type != TRIDASH_TYPE_INT)
            return make_fail_type_error();

        value = obj->integer;
    } break;
    }

    int sign = value < 0 ? 1 : 0;
    if (sign) value *= -1;

    int digits = count_digits(value);

    struct tridash_object *str = alloc_string(digits + sign);

    int_string_digits(str->string.data + digits + sign - 1, value);

    if (sign)
        str->string.data[0] = '-';

    return (uintptr_t)str;
}

int count_digits(int32_t value) {
    if (value < 0)
        value *= -1;

    int digits = 0;

    do {
        digits++;
        value /= 10;
    } while (value);

    return digits;
}

void int_string_digits(char *buf, int32_t value) {
    do {
        *buf = '0' + (value % 10);
        value /= 10;

        buf--;
    } while (value);
}


/// Real To String

/**
 * Convert a floating point value to an exponent notation string
 * representation.
 *
 * @param value The value.
 *
 * @return The exponent string representation.
 */
static uintptr_t real_to_exp_string(float value);
/**
 * Convert a floating point value to an decimal string representation.
 *
 * @param value The value.
 *
 * @return The decimal string representation.
 */
static uintptr_t real_to_dec_string(float value);

uintptr_t real_to_string(uintptr_t ptr) {
    ptr = resolve(ptr);

    if (PTR_TAG(ptr) == TAG_TYPE_FAIL)
        return ptr;

    if (PTR_TAG(ptr) != TAG_TYPE_PTR)
        return make_fail_type_error();

    struct tridash_object *obj = (void *)ptr;

    if (obj->type != TRIDASH_TYPE_FLOAT)
        return make_fail_type_error();

    float value = obj->real;

    if (value > 1e9) {
        return real_to_exp_string(value);
    }

    //* TODO: Check for Infinities and NaN */

    return real_to_dec_string(value);
}

static uintptr_t real_to_exp_string(float value) {
    int sign = value < 0 ? 1 : 0;

    if (sign) value = -value;

    int exponent = 0;

    if (value > 1e32) {
        value /= 1e32;
        exponent += 32;
    }
    if (value > 1e16) {
        value /= 1e16;
        exponent += 16;
    }
    if (value > 1e8) {
        value /= 1e8;
        exponent += 8;
    }
    if (value > 1e4) {
        value /= 1e4;
        exponent += 4;
    }
    if (value > 1e2) {
        value /= 1e2;
        exponent += 2;
    }
    if (value > 1e1) {
        value /= 1e1;
        exponent += 1;
    }

    int32_t int_part = (int32_t)value;
    int32_t frac_part = (int32_t)((value - int_part) * 1e6);

    int int_digits = count_digits(int_part);
    int frac_digits = count_digits(frac_part);
    int exp_digits = count_digits(exponent);

    int str_size = sign + int_digits + frac_digits + exp_digits + 2;

    struct tridash_object *str = alloc_string(str_size);
    char * buf = str->string.data + str_size - 1;

    // Convert exponent to string

    int_string_digits(buf, exponent);

    buf -= exp_digits;
    *buf = 'e';

    // Convert fractional part to string

    int_string_digits(--buf, frac_part);

    buf -= frac_part;
    *buf = '.';

    // Convert integral part to string

    int_string_digits(--buf, int_part);

    // Add sign if necessary

    if (sign) {
        str->string.data[0] = '-';
    }

    return (uintptr_t)str;
}

static uintptr_t real_to_dec_string(float value) {
    int sign = value < 0 ? 1 : 0;

    if (sign) value = -value;

    int32_t int_part = (int32_t)value;
    int32_t frac_part = (int32_t)((value - int_part) * 1e6);

    int int_digits = count_digits(int_part);
    int frac_digits = count_digits(frac_part);
    int str_size = sign + int_digits + frac_digits + 1;

    struct tridash_object *str = alloc_string(str_size);
    char * buf = str->string.data + str_size - 1;

    // Convert fractional part to string

    int_string_digits(buf, frac_part);

    buf -= frac_digits;
    *buf = '.';

    // Convert integral part to string

    int_string_digits(--buf, int_part);

    // Add sign if necessary

    if (sign) {
        str->string.data[0] = '-';
    }

    return (uintptr_t)str;
}


/// Character to String

uintptr_t char_to_string(uintptr_t ptr) {
    ptr = resolve(ptr);

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_FAIL:
        return ptr;
        break;

    case TAG_TYPE_INT:
    case TAG_TYPE_FUNCREF:
        return make_fail_type_error();
    }

    struct tridash_object *obj = (void *)ptr;

    if (obj->type != TRIDASH_TYPE_CHAR)
        return make_fail_type_error();

    uint32_t code = obj->char_code;
    size_t bytes = 0;

    if (code <= 0x7F) bytes = 1;
    else if (code <= 0x7FF) bytes = 2;
    else if (code <= 0xFFFF) bytes = 3;
    else bytes = 4;

    struct tridash_object *str = alloc_string(bytes);

    switch (bytes) {
    case 1:
        str->string.data[0] = code;
        break;

    case 2:
        str->string.data[0] = 0xDF & (code >> 8);
        str->string.data[1] = 0xBF & code;

    case 3:
        str->string.data[0] = 0xEF & (code >> 16);
        str->string.data[1] = 0xBF & (code >> 8);
        str->string.data[2] = 0xBF & code;

    default:
        str->string.data[0] = 0xF7 & (code >> 24);
        str->string.data[1] = 0xBF & (code >> 16);
        str->string.data[2] = 0xBF & (code >> 8);
        str->string.data[3] = 0xBF & code;
    }

    return (uintptr_t)str;
}


/// Symbol to String

uintptr_t symbol_name(uintptr_t ptr) {
    ptr = resolve(ptr);

    if (PTR_TAG(ptr) == TAG_TYPE_FAIL)
        return ptr;

    if (PTR_TAG(ptr) != TAG_TYPE_PTR)
        return make_fail_type_error();

    struct tridash_object *obj = (void *)ptr;

    if (obj->type != TRIDASH_TYPE_SYMBOL)
        return make_fail_type_error();

    struct tridash_object *copy = copy_string(obj);
    copy->type = TRIDASH_TYPE_STRING;

    return (uintptr_t)copy;
}


/// Type Checks

uintptr_t is_int(uintptr_t ptr) {
    ptr = resolve(ptr);

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_INT:
        return TRIDASH_TRUE;

    case TAG_TYPE_FAIL:
        return ptr;

    case TAG_TYPE_PTR: {
        struct tridash_object *obj = (void *)ptr;
        return TRIDASH_BOOL(obj->type == TRIDASH_TYPE_INT);
    }
    }

    return TRIDASH_FALSE;
}

uintptr_t is_real(uintptr_t ptr) {
    ptr = resolve(ptr);

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_FAIL:
        return ptr;

    case TAG_TYPE_PTR: {
        struct tridash_object *obj = (void *)ptr;
        return TRIDASH_BOOL(obj->type == TRIDASH_TYPE_FLOAT);
    }
    }

    return TRIDASH_FALSE;
}

uintptr_t is_char(uintptr_t ptr) {
    ptr = resolve(ptr);

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_FAIL:
        return ptr;

    case TAG_TYPE_PTR: {
        struct tridash_object *obj = (void *)ptr;
        return TRIDASH_BOOL(obj->type == TRIDASH_TYPE_CHAR);
    }
    }

    return TRIDASH_FALSE;
}

uintptr_t is_string(uintptr_t ptr) {
    ptr = resolve(ptr);

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_FAIL:
        return ptr;

    case TAG_TYPE_PTR: {
        struct tridash_object *obj = (void *)ptr;
        return TRIDASH_BOOL(obj->type == TRIDASH_TYPE_STRING);
    }
    }

    return TRIDASH_FALSE;
}

uintptr_t is_symbol(uintptr_t ptr) {
    ptr = resolve(ptr);

    switch (PTR_TAG(ptr)) {
    case TAG_TYPE_FAIL:
        return ptr;

    case TAG_TYPE_PTR: {
        struct tridash_object *obj = (void *)ptr;
        return TRIDASH_BOOL(obj->type == TRIDASH_TYPE_SYMBOL);
    }
    }

    return TRIDASH_FALSE;
}

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

#include <stdint.h>
#include <math.h>

#include "math.h"
#include "types.h"

float frem(float a, float b) {
    int32_t q = a/b;

    return a - (b * q);
}


export uintptr_t is_inf(uintptr_t x) {
    if (IS_FAIL(x)) return x;
    if (!IS_REF(x)) return TRIDASH_FALSE;

    struct tridash_object *obj = (void *)x;

    if (obj->type == TRIDASH_TYPE_FLOAT) {
        return isinf(obj->real);
    }

    return TRIDASH_FALSE;
}

export uintptr_t is_nan(uintptr_t x) {
    switch (PTR_TAG(x)) {
    case TAG_TYPE_INT:
        return TRIDASH_FALSE;

    case TAG_TYPE_FUNCREF:
        return TRIDASH_TRUE;

    case TAG_TYPE_FAIL:
        return x;
    }

    struct tridash_object *obj = (void *)x;

    switch (obj->type) {
    case TRIDASH_TYPE_INT:
        return TRIDASH_FALSE;

    case TRIDASH_TYPE_FLOAT:
        return isnan(obj->real);
    }

    return TRIDASH_TRUE;
}

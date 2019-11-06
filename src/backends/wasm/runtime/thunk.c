/**
 * Tridash Wasm32 Runtime Library
 * Copyright (C) 2019  Alexander Gutev
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

#include "thunk.h"

/**
 * Thunk function pointer type.
 *
 * Takes a pointer to the start of the thunk's closure (the first word
 * after the thunk function)
 */
typedef uintptr_t(*thunk_fn)(uintptr_t *);

/**
 * Calls a thunk function.
 *
 * Pointer to the first word of the thunk.
 *
 * @return The value returned by the thunk function.
 */
static uintptr_t resolve_thunk(uintptr_t *thunk);


uintptr_t resolve(uintptr_t value) {
    while (IS_REF(value)) {
        uintptr_t *box = (void*)(value);

        switch (box[0]) {
        case TRIDASH_TYPE_RESOLVED_THUNK:
            value = box[1];
            break;

        case TRIDASH_TYPE_THUNK: {
            uintptr_t res = resolve_thunk(box);

            box[0] = TRIDASH_TYPE_RESOLVED_THUNK;
            box[1] = res;

            value = res;
        } break;

        default:
            return value;
        }
    }

    return value;
}

uintptr_t resolve_thunk(uintptr_t *thunk) {
    thunk_fn fn;
    uintptr_t result;

    // The pointer to the thunk function is obtained by the following
    // inline asm as casting a void* pointer to a function pointer is
    // "undefined behavior".

    asm("local.get %1;"
        "i32.load 4;"
        "local.set %0;"

        : "=r" (fn)
        : "r" (thunk));

    return fn(thunk+2);
}

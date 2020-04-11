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

#ifndef TRIDASH_THUNK_H
#define TRIDASH_THUNK_H

#include <stdint.h>
#include <stddef.h>

#include "macros.h"

/**
 * Thunk function pointer type.
 *
 * Takes a pointer to the start of the thunk's closure (the first word
 * after the thunk function).
 */
typedef uintptr_t(*thunk_fn)(void *);
/**
 * Thunk structure type
 */
struct thunk {
    /**
     * Thunk function.
     */
    thunk_fn fn;

    /**
     * Number of elements in closure.
     */
    size_t closure_size;

    /**
     * Closure array.
     */
    uintptr_t closure[];
};

/**
 * Thunk with an alternative value which should be returned when the
 * main value is a failure value.
 */
struct catch_thunk {
    uintptr_t value;
    uintptr_t fail_value;
};

/**
 * Returns the value of a thunk, computing it if it has not been
 * computed already.
 *
 * @param val Value to resolve. If this is not a reference or does not
 *    point to a thunk, it is returned directly.
 *
 * @return The resolved thunk value.
 */
export uintptr_t resolve(uintptr_t val);


/// Copying

/**
 * Copy a thunk along with its closure.
 *
 * Only the references to the objects in the closure are copied, not
 * the objects themselves.
 *
 * @param src Pointer to the thunk.
 * @return Pointer to the copied thunk.
 */
void *copy_thunk(const void *src);

/**
 * Copy the result computed by a thunk.
 *
 * The thunk's value must have previously been computed.  The thunk
 * object and its closure are not copied.
 *
 * @param src Pointer to a resolved thunk.
 */
void *copy_thunk_result(void *src);

/**
 * Copies the objects referenced in the thunk's closure, and updates
 * the references.
 *
 * @param object Pointer to the thunk.
 *
 * @return The pointer to the first byte following the thunk object.
 */
void *copy_thunk_closure(void *object);


/// Catch Thunks

/**
 * Create a catch thunk.
 *
 * @param value The main value.
 *
 * @param fail_value The value to be returned when @a value evaluates
 *   to a failure.
 *
 * @param test Failure type test function (currently unused).
 *
 * @return The catch thunk object.
 */
export void *make_catch_thunk(uintptr_t value, uintptr_t fail_value, uintptr_t test);

/**
 * Copy a catch thunk object.
 *
 * Note: Does not copy the main or failure value objects.
 *
 * @param src The catch thunk object.
 *
 * @return A copy of the object.
 */
void *copy_catch_thunk(const void *src);

/**
 * Copy the main and failure value objects of a catch thunk.
 *
 * @param object The catch thunk object.
 *
 * @return Pointer to the first byte following the catch thunk.
 */
void *copy_catch_thunk_objects(void *object);

#endif /* TRIDASH_THUNK_H */

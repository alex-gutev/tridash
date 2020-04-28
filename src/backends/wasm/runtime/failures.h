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

#ifndef TRIDASH_FAILURES_H
#define TRIDASH_FAILURES_H

#include <stdint.h>

#include "macros.h"

/// Creation

/**
 * Create a failure value.
 *
 * @param type Failure value type
 *
 * @return Tagged pointer to the failure value.
 */
export uintptr_t make_failure(uintptr_t type);

/**
 * Return the value identifying the type of a failure value.
 *
 * @param object The object.
 *
 * @return The failure type of @a object. If @a object is not a
 *   failure value, returns a Type-Error failure.
 */
export uintptr_t failure_type(uintptr_t object);


/// Copying

void *copy_failure(const void *src);

void *copy_failure_type(void *src);


/// Builtin Failure Types

/* Type-Error */
export uintptr_t fail_type_error(void);
export uintptr_t make_fail_type_error(void);

/* No-Value */
export uintptr_t fail_no_value(void);
export uintptr_t make_fail_no_value(void);

/* Arity Error */
export uintptr_t fail_arity_error(void);
export uintptr_t make_fail_arity_error(void);

/* Invalid Integer */
export uintptr_t fail_invalid_integer(void);
uintptr_t make_fail_invalid_integer(void);

/* Invalid Real */
export uintptr_t fail_invalid_real(void);
uintptr_t make_fail_invalid_real(void);

/* Index Out of Bounds */
export uintptr_t fail_index_out_bounds(void);
uintptr_t make_fail_index_out_bounds(void);

#endif /* TRIDASH_FAILURES_H */

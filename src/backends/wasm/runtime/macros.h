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

#ifndef TRIDASH_MACROS_H
#define TRIDASH_MACROS_H

/* Utility Macros */

/// Macros

#define export __attribute__( ( visibility( "default" ) ) )


/// Tagging

#define WORD_SIZE sizeof(uintptr_t)

/**
 * Bit mask of the tag bits in a Tridash pointer.
 */
#define TAG_MASK (WORD_SIZE-1)

/**
 * Clears the non-tag bits of x.
 */
#define PTR_TAG(x) ((uintptr_t)(x) & TAG_MASK)


//// Tag Types

/**
 * The following tag constants identify the type of value.
 */

/**
 * Pointer to a boxed value stored on the heap.
 */
#define TAG_TYPE_PTR 0
/**
 * Immediate integer value stored in the non-tag portion.
 */
#define TAG_TYPE_INT 0x1

/**
 * Function pointer stored in the non-tag portion.
 */
#define TAG_TYPE_FUNCREF 0x2

/**
 * Failure value.
 */
#define TAG_TYPE_FAIL 0x3


//// Creating tagged pointers

/**
 * Create a tagged pointer, storing the immediate 'value', with tag
 * type 'type'.
 */
#define TAG_VALUE(value, type) (((value) << 2) | (type))

/**
 * Create a tagged pointer, storing the immediate integer 'value'.
 */
#define TAG_INT(value) TAG_VALUE(value, TAG_TYPE_INT)


//// Querying tagged value type

/**
 * The following macros test the tag type of a tagged pointer.
 */

/**
 * Test that 'x' is a reference to a boxed value on the heap.
 */
#define IS_REF(x) (PTR_TAG(x) == 0)

/**
 * Test that 'x' is an immediate integer value.
 */
#define IS_INT(x) (PTR_TAG(x) == TAG_TYPE_INT)

/**
 * Test that 'x' is a failure value.
 */
#define IS_FAIL(x) (PTR_TAG(x) == TAG_TYPE_FAIL)


//// Decoding Immediate Values

/**
 * Extract immediate integer value stored in tagged pointer.
 */
#define INT_VALUE(x) (((int32_t)(x)) >> 2)

/**
 * Maximum immediate signed integer value that can be stored in a
 * tagged pointer.
 */
#define MAX_IMMEDIATE_INT ((int32_t)(INT32_MAX) >> 1)
/**
 * Minimum immediate signed integer value that can be stored in a
 * tagged pointer.
 */
#define MIN_IMMEDIATE_INT ((int32_t)(INT32_MIN) >> 1)


#endif /* TRIDASH_MACROS_H */

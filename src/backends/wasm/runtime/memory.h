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

#ifndef TRIDASH_MEMORY_H
#define TRIDASH_MEMORY_H

#include <stdint.h>
#include <stdlib.h>

#include "types.h"

/**
 * Pointer to the top of the stack.
 */
extern char ** stack_top;


/**
 * Initialize the garbage collector.
 *
 * @param stack Pointer to the stack base.
 *
 * @param heap Pointer to the start of the heap which is managed by
 *   the garbage collector.
 *
 * @param size Size of the heap. It is assumed that the memory can
 *   grow beyond this size.
 */
export void initialize(char *stack, char *heap, size_t size);

/**
 * Allocate a block of memory.
 *
 * @param size Size of the block in bytes.
 * @return Pointer to the first byte of the block.
 */
export void * alloc(size_t size);

/**
 * Run the garbage collector.
 */
export void run_gc(void);


/**
 * Copies a block of memory from one region to another.
 *
 * @param dest The destination region to which the source region is
 *   copied.
 *
 * @param src The source region.
 *
 * @param size Number of bytes to copy.
 */
export void memcopy(char *dest, const char *src, size_t size);


/**
 * Checks whether a pointer points to a memory location within the
 * garbage collected heap.
 *
 * @param ptr The pointer
 *
 * @return True (1) if the pointer points within the garbage collected
 *   heap.
 */
int is_managed(const void *ptr);

/**
 * Set all bytes in a block of memory to zero.
 *
 * @param ptr Pointer to the block.
 * @param size Size of the block.
 */
export void memclear(char *ptr, size_t size);


/** Manipulating Root Set */

/**
 * Save a pointer onto the GC root set stack.
 *
 * @param ptr The pointer (may be a tagged immediate value).
 */
void save_ptr(const void *ptr);

/**
 * Pop a pointer off the GC root set stack.
 */
void *restore_ptr(void);

/**
 * Generate statements which save @a ptr onto the GC root set stack,
 * followed by the statement BLOCK, followed by a statement which sets
 * @a ptr to the value popped off the GC root set stack.
 */
#define SAVED_PTR(ptr, block) save_ptr(ptr); block; ptr = restore_ptr();

/**
 * Same as SAVED_PTR except intended to be used with tagged pointers.
 */
#define SAVED_TPTR(ptr, block) save_ptr((void*)ptr); block; ptr = (uintptr_t)restore_ptr();

#endif /* TRIDASH_MEMORY_H */

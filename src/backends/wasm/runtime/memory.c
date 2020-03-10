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

#include "memory.h"

#include "copying.h"

#define PAGE_SIZE (64 * 1024);

/**
 * Pointer to the base of the stack i.e. the bottom element.
 */
static char ** stack_base;

/**
 * Pointer to the first byte of the heap memory, which is managed by
 * the garbage collector.
 *
 * Objects located below this address are not collected.
 */
static char * heap_base;

/**
 * Pointer to the heap to which objects are copied after garbage
 * collection.
 */
static char * to_space;
/**
 * Pointer to the heap in which objects are currently allocated.
 */
static char * from_space;

/**
 * Size of the available heap space, including both the "from space"
 * and "to space".
 */
static size_t heap_size;

/**
 * Pointer to the location at which new objects are allocated.
 */
static char * alloc_ptr;
/**
 * Pointer to the next object to scan during a garbage collection.
 */
static char * scan_ptr;


//// Copying Referenced Objects

/**
 * Copies all objects in the "from space" which are referenced by an
 * object in the "to space".
 */
static void copy_live(void);


//// Utilities

/**
 * Attempts to extend the available heap space.
 *
 * @param amount The minimum number of bytes by which to extend the
 *   heap.
 */
static void grow_memory(size_t amount);


//// Initialization

void initialize(char *stack, char *heap, size_t size) {
    stack_base = (char**)stack;
    stack_top = stack_base;

    heap_base = heap;

    from_space = alloc_ptr = heap;
    to_space = heap + size/2;

    heap_size = size;
}


//// Allocation and Collection

void * alloc(size_t size) {
    // Align to word size
    size += ~size + 1 & TAG_MASK;

    if ((uintptr_t)(alloc_ptr + size) >= (uintptr_t)(from_space + heap_size/2))
        run_gc();

    if ((uintptr_t)(alloc_ptr + size) >= (uintptr_t)(from_space + heap_size/2))
        grow_memory(heap_size);

    char *ptr = alloc_ptr;
    alloc_ptr += size;

    return ptr;
}

void run_gc(void) {
    alloc_ptr = to_space;

    to_space = from_space;
    from_space = alloc_ptr;

    // Allign alloc_ptr to word boundary
    alloc_ptr += ~(uintptr_t)alloc_ptr + 1 & TAG_MASK;
    scan_ptr = alloc_ptr;

    char ** stack = (char**)stack_base;

    while ((uintptr_t)stack > (uintptr_t)stack_top) {
        *stack = copy_object(*stack);
        stack--;
    }

    copy_live();
}


//// Copying Objects



//// Copying Referenced Objects

void copy_live(void) {
    while ((uintptr_t)scan_ptr < (uintptr_t)alloc_ptr) {
        scan_ptr = copy_referenced_objects(scan_ptr);
    }
}


//// Utilities

void grow_memory(size_t amount) {
    size_t pages = amount / PAGE_SIZE;

    __builtin_wasm_memory_grow(0, pages);

    heap_size += amount;
}

void memcopy(char *dest, const char *src, size_t size) {
    while (size--) {
        *dest = *src;

        dest++;
        src++;
    }
}

int is_managed(const void *ptr) {
    return (uintptr_t)ptr >= (uintptr_t)heap_base;
}

void memclear(char *ptr, size_t size) {
    while (size--) {
        *ptr = 0;
    }
}

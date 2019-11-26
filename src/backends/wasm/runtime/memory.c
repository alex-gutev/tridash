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

#include "memory.h"

#define PAGE_SIZE (64 * 1024);

/**
 * Pointer to the base of the stack i.e. the bottom element.
 */
static char ** stack_base;

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


//// Object Copying Functions

/**
 * Copy a Tridash object from the "from space" to the "to space".
 *
 * @param object Pointer to the object.
 * @return Pointer of the copied object.
 */
static char *copy_object(char *object);

/**
 * Copy a boxed number.
 *
 * @param src Pointer to the number.
 * @return Pointer to the copied boxed number.
 */
static char *copy_number(char *src);

/**
 * Copy a thunk along with its closure.
 *
 * Only the references to the objects in the closure are copied, not
 * the closure itself.
 *
 * @param src Pointer to the thunk.
 * @return Pointer to the copied thunk.
 */
static char *copy_thunk(char *src);

/**
 * Copy the result computed by a thunk.
 *
 * The thunk's value must have previously been computed.  The thunk
 * object and its closure are not copied.
 *
 * @param src Pointer to a resolved thunk.
 */
static char *copy_thunk_result(char *src);


//// Copying Referenced Objects

/**
 * Copies all objects in the "from space" which are referenced by an
 * object in the "to space".
 */
static void copy_live(void);

/**
 * Copies the objects referenced in the thunk's closure, and updates
 * the references.
 *
 * @param object Pointer to the thunk.
 *
 * @return The pointer to the first byte following the thunk object.
 */
static char *copy_thunk_closure(char *object);


//// Utilities

/**
 * Attempts to extend the available heap space.
 *
 * @param amount The minimum number of bytes by which to extend the
 *   heap.
 */
static void grow_memory(size_t amount);

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
static void memcopy(char *src, char *dest, size_t size);


//// Initialization

void initialize(char *stack, char *heap, size_t size) {
    stack_base = (char**)stack;
    stack_top = stack_base;

    from_space = alloc_ptr = heap;
    to_space = heap + size/2;

    heap_size = size;
}


//// Allocation and Collection

char * alloc(size_t size) {
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

char *copy_object(char *object) {
    if (!IS_REF(object))
        return object;

    char *dest;

    switch(object[0]) {
    case TRIDASH_TYPE_THUNK:
        dest = copy_thunk(object);
        break;

    case TRIDASH_TYPE_RESOLVED_THUNK:
        dest = copy_thunk_result(object);
        break;

    case TRIDASH_TYPE_INT:
    case TRIDASH_TYPE_FLOAT:
        dest = copy_number(object);
        break;

    case TRIDASH_TYPE_FORWARD:
        return ((char**)(object))[1];
    }

    object[0] = TRIDASH_TYPE_FORWARD;
    ((uintptr_t*)(object))[1] = (uintptr_t)dest;

    return dest;
}

char *copy_number(char *src) {
    char *dest = alloc(2 * WORD_SIZE);

    // Consider whether this really needs to be implemented in inline
    // ASM

    asm("local.get %1;"
        "local.get %0;"
        "i64.load 0;"
        "i64.store 0;"

        : : "r" (src), "r" (dest));

    return dest;
}

char *copy_thunk(char *src) {
    uintptr_t *thunk = (uintptr_t*)((void *)src);
    uintptr_t size = (thunk[2] + 3) * WORD_SIZE;

    char *dest = alloc(size);
    memcopy(dest, src, size);

    return dest;
}

char *copy_thunk_result(char *src) {
    while (src[0] == TRIDASH_TYPE_RESOLVED_THUNK) {
        src = ((char**)src)[1];

        if (!IS_REF(src))
            return src;
    }

    return copy_object(src);
}


//// Copying Referenced Objects

void copy_live(void) {
    while ((uintptr_t)scan_ptr < (uintptr_t)alloc_ptr) {
        switch(scan_ptr[0]) {
        case TRIDASH_TYPE_THUNK:
            scan_ptr = copy_thunk_closure(scan_ptr);
            break;

        case TRIDASH_TYPE_RESOLVED_THUNK:
            scan_ptr += (3 + ((uintptr_t*)scan_ptr)[2]) * WORD_SIZE;
            break;

        default:
            scan_ptr += 2 * WORD_SIZE;
            break;
        }
    }
}

char *copy_thunk_closure(char *object) {
    uintptr_t *thunk = (uintptr_t*)object;
    uintptr_t size = thunk[2];

    for (uintptr_t i = 0; i < size; i++) {
        thunk[3 + i] = (uintptr_t)copy_object((char*)(thunk[3 + i]));
    }

    return (char*)(thunk + 3 + size);
}


//// Utilities

void grow_memory(size_t amount) {
    size_t pages = amount / PAGE_SIZE;

    __builtin_wasm_memory_grow(0, pages);

    heap_size += amount;
}

void memcopy(char *dest, char *src, size_t size) {

    while (size--) {
        *dest = *src;

        dest++;
        src++;
    }
}

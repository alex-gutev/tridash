/**
 * marshaller.js
 *
 * Tridash WebAssembly Loader
 *
 * Copyright 2020 Alexander Gutev
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/**
 * Tridash Value Marshaller.
 *
 * Provides functionality for passing values from JavaScript to
 * Tridash modules and vice versa.
 */
class Marshaller {
    /**
     * Constructor
     *
     * @param module Runtime library module instance
     * @param memory WebAssembly Memory Object
     */
    constructor(module, memory) {
        this.module = module;
        this.memory = memory;
    };


    /* Marshalling from JavaScript to Tridash */

    /**
     * Convert a JavaScript value to a Tridash value.
     *
     * Supports:
     *
     *  - Numbers (Integers and Floating Point)
     *  - Strings
     *
     * @param value The JavaScript value
     *
     * @return Tridash value pointer
     */
    to_tridash(value) {
        if (Number.isInteger(value)) {
            return this.to_tridash_int(value);
        }
        else if (typeof value === 'number') {
            return this.to_tridash_float(value);
        }
        else if (typeof value === 'string') {
            return this.to_tridash_string(value);
        }

        throw Marshaller.EncodeError(value);
    }

    /**
     * Convert a JavaScript integer to a Tridash integer.
     *
     * @param value The JavaScript integer value
     *
     * @return Tridash value pointer
     */
    to_tridash_int(value) {
        if (Marshaller.min_int <= value && value <= Marshaller.max_int) {
            return (value << 2) | Marshaller.tag_int;
        }

        var ptr = this.module.exports.alloc(8);
        var words = new Uint32Array(this.memory.buffer, ptr, 2);

        ptr[0] = Marshaller.type_int;
        ptr[1] = value;

        return ptr;
    }

    /**
     * Convert a JavaScript float to a Tridash float.
     *
     * @param value The JavaScript floating-point value
     *
     * @return Tridash value pointer
     */
    to_tridash_float(value) {
        var ptr = this.alloc(8);
        var words = new Uint32Array(this.memory.buffer, ptr, 1);
        var floats = new Float32Array(this.memory.buffer, ptr+4, 1);

        words[0] = Marshaller.type_float;
        floats[0] = value;
    }

    /**
     * Convert a JavaScript string to a Tridash string.
     *
     * @param value The JavaScript string value
     *
     * @return Tridash value pointer
     */
    to_tridash_string(value) {
        const encoder = new TextEncoder();
        const utf8 = encoder.encode(value);

        var ptr = this.alloc(8 + utf8.length);
        var words = new Uint32Array(this.memory.buffer, ptr, 2);

        words[0] = Marshaller.type_string;
        words[1] = utf8.length;

        var bytes = new Uint8Array(this.memory.buffer, ptr + 8, utf8.length);
        bytes.set(utf8);
    }

    /**
     * Allocate a block of memory in the Tridash module's memory
     * space.
     *
     * @param size Size of the memory block in bytes.
     *
     * @return Pointer to the first byte of the block.
     */
    alloc(size) {
        return this.exports.alloc(size);
    }


    /* Marshalling from Tridash to JavaScript */

    /**
     * Convert a Tridash Value to a JavaScript value.
     *
     * @param ptr Pointer to the Tridash value.
     *
     * @return The JavaScript value.
     */
    to_js(ptr) {

        switch (ptr & Marshaller.tag_bits) {
        case Marshaller.tag_int:
            return ptr >> 2;

        default:
            return this.unbox(ptr);
        }
    }

    /**
     * Convert a Tridash object, stored in the module's memory heap,
     * to an equivalent JavaScript object.
     *
     * @param ptr Pointer to the Tridash object within the heap.
     *
     * @return The JavaScript object.
     */
    unbox(ptr) {
        var words = new Uint32Array(this.memory.buffer, ptr, 2);

        switch (words[0]) {
        case Marshaller.type_int: {
            var ints = new Int32Array(this.memory.buffer, ptr + 4, 1);
            return ints[0];
        } break;

        case Marshaller.type_float: {
            var floats = new Float32Array(this.memory.buffer, ptr + 4, 1);
            return floats[0];
        } break;

        case Marshaller.type_string:
            return this.decode_string(ptr);
        }

        throw Marshaller.DecodeError(words[0]);
    }

    /**
     * Decode a Tridash string object into a JavaScript string.
     *
     * @param ptr Pointer to the string object.
     *
     * @return The JavaScript string.
     */
    decode_string(ptr) {
        const words = new Uint32Array(this.memory.buffer, ptr, 2);
        const bytes = new Uint8Array(this.memory.buffer, ptr + 8, words[1]);

        const decoder = new TextDecoder();
        return decoder.decode(bytes);
    }
}

/* Pointer Tags */

/** Pointer Tag Mask */
Marshaller.tag_bits = 0x3;
/** Heap Pointer Tag */
Marshaller.tag_pointer = 0;
/** Immediate Integer Tag */
Marshaller.tag_int = 0x1;

/** Maximum Immediate Integer Value */
Marshaller.max_int = Math.pow(2, 29) - 1;
/** Minimum Immediate Integer Value */
Marshaller.min_int = -Math.pow(2, 29);


/* Object Types */

/** Boxed Integer */
Marshaller.type_int = 2;
/** Boxed Float */
Marshaller.type_float = 3;
/** String Object */
Marshaller.type_string = 4;


/* Errors */

/**
 * Error marshalling a value from JavaScript to Tridash.
 */
Marshaller.EncodeError = class extends Error {
    constructor(value) {
        super("Don't know how to encode: " + value);
        this.name = "Marshaller.EncodeError";
    }
};

/**
 * Error marshalling a value from Tridash to JavaScript.
 */
Marshaller.DecodeError = class extends Error {
    constructor(type) {
        super("Unknown object type: " + type);
        this.name = "Marshaller.DecodeError";
    }
};

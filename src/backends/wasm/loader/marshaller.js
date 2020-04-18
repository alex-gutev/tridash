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
     *
     * @param memory WebAssembly Memory Object
     *
     * @param stack_top Pointer to the memory location at which the
     *   pointer to the top of the stack is stored.
     *
     * @param stack_base Pointer to the element at the bottom of the
     *   stack.
     */
    constructor(module, memory, stack_top, stack_base) {
        this.module = module;
        this.memory = memory;

        this.stack_top = stack_top;
        this.stack_base = stack_base;
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
        else if (value instanceof Marshaller.Fail) {
            return this.module.exports.make_failure(
                this.to_tridash(value.type)
            );
        }
        else if (value instanceof Marshaller.TridashValue) {
            return value.ptr;
        }

        throw new Marshaller.EncodeError(value);
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

        const ptr = this.module.exports.alloc(8);
        const view = new DataView(this.memory.buffer, ptr);

        view.setUint32(0, Marshaller.type_int, true);
        view.setInt32(4, value, true);

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
        const ptr = this.alloc(8);
        const view = new DataView(this.memory.buffer, ptr);

        view.setUint32(0, Marshaller.type_float, true);
        view.setFloat32(4, value, true);

        return ptr;
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

        const ptr = this.alloc(8 + utf8.length);
        const view = new DataView(this.memory.buffer, ptr);

        view.setUint32(0, Marshaller.type_string, true);
        view.setUint32(4, utf8.length, true);

        var bytes = new Uint8Array(this.memory.buffer, ptr + 8, utf8.length);
        bytes.set(utf8);

        return ptr;
    }


    /* Runtime Library Functions */

    /**
     * Allocate a block of memory in the Tridash module's memory
     * space.
     *
     * @param size Size of the memory block in bytes.
     *
     * @return Pointer to the first byte of the block.
     */
    alloc(size) {
        return this.module.exports.alloc(size);
    }

    /**
     * If a Tridash value is a thunk, evaluate it and return a pointer
     * to the computed value.
     *
     * @param ptr Pointer to the Tridash value or thunk.
     *
     * @return Pointer to the computed value, if @a ptr is a thunk,
     *   or @a ptr itself if it is not a thunk.
     */
    resolve(ptr) {
        return this.module.exports.resolve(ptr);
    }


    /* Arrays */

    /**
     * Create an array of a given size on the Tridash heap.
     *
     * @param size Size of the array in 32-bit words.
     * @param type Type of array, by default array of objects.
     *
     * @return Pointer to the array.
     */
    make_array(size, type = Marshaller.type_array) {
        var ptr = this.alloc(8 + 4 * size);
        var words = new Uint32Array(this.memory.buffer, ptr, 2);

        words[0] = type;
        words[1] = size;

        return ptr;
    }

    /**
     * Set an element of a Tridash array.
     *
     * @param ptr Pointer to the array object.
     * @param index Element index.
     * @param value Value to which to set the element.
     */
    set_array_elem(ptr, index, value) {
        var words = new Uint32Array(this.memory.buffer, ptr + 8 + 4 * index, 1);
        words[0] = value;
    }

    /**
     * Set the values of the elements of a Tridash array.
     *
     * @param ptr Pointer to the array object.
     *
     * @param values Array of values to which the elements of the
     *   array are set.
     */
    set_array_elems(ptr, values) {
        var words = new Uint32Array(this.memory.buffer, ptr + 8, values.length);
        words.set(values);
    }

    /**
     * Return the value of an array element.
     *
     * @param ptr Pointer to the array object.
     * @param index Index of the element.
     *
     * @return The element value.
     */
    get_array_elem(ptr, index) {
        var words = new Uint32Array(this.memory.buffer, ptr + 8 + 4 * index, 1);
        return words[0];
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

        case Marshaller.tag_funcref: {
            const index = ptr >>> 2;

            if (index < 2) {
                return index === 0 ? false : true;
            }

            return new Marshaller.Funcref(ptr);
        } break;

        case Marshaller.tag_fail:
            ptr = ptr & ~0x3;

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
        const view = new DataView(this.memory.buffer, ptr);
        const type = view.getUint32(0, true);

        switch (type) {
        case Marshaller.type_thunk:
        case Marshaller.type_resolved_thunk:
            return new Marshaller.Thunk(this, ptr);

        case Marshaller.type_int:
            return view.getInt32(4, true);

        case Marshaller.type_float:
            return view.getFloat32(4, true);

        case Marshaller.type_string:
            return this.decode_string(ptr);


        case Marshaller.type_array:
            return this.to_js_array(ptr, view);

        case Marshaller.type_int_array:
            return this.to_js_int_array(ptr, view);


        case Marshaller.type_object:
            return this.to_js_object(ptr, view);


        case Marshaller.type_char:
            return new Marshaller.Char(view.getUint32(4, true));

        case Marshaller.type_symbol:
            return new Marshaller.Symbol(this.decode_string(ptr));


        case Marshaller.type_list_node:
            return new Marshaller.ListNode(
                this,
                view.getUint32(4, true),
                view.getUint32(8, true)
            );


        case Marshaller.type_failure:
            return new Marshaller.Fail(
                this.to_js(view.getUint32(4, true))
            );

        case Marshaller.type_node:
            return new Marshaller.Node(
                view.getUint32(4, true)
            );

        case Marshaller.type_funcref_args:
            return new Marshaller.Funcref(ptr);
        }

        throw new Marshaller.DecodeError(type);
    }

    /**
     * Decode a Tridash string object into a JavaScript string.
     *
     * @param ptr Pointer to the string object.
     *
     * @return The JavaScript string.
     */
    decode_string(ptr) {
        const view = new DataView(this.memory.buffer, ptr);
        const bytes = new Uint8Array(this.memory.buffer, ptr + 8, view.getUint32(4, true));

        const decoder = new TextDecoder();
        return decoder.decode(bytes);
    }

    /**
     * Convert a Tridash object array to a JavaScript array of
     * JavaScript objects.
     *
     * @param ptr Pointer to the array object.
     *
     * @param view DataView object for the memory region beginning
     *   at @a ptr.
     */
    to_js_array(ptr, view) {
        const objects = this.to_js_int_array(ptr, view);
        return Array.prototype.map.call(objects, this.to_js.bind(this));
    }

    /**
     * Convert a Tridash integer array object a JavaScript
     * Uint32Array.
     *
     * @param ptr Pointer to the array object.
     *
     * @param view DataView object for the memory region beginning
     *   at @a ptr.
     */
    to_js_int_array(ptr, view) {
        const size = view.getUint32(4, true);
        return new Uint32Array(this.memory.buffer, ptr + 8, size);
    }


    /** Objects */

    /**
     * Convert a user-defined Tridash object to a JavaScript object
     * with equivalent fields.
     *
     * Note: The values of the object fields are not resolved, nor
     * converted to JavaScript objects. Rather the pointers to the
     * field values are stored.
     *
     * @param ptr Pointer to the user-defined object.
     *
     * @param view DataView object for the memory region beginning
     *   at @a ptr.
     *
     * @return The JavaScript Object
     */
    to_js_object(ptr, view) {
        const descriptor = view.getUint32(4, true);
        const desc_view = new DataView(this.memory.buffer, descriptor);

        var object = {}

        for (var i = 0; i < desc_view.getUint32(4, true); i++) {
            const bucket = 8 + i * 8;

            const key = desc_view.getUint32(bucket, true);
            const index = desc_view.getUint32(bucket + 4, true);

            if (key !== 0) {
                object[this.decode_string(key - 4)] =
                    view.getUint32(8 + index * 4, true);
            }
        }

        return object;
    }

    /**
     * Resolve, and convert to a JavaScript value, the value of each
     * field in an object.
     *
     * @param object JavaScript object, converted from a Tridash
     *   user-defined object, in which the fields are pointers to the
     *   objects.
     */
    resolve_object_fields(object) {
        const fields = Object.keys(object);

        fields.forEach((field) => {
            this.stack_push(object[field]);
        });

        fields.reverse().forEach((field) => {
            object[field] = this.to_js(this.resolve(this.stack_pop()));
        });
    }

    /** Stack Manipulation */

    /**
     * Return the stack element at index @a index from the bottom of
     * the stack.
     *
     * @param index Index of the element to return.
     *
     * @return The value of the stack element.
     */
    stack_elem(index) {
        const words = new Uint32Array(this.memory.buffer, this.stack_base - index, 1);
        return words[0];
    }

    /**
     * Set the value of the stack element at index @a index from the
     * bottom of the stack.
     *
     * @param index Index of the element to set.
     * @param ptr Value (reference) to the set the element to.
     */
    set_stack_elem(index, ptr) {
        const words = new Uint32Array(this.memory.buffer, this.stack_base - index, 1);
        words[index] = ptr;
    }

    /**
     * Push a value on the stack.
     *
     * @param value The value to push, interpreted as a reference.
     *
     * @return Pointer to the stack element at which @a value is
     *   stored.
     */
    stack_push(value) {
        const view = new DataView(this.memory.buffer);
        const top = view.getUint32(this.stack_top, true);

        view.setUint32(top, value, true);
        view.setUint32(this.stack_top, top - 4, true);

        return top;
    }

    /**
     * Pop an element of the stack and return it.
     *
     * @return The value at the top of the stack.
     */
    stack_pop() {
        const view = new DataView(this.memory.buffer);
        const top = view.getUint32(this.stack_top, true) + 4;

        const value = view.getUint32(top, true);

        view.setUint32(this.stack_top, top, true);

        return value;
    }


    /* Raw Memory Access */

    /**
     * Return the value of a given word in memory.
     *
     * @param ptr Pointer to the 32-bit word.
     *
     * @return 32-bit unsigned integer value.
     */
    get_word(ptr) {
        const view = new DataView(this.memory.buffer);
        return view.getUint32(ptr, true);
    }
}

/* Pointer Tags */

/** Pointer Tag Mask */
Marshaller.tag_bits = 0x3;
/** Heap Pointer Tag */
Marshaller.tag_pointer = 0;
/** Immediate Integer Tag */
Marshaller.tag_int = 0x1;
/** Function Reference */
Marshaller.tag_funcref = 0x2;
/** Failure Value */
Marshaller.tag_fail = 0x3;

/** Maximum Immediate Integer Value */
Marshaller.max_int = Math.pow(2, 29) - 1;
/** Minimum Immediate Integer Value */
Marshaller.min_int = -Math.pow(2, 29);


/* Object Types */

/** Thunks */
Marshaller.type_thunk = 0;
Marshaller.type_resolved_thunk = 1;

/** Boxed Integer */
Marshaller.type_int = 2;
/** Boxed Float */
Marshaller.type_float = 3;

/** String Object */
Marshaller.type_string = 4;

/** Failure Value */
Marshaller.type_fail = 5;

/** Arrays */
Marshaller.type_array = 8; /* Object Array */
Marshaller.type_int_array = 12; /* Integer Array */

/** Objects */
Marshaller.type_object = 11;

/** Symbols */
Marshaller.type_symbol = 9;

/** Characters */
Marshaller.type_char = 10;


/** Linked List Node */
Marshaller.type_list_node = 13;


/** Failure */
Marshaller.type_failure = 5;


/** Raw Node Object */
Marshaller.type_node = 14;

/* Meta-Node Reference Object */
Marshaller.type_funcref = 6;
Marshaller.type_funcref_args = 7;

/* JS Object Types */

/**
 * Represents a thunk on the Tridash heap.
 *
 * Methods:
 *
 * resolve():
 *
 *   Resolve the thunk and return the resolved value.
 *
 *   NOTE: Must be called before any operations are performed on the
 *   Tridash heap.
 */
Marshaller.Thunk = function(marshaller, ptr) {
    this.resolve = () => {
        return this.value === undefined ?
            (this.value = marshaller.to_js(marshaller.resolve(ptr))) :
            this.value;
    };
};

/**
 * Represents a character, with code @a code.
 */
Marshaller.Char = function(code) {
    this.code = code;
};

/**
 * Represents a Tridash symbol with name string @a name.
 */
Marshaller.Symbol = function(name) {
    this.name = name;
};

/**
 * Represents a linked list node.
 *
 * Members:
 *
 *  head: The element stored in the list node.
 *  tail: Pointer to the next list node.
 *
 * Initially both `head` and `tail` contain pointers to the actual
 * objects in the Tridash heap. The `resolve` method converts those
 * pointers to JavaScript objects.
 *
 * Methods:
 *
 *  resolve():
 *
 *    Convert the `head` and `tail` to JavaScript objects, resolving
 *    any thunks if necessary.
 *
 *    NOTE: This method must be called before any operations are
 *    performed on the Tridash heap.
 *
 */
Marshaller.ListNode = function(marshaller, head, tail) {
    var resolved = false;

    this.head = head;
    this.tail = tail;

    this.resolve = () => {
        if (!resolved) {
            marshaller.stack_push(tail);
            marshaller.stack_push(head);

            this.head = marshaller.to_js(marshaller.resolve(head));
            marshaller.stack_pop();

            this.tail = marshaller.to_js(marshaller.resolve(marshaller.stack_pop()));
            resolved = true;
        }
    };
};

/**
 * Represents a Tridash Failure Value of type @a type.
 *
 * Members:
 *
 *  type: Value identifying type of failure.
 */
Marshaller.Fail = function(type) {
    this.type = type;
};

/**
 * Represents a reference to a raw node object.
 *
 * @param id Node index identifier
 */
Marshaller.Node = function(id) {
    this.id = id;
};

/**
 * Represents a meta-node reference object.
 *
 * @param ptr Pointer to the function reference object (can be either
 *   a tagged pointer or pointer to object).
 */
Marshaller.Funcref = function(ptr) {
    this.ptr = ptr;
};


/** Tridash Value Wrapper */

/**
 * Represents a value which has been already marshalled from
 * JavaScript to the Tridash heap.
 *
 * @param ptr Pointer to the Tridash value.
 */
Marshaller.TridashValue = function(ptr) {
    this.ptr = ptr;
};

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

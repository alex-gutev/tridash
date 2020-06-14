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

        this.init_node_refs();
    };

    /**
     * Store the pointers to the node references, of the builtin
     * failure type nodes, in the 'node_refs' array.
     */
    init_node_refs() {
        this.node_refs = [];

        const types = Marshaller.FailTypes;

        this.node_refs[types.NoValue] = this.module.exports.fail_no_value();
        this.node_refs[types.TypeError] = this.module.exports.fail_type_error();
        this.node_refs[types.InvalidInteger] = this.module.exports.fail_invalid_integer();
        this.node_refs[types.InvalidReal] = this.module.exports.fail_invalid_real();
        this.node_refs[types.ArityError] = this.module.exports.fail_arity_error();
        this.node_refs[types.IndexOutBounds] = this.module.exports.fail_index_out_bounds();
        this.node_refs[types.Empty] = this.module.exports.empty_list();
    }


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
        else if (Array.isArray(value)) {
            return this.to_tridash_array(value);
        }
        else if (value instanceof Marshaller.Fail) {
            return this.module.exports.make_failure(
                this.to_tridash(value.type)
            );
        }
        else if (value instanceof Marshaller.TridashValue) {
            return value.ptr;
        }
        else if (value === true) {
            return Marshaller.True;
        }
        else if (value === false) {
            return Marshaller.False;
        }
        else if (value instanceof Marshaller.Char) {
            return this.to_tridash_char(value.code);
        }
        else if (value instanceof Marshaller.Symbol) {
            return this.to_tridash_symbol(value.name);
        }
        else if (value instanceof Marshaller.ListNode) {
            return this.to_tridash_list_node(value.head, value.tail);
        }
        else if (value instanceof Marshaller.Node) {
            const ptr = this.node_refs[value.id];

            if (ptr !== undefined) {
                return ptr;
            }
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
     * Create a Tridash character object on the WebAssembly heap.
     *
     * @param code Character code.
     *
     * @return Pointer to the object.
     */
    to_tridash_char(code) {
        const ptr = this.alloc(8);
        const buf = new Uint32Array(this.memory.buffer, ptr, 2);

        buf[0] = Marshaller.type_char;
        buf[1] = code;

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

    /**
     * Create a Tridash symbol object.
     *
     * @param name Symbol name.
     *
     * @return Pointer to the object.
     */
    to_tridash_symbol(name) {
        const encoder = new TextEncoder();
        const utf8 = encoder.encode(name);

        const ptr = this.alloc(8 + utf8.length);
        const view = new DataView(this.memory.buffer, ptr);

        view.setUint32(0, Marshaller.type_symbol, true);
        view.setUint32(4, utf8.length, true);

        var bytes = new Uint8Array(this.memory.buffer, ptr + 8, utf8.length);
        bytes.set(utf8);

        return ptr;
    }


    /**
     * Convert a JavaScript array to a Tridash object array. Each
     * object in the array is converted to a Tridash object, by
     * to_tridash.
     *
     * @param array JavaScript array.
     *
     * @return Pointer to the Tridash array.
     */
    to_tridash_array(array) {
        const loc = this.stack_push(this.make_array(array.length));

        for (var i = 0; i < array.length; i++) {
            this.set_array_elem(this.get_word(loc), i, this.to_tridash(array[i]));
        }

        return this.stack_pop();
    }

    to_tridash_list_node(head, tail) {
        const phead = this.to_tridash(head);
        this.stack_push(phead);

        const ptail = this.to_tridash(tail);

        return this.module.exports.make_list_node(this.stack_pop(), ptail);
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

        words.fill(0);

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

        if (ptr === 0) return null;

        switch (type) {
        case Marshaller.type_thunk:
        case Marshaller.type_resolved_thunk:
        case Marshaller.type_catch_thunk:
            return this.to_js(this.resolve(ptr));

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
            return this.to_js_list_node(ptr);


        case Marshaller.type_failure:
            return new Marshaller.Fail(
                this.to_js(view.getUint32(4, true))
            );

        case Marshaller.type_node: {
            const id = view.getUint32(4, true);
            this.node_refs[id] = ptr;

            return new Marshaller.Node(id);
        }

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
     *
     * @return JavaScript Array of Objects
     */
    to_js_array(ptr, view) {
        const size = view.getUint32(4, true);

        var objects = [];

        for (var i = 0; i < size; i++) {
            const mem = new DataView(this.memory.buffer, 0);

            this.stack_push(ptr);
            objects.push(this.to_js(mem.getUint32(ptr + 8 + i * 4, true)));
            ptr = this.stack_pop(ptr);
        }

        return objects;
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
     * @param ptr Pointer to the user-defined object.
     *
     * @param view DataView object for the memory region beginning
     *   at @a ptr.
     *
     * @return The JavaScript Object
     */
    to_js_object(ptr, view) {
        const descriptor = view.getUint32(4, true);
        var desc_view = new DataView(this.memory.buffer, descriptor);
        const buckets = desc_view.getUint32(4, true);

        var object = new Marshaller.Object();

        for (var i = 0; i < buckets; i++) {
            const mem = new DataView(this.memory.buffer, 0);
            desc_view = new DataView(this.memory.buffer, descriptor);

            const bucket = 8 + i * 8;

            const key = desc_view.getUint32(bucket, true);
            const index = desc_view.getUint32(bucket + 4, true);

            if (key !== 0) {
                this.stack_push(ptr);

                object[this.decode_string(key - 4)] =
                    this.to_js(mem.getUint32(ptr + 8 + index * 4, true));

                ptr = this.stack_pop(ptr);
            }
        }

        return object;
    }

    /**
     * Convert a Tridash linked list node to a JavaScript linked list
     * node represented by a Marshaller.ListNode object.
     *
     * @param ptr Pointer to the Tridash object.
     *
     * @return A Marshaller.ListNode object.
     */
    to_js_list_node(ptr) {
        var mem = new DataView(this.memory.buffer, 0);
        const node = new Marshaller.ListNode();

        this.stack_push(ptr);
        node.head = this.to_js(mem.getUint32(ptr + 4, true));

        mem = new DataView(this.memory.buffer, 0);
        ptr = this.stack_pop();
        node.tail = this.to_js(mem.getUint32(ptr + 8, true));

        return node;
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

    /**
     * Set the value of a given 32-bit word in memory.
     *
     * @param ptr Pointer to the word.
     * @param value Raw word value.
     */
    set_word(ptr, value) {
        const view = new DataView(this.memory.buffer);
        view.setUint32(ptr, value, true);
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


/* Boolean Values */

Marshaller.True = 4 | Marshaller.tag_funcref;
Marshaller.False = Marshaller.tag_funcref;

/* Object Types */

/** Thunks */
Marshaller.type_thunk = 0;
Marshaller.type_resolved_thunk = 1;
Marshaller.type_catch_thunk = 15;

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
 *  tail: The next node in the linked list.
 *
 */
Marshaller.ListNode = function(head, tail) {
    this.head = head;
    this.tail = tail;
};

/**
 * Represents a Tridash object with user-defined subnodes.
 *
 * The value of each subnode is stored in an object member with the
 * key being the same as the subnode identifier.
 */
Marshaller.Object = function() {};


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
 * Check whether a JS value represents a Tridash failure value.
 *
 * @param thing The JS value.
 *
 * @return True if @a thing represents a Tridash failure.
 */
Marshaller.Fail.is_fail = function(thing) {
    return thing instanceof Marshaller.Fail;
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


/** Builtin Failure Types */

Marshaller.FailTypes = {
    NoValue   : new Marshaller.Node(0),
    TypeError : new Marshaller.Node(1),
    InvalidInteger : new Marshaller.Node(2),
    InvalidReal : new Marshaller.Node(3),
    ArityError : new Marshaller.Node(4),
    IndexOutBounds : new Marshaller.Node(5),
    Empty     : new Marshaller.Node(6)
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

/**
 * runtime.test.js
 *
 * Tridash Wasm32 Runtime Library Tests
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
 */

const assert = require('assert');
const fs = require('fs');

/* Utility Functions */

/**
 * Create a WebAssembly Function Reference Table.
 *
 * @param size The initial size of the table.
 *
 * @return The table.
 */
function make_fn_table(size) {
    return new WebAssembly.Table({
        element: "anyfunc",
        initial: size
    });
}

/**
 * Create a WebAssembly Memory Object.
 *
 * @param size The initial size of the memory in pages.
 *
 * @return The memory object.
 */
function make_memory(size = 1) {
    return new WebAssembly.Memory({ initial: size });
}

/**
 * Load a WebAssembly module.
 *
 * @param file Path to the wasm file.
 * @param imports Imports object.
 *
 * @return the WebAssembly module instance.
 */
async function load_module(file, imports) {
    const bytes = fs.readFileSync(file);
    const module = await WebAssembly.compile(bytes);

    return await WebAssembly.instantiate(
        module, imports
    );
}

/**
 * Runtime library loader
 *
 * Loads and initializes the runtime library.
 *
 * Provides the following member variables:
 *
 *   stack_size:
 *
 *     Size of stack in bytes
 *
 *   constant_section_size:
 *
 *      Size of the unmanaged memory section in bytes.
 *
 *   stack_base:
 *
 *      Index of the element (32-bit word) at the base of the stack.
 *
 *   dylink_section:
 *
 *       Uint8Array of the bytes forming part of the 'dylink' custom
 *       section.
 *
 *   data_size:
 *
 *       Size of the memory requested by the module for internal
 *       use. This is located after the unmanaged memory section and
 *       before the GC managed heap.
 *
 *   base:
 *
 *     Module "memory base" pointer. Offset to the first byte
 *     available to the module for internal use.
 *
 *   heap_start:
 *
 *     Offset to the first byte of the GC managed heap.
 *
 *   table:
 *
 *     WebAssembly function table.
 *
 *   memory:
 *
 *     WebAssembly memory object.
 *
 *   instance:
 *
 *     WebAssembly module instance.
 *
 *   exports:
 *
 *     Runtime library module exports.
 *
 */
class Runtime {
    /**
     * Constructor
     *
     * @param stack_size The stack size to use in bytes
     *
     * @param constant_section_size The amount of space to reserve for
     *   the unmanaged memory section in bytes
     */
    constructor(stack_size = 24, constant_section_size = 24) {
        this.stack_size = stack_size;
        this.constant_section_size = constant_section_size;

        this.stack_base = this.stack_size / 4;
        this.constant_section = this.stack_size;
    }

    /**
     * Instantiate the runtime library module and initialize the
     * garbage collector.
     */
    async load() {
        const module = await Runtime.module;
        const sections = WebAssembly.Module.customSections(module, 'dylink');

        if (sections.length === 0)
            throw "No Dylink Section in Runtime";

        this.dylink_section = new Uint8Array(sections[0], 0, sections[0].byteLength);
        this.data_size = this.dylink_section[0] + 4;

        this.base = this.stack_size + this.constant_section_size;
        this.heap_start = this.base + this.data_size;

        this.table = make_fn_table(10);
        this.memory = make_memory(1);

        this.instance = await WebAssembly.instantiate(
            await module,
            {
                env: {
                    table: this.table,
                    memory: this.memory,
                    __memory_base: this.base + 4,
                    "g$stack_top": () => this.base
                }
            }
        );

        this.exports = this.instance.exports;
        this.instance.exports.__post_instantiate();

        this.init_memory();
    }

    /**
     * Initialize the garbage collector.
     *
     * @param limit Limit of the managed heap.
     */
    init_memory(limit = 64 * 1024) {
        var size = limit - this.heap_start;
        this.exports.initialize(this.stack_size - 4, this.heap_start, size);

        this.stack_index = this.stack_base - 1;
        this.stack_top = new Uint32Array(this.memory.buffer, this.base, 1);
        this.stack = new Uint32Array(this.memory.buffer, 0, this.stack_base);
    }

    /* Stack Manipulation */

    /**
     * Push a value (interpreted as a pointer) onto the stack.
     *
     * @param ptr The value to push
     */
    stack_push(ptr) {
        this.stack[this.stack_index--] = ptr;
        this.stack_top[0] -= 4;
    }

    /**
     * Return the element at a particular index within the stack.
     *
     * @param index Index of the element. The element at the top of
     *   the stack is located at index 0.
     *
     * @return Value of the stack element.
     */
    stack_elem(index) {
        return this.stack[this.stack_base - index - 1];
    }

    /** Loading Other Modules */

    /**
     * Load a WebAssembly module with the runtime library functions
     * imported into it.
     *
     * @param file Path to the wasm file.
     *
     * @return The module instance.
     */
    load_module(file) {
        var imports = {
            runtime: {
                table: this.table,
                memory: this.memory
            }
        };
        Object.assign(imports.runtime, this.exports);

        return load_module(file, imports);
    }
}

Runtime.bytes = fs.readFileSync('./runtime/runtime.wasm');
Runtime.module = WebAssembly.compile(Runtime.bytes);


/**
 * Tagged pointer tag values
 */
const Tags = {
    integer: 0x1,
    funcref: 0x2,
    failure: 0x3
};

/**
 * Tridash Object Type Constants
 */
const Types =  {
    thunk: 0,
    resolved_thunk: 1,
    integer: 2,
    real: 3,
    string: 4,
    failure: 5,
    funcref: 7,
    array: 8,
    symbol: 9,
    character: 10,
    object: 11,
    int_array: 12,
    list_node: 13
};

Object.freeze(Tags);
Object.freeze(Types);

/**
 * Encode an integer value into an immediate value tagged pointer.
 *
 * @param value The integer value.
 * @param tag The tag type (default integer)
 *
 * @return The tagged pointer.
 */
function im(value, tag = Tags.integer) {
    return (value << 2) | tag;
}


/**
 * Test Utilities
 *
 * Provides utility functions for creating Tridash objects in the
 * WebAssembly memory object and testing whether a memory location
 * contains an object of a certain type.
 */
class TestUtil {
    /**
     * Constructor
     *
     * @param table Indirect Function Table
     * @param memory Wasm Memory Object
     * @param runtime Runtime Library Module
     */
    constructor(table, memory, runtime) {
        this.table = table;
        this.memory = memory;
        this.runtime = runtime.exports;
    }


    /* Tridash Object Constructors */

    /**
     * Create a thunk.
     *
     * @param fn Thunk function index
     *
     * @param closure Array of values in the thunks closure.
     *
     * @param resolve If true a resolved thunk is create with result
     *   value @a fn, otherwise (the default) an unresolved thunk is
     *   created.
     *
     * @return Location of the thunk object.
     */
    make_thunk(fn, closure, resolved = false) {
        var ref = this.runtime.alloc((3 + closure.length) * 4);
        var words = new Uint32Array(this.memory.buffer, ref, 3 + closure.length);

        words[0] = resolved ? Types.resolved_thunk : Types.thunk;
        words[1] = fn;
        words[2] = closure.length;

        words.set(closure, 3);

        return ref;
    }

    /**
     * Create a boxed integer object.
     *
     * @param value Integer value
     * @return Location of the object
     */
    make_int(value) {
        var ref = this.runtime.alloc(2 * 4);
        var buf = new Int32Array(this.memory.buffer, ref, 2);

        buf[0] = Types.integer;
        buf[1] = value;

        return ref;
    }

    /**
     * Create a boxed integer object at a given memory location.
     *
     * @param offset Location at which to create the object.
     * @param value Integer value
     */
    box_int(offset, value) {
        var buf = new Int32Array(this.memory.buffer, offset, 2);

        buf[0] = Types.integer;
        buf[1] = value;
    }

    /**
     * Create a boxed float object.
     *
     * @param value Float value
     * @return Location of the object
     */
    make_float(value) {
        var ref = this.runtime.alloc(2 * 4);

        var words = new Uint32Array(this.memory.buffer, ref, 1);
        words[0] = Types.real;

        var floats = new Float32Array(this.memory.buffer, ref, 2);
        floats[1] = value;

        return ref;
    }

    /**
     * Create a boxed float object at a given memory location.
     *
     * @param offset Location at which to create the object.
     * @param value Integer value
     */
    box_float(offset, value) {
        var words = new Uint32Array(this.memory.buffer, offset, 1);
        words[0] = Types.real;

        var floats = new Float32Array(this.memory.buffer, offset, 2);
        floats[1] = value;
    }

    /**
     * Create a boxed character object.
     *
     * @param code Character code
     * @return Location of the object
     */
    make_char(code) {
        var ref = this.runtime.alloc(2 * 4);
        var words = new Uint32Array(this.memory.buffer, ref, 2);

        words[0] = Types.character;
        words[1] = code;

        return ref;
    }

    /**
     * Create a string object.
     *
     * @param str The string. Should only contain ASCII characters.
     *
     * @return Location of the object
     */
    make_string(str) {
        var ref = this.runtime.alloc(2 * 4 + str.length);
        var words = new Uint32Array(this.memory.buffer, ref, 1);

        // Set object type = 'string'
        words[0] = Types.string;
        this.encode_string(ref + 4, str);

        return ref;
    }

    /**
     * Create a symbol object.
     *
     * @param name Symbol name string. Should only contain ASCII
     *   characters.
     *
     * @return Location of the object
     */
    make_symbol(name) {
        var ref = this.runtime.alloc((2 * 4) + name.length);
        var words = new Uint32Array(this.memory.buffer, ref, 1);

        words[0] = Types.symbol;
        this.encode_string(ref + 4, name);

        return ref;
    }

    /**
     * Encode a JS string (in UTF-8) at a given memory location. The
     * first 32-bit word contains the string length followed by the
     * bytes comprising the string.
     *
     * @param offset Location at which to encode the string
     * @param str String to encode
     */
    encode_string(offset, str) {
        var words = new Uint32Array(this.memory.buffer, offset, 1);

        // Encode string to UTF-8
        var encoder = new TextEncoder();
        var utf8 = encoder.encode(str);

        // Store String Size
        words[0] = utf8.length;

        // Store string contents1
        var bytes = new Uint8Array(this.memory.buffer, offset + 4, utf8.length);
        bytes.set(utf8);
    }

    /**
     * Create an array object.
     *
     * @param array Array of values
     *
     * @return Location of the object
     */
    make_array(array, type = Types.array) {
        var ref = this.runtime.alloc((2 * 4) + array.length * 4);
        var buf = new Uint32Array(this.memory.buffer, ref, 2 + array.length);

        buf[0] = type;
        buf[1] = array.length;
        buf.set(array, 2);

        return ref;
    }

    /**
     * Create a function reference object.
     *
     * @param fn Function index
     * @param args Array of default argument values
     *
     * @return Location of the object
     */
    make_funcref(fn, args) {
        var ref = this.runtime.alloc((3 * 4) + args.length * 4);
        var buf = new Uint32Array(this.memory.buffer, ref, 3 + args.length);

        buf[0] = Types.funcref;
        buf[1] = fn;
        buf[2] = args.length;
        buf.set(args, 3);

        return ref;
    }

    /**
     * Create a simple object descriptor.
     *
     * The descriptor created, only contains the number of subnodes in
     * the object, and not the map from subnode identifiers to array
     * indices.
     *
     * NOTE: The descriptor is created on the heap managed by the
     * garbage collector, thus will survive for, at most, one garbage
     * collection cycle.
     *
     * @param num_fields Number of subnodes
     *
     * @return Location of the descriptor.
     */
    make_simple_object_descriptor(num_fields) {
        var ref = this.runtime.alloc(4);
        var buf = new Uint32Array(this.memory.buffer, ref, 1);

        buf[0] = num_fields;
        return ref;
    }

    /**
     * Create a user-defined Tridash object.
     *
     * @param descriptor Location of the object descriptor
     * @param values Array of subnode values.
     *
     * @return Location of the object
     */
    make_object(descriptor, values) {
        var ref = this.runtime.alloc(8 + values.length * 4);
        var buf = new Uint32Array(this.memory.buffer, ref, 2 + values.length);

        buf[0] = Types.object;
        buf[1] = descriptor;
        buf.set(values, 2);

        return ref;
    }


    /* Linked Lists */

    /**
     * Create a linked list node.
     *
     * @param head Element value stored in the node.
     * @param Pointer to next node.
     *
     * @return Pointer to the linked list node.
     */
    make_list(head, tail) {
        return this.runtime.make_list_node(head, tail);
    }


    /* Checking Addresses */

    /**
     * Check whether an address is aligned to a particular word
     * boundary.
     *
     * @param address The address
     *
     * @param alignment The alignment (multiple of 2), by default word
     *   alignment (4).
     *
     * @return True if the address is aligned.
     */
    static is_aligned(address, alignment = 4) {
        return (address % alignment) === 0;
    }

    /**
     * Assert that an object was copied during the last garbage
     * collection cycle, by checking that the new object reference is
     * not equal to the reference prior to the GC cycle.
     *
     * @param old_ref Reference to the object, before GC.
     * @param new_ref Reference to the object, after GC.
     *
     * @return @a new_ref if it is different from @a old_ref,
     *   otherwise this function does not return.
     */
    static check_copied(old_ref, new_ref) {
        assert.notEqual(old_ref, new_ref, 'Object not copied');
        return new_ref;
    }


    /* Checking Tridash Objects */

    /**
     * Assert that a memory location contains a thunk with a given
     * function index and closure size. Does not compare the closure
     * elements themselves.
     *
     * @param ref The memory location
     *
     * @param fn Function index
     *
     * @param size Closure size
     *
     * @param resolved If true a resolved thunk is expected. Otherwise
     *   an unresolved thunk is expected.
     */
    check_thunk(ref, fn, size, resolved = false) {
        var words = new Uint32Array(this.memory.buffer, ref, 3);

        assert.equal(words[0], resolved ? Types.resolved_thunk : Types.thunk,
                     'Object not thunk');

        assert.equal(words[1], fn, 'Incorrect thunk function');
        assert.equal(words[2], size, 'Incorrect closure size');
    }

    /**
     * Assert that a memory location contains a boxed integer with a
     * given value.
     *
     * @param ref The memory location
     * @param value The integer value
     */
    check_int(ref, value) {
        var words = new Uint32Array(this.memory.buffer, ref, 2);

        assert.equal(words[0], Types.integer, 'Object not integer');
        assert.equal(words[1], value, 'Incorrect integer value');
    }

    /**
     * Assert that a memory location contains a boxed character with a
     * given value.
     *
     * @param ref The memory location
     * @param value The character code
     */
    check_char(ref, value) {
        var words = new Uint32Array(this.memory.buffer, ref, 2);

        assert.equal(words[0], Types.character, 'Object not character');
        assert.equal(words[1], value, 'Incorrect character code');
    }

    /**
     * Assert that a memory location contains a string object.
     *
     * @param ref The memory location
     * @param str The expected string contents (as a JS string)
     */
    check_string(ref, str) {
        var words = new Uint32Array(this.memory.buffer, ref, 2);

        assert.equal(words[0], Types.string, 'Copied object not string');
        this.check_string_bytes(ref + 4, str);
    }

    /**
     * Assert that a memory location contains a symbol object.
     *
     * @param ref The memory location
     * @param name The expected symbol name (as a JS string)
     */
    check_symbol(ref, name) {
        var words = new Uint32Array(this.memory.buffer, ref, 1);

        assert.equal(words[0], Types.symbol, 'Object not symbol');
        this.check_string_bytes(ref + 4, name);
    }

    /**
     * Assert that a memory location contains a given string.
     *
     * The string is encoded to UTF-8, and it is checked that the
     * first word of the location contains the size of the UTF-8 byte
     * array followed by the contents of the array.
     *
     * @param ref The memory location
     * @param str The expected string (as a JS string)
     */
    check_string_bytes(ref, str) {
        var words = new Uint32Array(this.memory.buffer, ref, 1);

        // Assuming ASCII characters only
        assert.equal(words[0], str.length, 'Copied string not of same length');

        var bytes = new Uint8Array(this.memory.buffer, ref + 4, str.length);
        var string = new TextDecoder('utf8').decode(bytes);

        assert.equal(string, str, 'Copied string not equal to original.');
    }

    /**
     * Assert that a memory location contains an array object of a
     * given size. The array elements themselves are not checked.
     *
     * @param ref The memory location
     * @param length Array length
     */
    check_is_array(ref, length, type = Types.array) {
        var words = new Uint32Array(this.memory.buffer, ref, 2);

        assert.equal(words[0], type, 'Copied object is array');
        assert.equal(words[1], length, 'Incorrect array length');
    }

    /**
     * Assert that a memory location contains a function reference
     * object with a given function index and default argument array
     * size. The array elements themselves are not checked.
     *
     * @param ref The memory location
     * @param fn Function index
     * @param length Default argument array length
     */
    check_is_funcref(ref, fn, length) {
        var words = new Uint32Array(this.memory.buffer, ref, 3);

        assert.equal(words[0], Types.funcref, 'Object not function reference');
        assert.equal(words[1], fn, 'Incorrect function index');
        assert.equal(words[2], length, 'Incorrect number of arguments in function reference');
    }

    /**
     * Assert that a memory location contains a user-defined object
     * with a given object descriptor. The subnode values are not
     * checked.
     *
     * @param ref The memory location
     * @param descriptor Location of object descriptor
     */
    check_is_object(ref, descriptor) {
        var words = new Uint32Array(this.memory.buffer, ref, 2);

        assert.equal(words[0], Types.object, 'Object not user-object');
        assert.equal(words[1], descriptor, 'Incorrect object descriptor');
    }

    /**
     * Assert that a memory locations contains a linked list node
     * object.
     *
     * @param ref The memory location.
     */
    check_is_list(ref) {
        const view = new DataView(this.memory.buffer, ref);

        assert.equal(view.getUint32(0, true), Types.list_node, 'Object not linked list node');
    }


    /* Checking Failures */

    /**
     * Check that a Tridash object pointer is tagged as a failure
     * value.
     *
     * @param ref The tridash Pointer
     *
     * @return True if the pointer is tagged as a failure value
     */
    static is_tag_fail(ref) {
        return (ref & 0x3) == 3;
    }

    /**
     * Remove the tag from a tagged failure pointer, returning the
     * memory location of the failure value object.
     *
     * @param ref The tagged failure value pointer
     *
     * @return Location of the failure value object
     */
    static fail_ptr(ref) {
        return ref - 3;
    }

    /**
     * Assert that a memory location contains a failure value object
     * with a given type.
     *
     * @param ref The memory location
     * @param type Failure type value
     */
    check_is_failure(ref, type) {
        var words = new Uint32Array(this.memory.buffer, ref, 2);

        assert.equal(words[0], Types.failure, 'Object not failure');
        assert.equal(words[1], type, 'Incorrect failure type');
    }
};


/* Tests */

describe('Lazy Evaluation', function() {
    var runtime;
    var fns;
    var util;

    /**
     * Create a thunk on the heap.
     *
     * @param mem The WebAssembly memory.
     * @param offset Offset into the heap at which to create the thunk.
     * @param fn Index of thunk function.
     * @param data Closure data.
     */
    function make_thunk(mem, offset, fn, data) {
        var words = new Uint32Array(mem.buffer, offset, 3);

        words[0] = 0;
        words[1] = fn;
        words[2] = data;
    }

    beforeEach(async function() {
        runtime = new Runtime();
        await runtime.load();

        fns = runtime.exports;
        util = new TestUtil(runtime.table, runtime.memory, runtime.instance);
    });

    describe('resolve(Thunk)', function() {
        it('Should directly return immediate values', function() {
            const val = im(1);
            assert.equal(fns.resolve(val), val);
        });

        it('Should directly return pointers to boxed values', function() {
            util.box_int(4, 24);
            util.box_float(12, 7.5);

            assert.equal(fns.resolve(4), 4);
            assert.equal(fns.resolve(12), 12);
        });

        it('Should call thunk function to compute value', async function() {
            const thunks = await runtime.load_module('./runtime/thunks.wasm');

            var mem = new Uint32Array(runtime.memory.buffer, 4, 3);

            assert.equal(fns.resolve(4), im(1));

            // Test that the type is updated to 'resolved thunk', and
            // the function index is replaced with the computed value.
            assert.deepEqual(mem, Uint32Array.from([1,im(1),0]));

            // Test that the computed value will be returned, when
            // calling resolve on a computed thunk.
            assert.equal(fns.resolve(4), im(1));
        });

        it('Should pass closure to thunk function', async function() {
            const thunks = await runtime.load_module('./runtime/thunks.wasm');

            // Make closure thunk with closure over value 21
            make_thunk(runtime.memory, 16, 2, 21);

            var mem = new Uint32Array(runtime.memory.buffer, 16, 3);

            assert.equal(fns.resolve(16), im(22));

            // Test that the type is updated to 'resolve thunk', and
            // the function index is replaced with the computed value.
            assert.deepEqual(mem, Uint32Array.from([1,im(22),21]));

            // Test that the computed value will be returned, when
            // calling resolve on a computed thunk.
            assert.equal(fns.resolve(16), im(22));
        });

        it('Should compute value of thunks, returned by thunks', async function() {
            const thunks = await runtime.load_module('./runtime/thunks.wasm');

            // Make two chained thunks
            make_thunk(runtime.memory, 16, 3, 0);
            make_thunk(runtime.memory, 28, 3, 0);

            assert.equal(fns.resolve(16), im(1));

            // Test that calling resolve on a resolved thunk, which
            // returns a thunk, returns the final resolved value.
            assert.equal(fns.resolve(16), im(1));

            // Test that calling resolve on a thunk which returns a
            // resolved thunk, returns the final resolved value.
            assert.equal(fns.resolve(28), im(1));
        });
    });
});

describe('Core Functions', function() {
    var runtime;
    var util;

    beforeEach(async function() {
        runtime = new Runtime();

        await runtime.load();
        util = new TestUtil(runtime.table, runtime.memory, runtime.instance);
    });

    describe('Failures', function() {
        describe('make_failure', function() {
            it('Should create a failure on the heap', function() {
                var ref = runtime.exports.make_failure(im(3));

                assert(TestUtil.is_tag_fail(ref), 'Not tagged failure pointer');

                ref = TestUtil.fail_ptr(ref);
                util.check_is_failure(ref, im(3));
            });
        });
    });

    describe('Lists', function() {
        describe('make_list_node', function() {
            it('Should create a linked list node', function() {
                const node = runtime.exports.make_list_node(im(53), runtime.exports.empty_list());

                util.check_is_list(node);
            });
        });

        describe('list_node_head', function() {
            it('Should return the element stored in a linked list node', function() {
                const node = runtime.exports.make_list_node(im(53), runtime.exports.empty_list());

                assert.equal(runtime.exports.list_node_head(node), im(53));
            });

            it('Should return a failure of type Empty when given an empty list', function() {
                const list = runtime.exports.empty_list();
                var head = runtime.exports.list_node_head(list);

                assert(TestUtil.is_tag_fail(head), 'Not tagged failure pointer');
                head = TestUtil.fail_ptr(head);

                util.check_is_failure(head, runtime.exports.empty_list());
            });

            it('Should return Type-Error failure when given immediate integer', function() {
                var head = runtime.exports.list_node_head(im(7));

                assert(TestUtil.is_tag_fail(head), 'Not tagged failure pointer');
                head = TestUtil.fail_ptr(head);

                util.check_is_failure(head, runtime.exports.fail_type_error());
            });

            it('Should return Type-Error failure when given immediate function reference', function() {
                var head = runtime.exports.list_node_head(im(19, Tags.funcref));

                assert(TestUtil.is_tag_fail(head), 'Not tagged failure pointer');
                head = TestUtil.fail_ptr(head);

                util.check_is_failure(head, runtime.exports.fail_type_error());
            });

            it('Should return Type-Error failure when not given list node object', function() {
                var int_ref = util.make_int(78);
                var head = runtime.exports.list_node_head(int_ref);

                assert(TestUtil.is_tag_fail(head), 'Not tagged failure pointer');
                head = TestUtil.fail_ptr(head);

                util.check_is_failure(head, runtime.exports.fail_type_error());
            });

            it('Should return failure argument directly', function() {
                var head = runtime.exports.list_node_head(runtime.exports.make_fail_no_value());

                assert(TestUtil.is_tag_fail(head), 'Not tagged failure pointer');
                head = TestUtil.fail_ptr(head);

                util.check_is_failure(head, runtime.exports.fail_no_value());
            });
        });

        describe('list_node_tail', function() {
            it('Should return a pointer to the next node', function() {
                const node2 = runtime.exports.make_list_node(im(47), runtime.exports.empty_list());
                const node1 = runtime.exports.make_list_node(im(53), node2);

                assert.equal(runtime.exports.list_node_tail(node1), node2);
                assert.equal(runtime.exports.list_node_tail(node2), runtime.exports.empty_list());
            });

            it('Should return a failure of type Empty when given an empty list', function() {
                const list = runtime.exports.empty_list();
                var tail = runtime.exports.list_node_tail(list);

                assert(TestUtil.is_tag_fail(tail), 'Not tagged failure pointer');
                tail = TestUtil.fail_ptr(tail);

                util.check_is_failure(tail, runtime.exports.empty_list());
            });

            it('Should return Type-Error failure when given immediate integer', function() {
                var tail = runtime.exports.list_node_tail(im(7));

                assert(TestUtil.is_tag_fail(tail), 'Not tagged failure pointer');
                tail = TestUtil.fail_ptr(tail);

                util.check_is_failure(tail, runtime.exports.fail_type_error());
            });

            it('Should return Type-Error failure when given immediate function reference', function() {
                var tail = runtime.exports.list_node_tail(im(19, Tags.funcref));

                assert(TestUtil.is_tag_fail(tail), 'Not tagged failure pointer');
                tail = TestUtil.fail_ptr(tail);

                util.check_is_failure(tail, runtime.exports.fail_type_error());
            });

            it('Should return Type-Error failure when not given list node object', function() {
                var int_ref = util.make_int(78);
                var tail = runtime.exports.list_node_tail(int_ref);

                assert(TestUtil.is_tag_fail(tail), 'Not tagged failure pointer');
                tail = TestUtil.fail_ptr(tail);

                util.check_is_failure(tail, runtime.exports.fail_type_error());
            });

            it('Should return Type-Error failure when tail is not a list', function() {
                const node = runtime.exports.make_list_node(im(47), im(10));
                var tail = runtime.exports.list_node_tail(node);

                assert(TestUtil.is_tag_fail(tail), 'Not tagged failure pointer');
                tail = TestUtil.fail_ptr(tail);

                util.check_is_failure(tail, runtime.exports.fail_type_error());

            });

            it('Should return failure argument directly', function() {
                var tail = runtime.exports.list_node_tail(runtime.exports.make_fail_no_value());

                assert(TestUtil.is_tag_fail(tail), 'Not tagged failure pointer');
                tail = TestUtil.fail_ptr(tail);

                util.check_is_failure(tail, runtime.exports.fail_no_value());
            });
        });
    });
});

describe('Memory', function() {
    var runtime;
    var mem_buf;

    var util;

    beforeEach(async function() {
        runtime = new Runtime();
        await runtime.load();

        util = new TestUtil(runtime.table, runtime.memory, runtime);

        mem_buf = new Uint8Array(runtime.memory.buffer, 0, 64*1024);
    });

    describe('alloc(size)', function() {
        it('First block allocated at start of heap', function() {
            var addr = runtime.exports.alloc(8);
            assert.equal(addr, runtime.heap_start);
        });

        it('Blocks allocated in non-overlapping regions', function() {
            var addr1 = runtime.exports.alloc(4);
            mem_buf.set([1,2,3,4], addr1);

            var addr2 = runtime.exports.alloc(8);
            mem_buf.set([11,12,13,14,15,16,17,18], addr2);

            assert.deepEqual(
                mem_buf.slice(addr1, addr1 + 4),
                Uint8Array.from([1,2,3,4]),
                'Contents of first object overwritten'
            );
        });

        it('Blocks allocated at 32-bit word boundaries', function() {
            var addr1 = runtime.exports.alloc(2);
            var addr2 = runtime.exports.alloc(1);
            var addr3 = runtime.exports.alloc(4);

            assert(TestUtil.is_aligned(addr1), 'First object not word aligned');
            assert(TestUtil.is_aligned(addr2), 'Second object not word aligned');
            assert(TestUtil.is_aligned(addr3), 'Third object not word aligned');
        });
    });

    describe('Garbage Collection', function() {
        describe('Objects Reachable from Root Set', function() {
            it('Objects in root set to copied new heap', function() {
                var ref1 = util.make_int(0x0AF68735);
                runtime.stack_push(ref1);

                var ref2 = util.make_int(0x05003110);

                // Run Garbage Collection
                runtime.exports.run_gc();

                ref1 = TestUtil.check_copied(ref1, runtime.stack_elem(0));
                assert(TestUtil.is_aligned(ref1), 'Copied object address not word aligned');

                util.check_int(ref1, 0x0AF68735);
            });

            it('Garbage objects not copied to new heap', function() {
                var ref1 = util.make_int(0x0AF68735);
                runtime.stack_push(ref1);

                var ref2 = util.make_int(0x05003110);

                // Run Garbage Collection
                runtime.exports.run_gc();
                ref1 = runtime.stack_elem(0);

                assert.deepEqual(
                    mem_buf.slice(ref1+8, ref1+16),
                    Uint8Array.from([0,0,0,0,0,0,0,0]),
                    'Garbage object copied to new heap'
                );
            });

            it('All objects in root set copied to new heap', function() {
                // Create three boxed integers
                var ref1 = util.make_int(0x12345678);
                var ref2 = util.make_int(0x87654321);
                var ref3 = util.make_int(0x90340124);

                runtime.stack_push(ref1);
                runtime.stack_push(ref3);

                // Run Garbage Collection
                runtime.exports.run_gc();

                ref1 = TestUtil.check_copied(ref1, runtime.stack_elem(0));
                ref3 = TestUtil.check_copied(ref3, runtime.stack_elem(1));

                util.check_int(ref1, 0x12345678);
                util.check_int(ref3, 0x90340124);
            });

            it('Immediate integer operands not traversed', function() {
                var ref1 = im(1);
                var ref2 = im(34);

                runtime.stack_push(ref1);
                runtime.stack_push(ref2);

                runtime.exports.run_gc();

                assert.equal(runtime.stack_elem(0), ref1, 'Immediate operand 1 not preserved');
                assert.equal(runtime.stack_elem(1), ref2, 'Immediate operand 2 not preserved');
            });

            it('Boxed floats copied', function() {
                var ref = util.make_float(6.73);
                var fbox = mem_buf.slice(ref, ref+8);

                runtime.stack_push(ref);

                // Run Garbage Collection
                runtime.exports.run_gc();

                ref = TestUtil.check_copied(ref, runtime.stack_elem(0));

                // Compare new and old objects bytewise, since floating
                // point values cannot be accurately compared.
                assert.deepEqual(mem_buf.slice(ref, ref + 8), fbox, 'Object not copied correctly');
            });

            it('Boxed characters copied', function() {
                const chr = 'a'.codePointAt(0);
                var ref = util.make_char(chr);

                runtime.stack_push(ref);

                // Run Garbage Collection
                runtime.exports.run_gc();

                ref = TestUtil.check_copied(ref, runtime.stack_elem(0));
                util.check_char(ref, chr);
            });

            it('Strings copied', function() {
                var str = "hello world";
                var ref = util.make_string(str);

                runtime.stack_push(ref);

                // Run Garbage Collection
                runtime.exports.run_gc();

                ref = TestUtil.check_copied(ref, runtime.stack_elem(0));
                util.check_string(ref, str);
            });

            it('Symbols copied', function() {
                let name = "my-sym-a";
                var ref = util.make_symbol(name);

                runtime.stack_push(ref);

                // Run Garbage Collection
                runtime.exports.run_gc();

                ref = TestUtil.check_copied(ref, runtime.stack_elem(0));
                util.check_symbol(ref, name);
            });

            it('Failures copied', function() {
                // Create failure Type

                var str = "my type";
                var str_ref = util.make_string(str);

                // Create failure object

                var fail_ref = runtime.exports.make_failure(str_ref);
                runtime.stack_push(fail_ref);


                // Run Garbage Collection
                runtime.exports.run_gc();

                fail_ref = TestUtil.check_copied(fail_ref, runtime.stack_elem(0));
                assert(TestUtil.is_tag_fail(fail_ref), 'Not tagged failure');


                // Check that copied object is a failure

                var words = new Uint32Array(runtime.memory.buffer, TestUtil.fail_ptr(fail_ref), 2);
                assert.equal(words[0], Types.failure, 'Copied object not failure value');

                // Check that failure type was copied

                str_ref = TestUtil.check_copied(str_ref, words[1]);
                util.check_string(str_ref, str);
            });

            it('Arrays copied', function() {
                var str = "hello";
                var str_ref = util.make_string(str);
                var int_ref = util.make_int(23);
                var arr_ref = util.make_array([im(14), int_ref, str_ref]);

                runtime.stack_push(arr_ref);

                // Run garbage collection
                runtime.exports.run_gc();

                // Check Array

                arr_ref = TestUtil.check_copied(arr_ref, runtime.stack_elem(0));
                util.check_is_array(arr_ref, 3);

                /// Check Elements

                var words = new Uint32Array(runtime.memory.buffer, arr_ref + 8, 3);

                // Check element 1 - immediate integer
                assert.equal(words[0], im(14), 'Element 1 not copied');

                // Check element 2 - boxed integer
                int_ref = TestUtil.check_copied(int_ref, words[1]);
                util.check_int(int_ref, 23);

                // Check element 1 - string
                str_ref = TestUtil.check_copied(str_ref, words[2]);
                util.check_string(str_ref, str);
            });

            it('Integer arrays copied', function() {
                var int_ref = util.make_int(23);

                var array = [1, int_ref, 123, 0, 5];
                var arr_ref = util.make_array(array, Types.int_array);

                runtime.stack_push(arr_ref);

                // Run garbage collection
                runtime.exports.run_gc();

                // Check Array
                arr_ref = TestUtil.check_copied(arr_ref, runtime.stack_elem(0));
                util.check_is_array(arr_ref, array.length, Types.int_array);

                // Check Elements
                var words = new Uint32Array(runtime.memory.buffer, arr_ref + 8, array.length);

                assert.deepStrictEqual(words, Uint32Array.from(array), "Integer Array Contents");
            });

            it('Function references copied', function() {
                var str = "hello";
                var str_ref = util.make_string(str);
                var int_ref = util.make_int(37);

                // Also tests tagged function references
                var funcref = util.make_funcref(2, [im(19, Tags.funcref), int_ref, str_ref]);

                runtime.stack_push(funcref);

                // Run garbage collection
                runtime.exports.run_gc();

                // Check Function Reference
                funcref = TestUtil.check_copied(funcref, runtime.stack_elem(0));
                util.check_is_funcref(funcref, 2, 3);

                /// Check Arguments

                var words = new Uint32Array(runtime.memory.buffer, funcref + 3 * 4, 3);

                // Check element 1 - immediate integer
                assert.equal(words[0], im(19, Tags.funcref), 'Element 1 not copied');

                // Check element 2 - boxed integer
                int_ref = TestUtil.check_copied(int_ref, words[1]);
                util.check_int(int_ref, 37);

                // Check element 3 - string
                str_ref = TestUtil.check_copied(str_ref, words[2]);
                util.check_string(str_ref, str);
            });

            it('Objects copied', function() {
                var int_ref = util.make_int(17);

                var descriptor = util.make_simple_object_descriptor(2);
                var obj_ref = util.make_object(descriptor, [im(29), int_ref]);

                runtime.stack_push(obj_ref);

                // Run garbage collection
                runtime.exports.run_gc();

                // Check object Reference
                obj_ref = TestUtil.check_copied(obj_ref, runtime.stack_elem(0));
                util.check_is_object(obj_ref, descriptor);

                /// Check Fields

                var words = new Uint32Array(runtime.memory.buffer, obj_ref + 2 * 4, 2);

                // Check Field 1 - Immediate Integer
                assert.equal(words[0], im(29), 'Field 1 not copied');

                // Check Field 2 - Boxed Integer
                int_ref = TestUtil.check_copied(int_ref, words[1]);
                util.check_int(int_ref, 17);
            });

            it('Linked List Nodes copied', function() {
                var int_ref = util.make_int(24);
                var node2 = util.make_list(im(122), runtime.exports.empty_list());
                var node1 = util.make_list(int_ref, node2);

                runtime.stack_push(node1);

                // Run garbage collection
                runtime.exports.run_gc();

                // Check node1 reference
                node1 = TestUtil.check_copied(node1, runtime.stack_elem(0));
                util.check_is_list(node1);

                /// Check node1.head
                var words = new Uint32Array(runtime.memory.buffer, node1 + 4, 2);

                int_ref = TestUtil.check_copied(int_ref, words[0]);
                util.check_int(int_ref, 24);

                // Check node1.tail

                node2 = TestUtil.check_copied(node2, words[1]);
                util.check_is_list(node2);

                words = new Uint32Array(runtime.memory.buffer, node2 + 4, 2);

                assert.equal(words[0], im(122), 'Head of node 2 not copied');
                assert.equal(words[1], runtime.exports.empty_list(), 'Tail of node 2 not copied');
            });
        });

        describe('Objects Referenced by Thunks', function() {
            it('Thunks copied with objects in their closure', function() {
                // Create boxed integer on heap
                var int_ref = util.make_int(0x11223344);

                // Create thunk with two values in closure
                var thunk_ref = util.make_thunk(5, [im(3), int_ref]);

                runtime.stack_push(thunk_ref);


                // Run garbage collection
                runtime.exports.run_gc();

                // Check thunk object

                thunk_ref = TestUtil.check_copied(thunk_ref, runtime.stack_elem(0));
                util.check_thunk(thunk_ref, 5, 2);

                /// Check closure elements

                var closure = new Uint32Array(runtime.memory.buffer, thunk_ref + 3 * 4, 2);

                // Check element 1
                assert.equal(closure[0], im(3), 'Element 1 of closure not copied');

                // Check element 2
                int_ref = TestUtil.check_copied(int_ref, closure[1]);
                util.check_int(int_ref, 0x11223344);
            });

            it('Objects, referenced by multiple other objects, copied once', function() {
                // Create boxed integer on heap
                var int_ref = util.make_int(0x11223344);

                // Create thunk with two values in closure
                var thunk_ref = util.make_thunk(5, [im(3), int_ref]);

                runtime.stack_push(thunk_ref);
                runtime.stack_push(int_ref);

                // Run garbage collection
                runtime.exports.run_gc();

                // Check thunk object

                thunk_ref = TestUtil.check_copied(thunk_ref, runtime.stack_elem(0));
                util.check_thunk(thunk_ref, 5, 2);

                /// Check closure elements

                var closure = new Uint32Array(runtime.memory.buffer, thunk_ref + 3 * 4, 2);

                // Check element 1
                assert.equal(closure[0], im(3), 'Element 1 of closure not copied');

                // Check element 2
                int_ref = TestUtil.check_copied(int_ref, closure[1]);
                util.check_int(int_ref, 0x11223344);

                assert.equal(int_ref, runtime.stack_elem(1), 'Reference to boxed integer in closure not equal to reference in root set');
            });

            it('Thunks, referenced by other thunks, copied', function() {
                // Create boxed integer on heap
                var int_ref = util.make_int(0x10023044);

                // Create thunks
                var thunk2_ref = util.make_thunk(17, [int_ref]);
                var thunk1_ref = util.make_thunk(5, [im(3), thunk2_ref]);

                // Add thunk1 to root set
                runtime.stack_push(thunk1_ref);


                // Run garbage collection
                runtime.exports.run_gc();

                // Check that thunk was actually copied to new heap
                thunk1_ref = TestUtil.check_copied(thunk1_ref, runtime.stack_elem(0));
                util.check_thunk(thunk1_ref, 5, 2);

                /// Check closure elements
                var thunk1 = new Uint32Array(runtime.memory.buffer, thunk1_ref + 3 * 4, 2);

                // Check element 1
                assert.equal(thunk1[0], im(3), 'Element 1 of closure 1 not copied');

                // Check element 2
                thunk2_ref = TestUtil.check_copied(thunk2_ref, thunk1[1]);
                util.check_thunk(thunk2_ref, 17, 1);

                // Check thunk2's closure
                var thunk2 = new Uint32Array(runtime.memory.buffer, thunk2_ref + 3 * 4, 1);

                int_ref = TestUtil.check_copied(int_ref, thunk2[0]);
                util.check_int(int_ref, 0x10023044);
            });

            it('Resolved thunks replaced with their results (immediate values)', function() {
                // Create resolved thunk
                var thunk_ref = util.make_thunk(im(7), [im(87)], true);
                runtime.stack_push(thunk_ref);


                // Run garbage collection
                runtime.exports.run_gc();

                // Check that the resolved thunk was replaced with its
                // result
                assert.equal(runtime.stack_elem(0), im(7), 'Thunk not replaced with result in root set');
            });

            it('Result of resolved thunk copied', function() {
                // Create boxed integer result
                var result_ref = util.make_int(87006500);

                // Create thunk with no closure
                var thunk_ref = util.make_thunk(result_ref, [], true);
                runtime.stack_push(thunk_ref);


                // Run garbage collection
                runtime.exports.run_gc();

                // Check that the thunk reference was replaced with a
                // reference to the boxed integer result

                result_ref = TestUtil.check_copied(result_ref, runtime.stack_elem(0));
                util.check_int(result_ref, 87006500);
            });

            it('Thunks, with strings in closure, copied', function() {
                var str = "hello world!";
                var str_ref = util.make_string(str);

                var thunk_ref = util.make_thunk(4, [str_ref]);
                runtime.stack_push(thunk_ref);

                // Run garbage collection
                runtime.exports.run_gc();

                thunk_ref = TestUtil.check_copied(thunk_ref, runtime.stack_elem(0));
                util.check_thunk(thunk_ref, 4, 1);

                // Check that closure was copied correctly
                var closure = new Uint32Array(runtime.memory.buffer, thunk_ref + 3 * 4, 1);

                str_ref = TestUtil.check_copied(str_ref, closure[0]);
                util.check_string(str_ref, str);
            });
        });

        describe('Unmanaged Objects', function() {
            it('Unmanaged objects in root set not copied', function() {
                // Create object below GC managed heap

                var int_ref = runtime.constant_section;
                util.box_int(int_ref, 1527);

                runtime.stack_push(int_ref);

                // Run garbage collection
                runtime.exports.run_gc();

                assert.equal(int_ref, runtime.stack_elem(0), 'Unmanaged object copied');
                util.check_int(int_ref, 1527);
            });

            it('Unmanaged object referenced by managed object not copied', function() {
                // Create object below GC managed heap

                var int_ref = runtime.constant_section;
                util.box_int(int_ref, 1527);

                var arr_ref = util.make_array([int_ref]);

                runtime.stack_push(arr_ref);

                // Run garbage collection
                runtime.exports.run_gc();

                arr_ref = TestUtil.check_copied(arr_ref, runtime.stack_elem(0));
                util.check_is_array(arr_ref, 1);

                var array = new Uint32Array(runtime.memory.buffer, arr_ref + 8, 1);

                assert.equal(int_ref, array[0], 'Unmanaged object copied');
                util.check_int(int_ref, 1527);
            });
        });
    });
});

/**
 * runtime.test.js
 *
 * Tridash Wasm32 Runtime Library Tests
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
 * Load the runtime library module.
 *
 * @param table The function reference table to use.
 * @param memory The memory object to use.
 * @param base The address at which the module's memory space begins.
 *
 * @return The module instance.
 */
async function load_runtime(table, memory, base) {
    var module = await load_module(
        './runtime/runtime.wasm',
        {
            env: {
                table: table,
                memory: memory,
                __memory_base: base + 4,
                "g$stack_top" : () => base,
            }
        }
    );

    module.exports.__post_instantiate();
    return module;
}

/**
 * Load a WebAssembly module with the functions exported from the
 * runtime library. Imported into the module.
 *
 * @param file Path to the wasm file.
 * @param runtime Runtime library module instance.
 * @param table The function reference table to import.
 * @param memory The memory object to import.
 *
 * @return The module instance.
 */
function load_with_runtime(file, runtime, table, memory) {
    var imports = {
        runtime: {
            table: table,
            memory: memory
        }
    };
    Object.assign(imports.runtime, runtime.exports);

    return load_module(file, imports);
}

/**
 * Encode an integer into an immediate integer tagged pointer.
 *
 * @param value The integer value.
 *
 * @return The tagged pointer.
 */
function im(value) {
    return (value << 2) | 0x1;
}

/**
 * Create a boxed signed integer value on the heap.
 *
 * @param mem The WebAssembly memory.
 * @param offset Offset into the heap at which to create the boxed value.
 * @param value The signed integer value.
 */
function box_int(mem, offset, value) {
    buf = new Int32Array(mem.buffer, offset, 2);

    buf[0] = 2;
    buf[1] = value;
}

/**
 * Create a boxed 32-bit float value on the heap.
 *
 * @param mem The WebAssembly memory.
 * @param offset Offset into the heap at which to create the boxed value.
 * @param value The float value.
 */
function box_float(mem, offset, value) {
    words = new Uint32Array(mem.buffer, offset, 1);
    words[0] = 3;

    floats = new Float32Array(mem.buffer, offset, 2);
    floats[1] = value;
}

/**
 * Create a thunk on the heap.
 *
 * @param mem The WebAssembly memory.
 * @param offset Offset into the heap at which to create the thunk.
 * @param fn Index of thunk function.
 * @param data Closure data.
 */
function make_thunk(mem, offset, fn, data) {
    words = new Uint32Array(mem.buffer, offset, 3);

    words[0] = 0;
    words[1] = fn;
    words[2] = data;
}


/* Tests */

describe('Lazy Evaluation', function() {
    var runtime;
    var table;
    var memory;
    var fns;

    function load(file) {
        return load_with_runtime(file, runtime, table, memory);
    }

    beforeEach(async function() {
        table = make_fn_table(10);
        memory = make_memory(1);
        runtime = await load_runtime(table, memory);

        fns = runtime.exports;
    });

    describe('resolve(Thunk)', function() {
        it('Should directly return immediate values', function() {
            const val = im(1);
            assert.equal(fns.resolve(val), val);
        });

        it('Should directly return pointers to boxed values', function() {
            box_int(memory, 4, 24);
            box_float(memory, 12, 7.5);

            assert.equal(fns.resolve(4), 4);
            assert.equal(fns.resolve(12), 12);
        });

        it('Should call thunk function to compute value', async function() {
            const thunks = await load('./runtime/thunks.wasm');

            var mem = new Uint32Array(memory.buffer, 4, 3);

            assert.equal(fns.resolve(4), im(1));

            // Test that the type is updated to 'resolve thunk', and
            // the function index is replaced with the computed value.
            assert.deepEqual(mem, Uint32Array.from([1,im(1),0]));

            // Test that the computed value will be returned, when
            // calling resolve on a computed thunk.
            assert.equal(fns.resolve(4), im(1));
        });

        it('Should pass closure to thunk function', async function() {
            const thunks = await load('./runtime/thunks.wasm');

            // Make closure thunk with closure over value 21
            make_thunk(memory, 16, 2, 21);

            var mem = new Uint32Array(memory.buffer, 16, 3);

            assert.equal(fns.resolve(16), im(22));

            // Test that the type is updated to 'resolve thunk', and
            // the function index is replaced with the computed value.
            assert.deepEqual(mem, Uint32Array.from([1,im(22),21]));

            // Test that the computed value will be returned, when
            // calling resolve on a computed thunk.
            assert.equal(fns.resolve(16), im(22));
        });

        it('Should compute value of thunks, returned by thunks', async function() {
            const thunks = await load('./runtime/thunks.wasm');

            // Make two chained thunks
            make_thunk(memory, 16, 3, 0);
            make_thunk(memory, 28, 3, 0);

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

describe('Memory', function() {
    const heap_start = 60;
    const stack_size = 24;
    const stack_base = stack_size / 4;

    var runtime;
    var table;
    var memory;
    var exports;

    var stack_index;
    var stack_top;
    var stack;
    var mem_buf;

    /**
     * Load a Wasm file with the runtime functions imported into it.
     *
     * @param file Path to the file
     * @return The WebAssembly Module
     */
    function load(file) {
        return load_with_runtime(file, runtime, table, memory);
    }

    /**
     * Call the 'initialize' function of the garbage collector and set
     * the 'stack_index', 'stack_top', 'stack' and 'mem_buf'
     * variables.
     *
     * @param size Initial size of the heap.
     */
    function init_memory(size = 64*1024 - heap_start) {
        exports.initialize(stack_size - 4, heap_start, size);

        stack_index = stack_base - 1;
        stack_top = new Uint32Array(memory.buffer, stack_size, 1);
        stack = new Uint32Array(memory.buffer, 0, stack_base);
        mem_buf = new Uint8Array(memory.buffer, 0, 64*1024);
    }

    /**
     * Checks whether an address is aligned to a particular boundary.
     *
     * @param address The address
     *
     * @param alignment The alignment (multiple of 2), by default word
     *   alignment (4).
     *
     * @return True if the address is aligned.
     */
    function is_aligned(address, alignment = 4) {
        return (address % alignment) === 0;
    }

    /**
     * Creates a thunk.
     *
     * @param addr Address to create the thunk at.
     * @param type Thunk type.
     * @param fn Thunk function pointer.
     * @param closure Values in the thunk closure.
     */
    function make_thunk(addr, type, fn, ...closure) {
        var thunk = new Uint32Array(memory.buffer, addr, 3 + closure.length);

        thunk[0] = type;
        thunk[1] = fn;
        thunk[2] = closure.length;

        thunk.set(closure, 3);
    }


    /**
     * Push a pointer onto the stack.
     *
     * @param ptr The pointer to push.
     */
    function stack_push(ptr) {
        stack[stack_index--] = ptr;
        stack_top[0] -= 4;
    }

    /**
     * Retrieve an element from the stack.
     *
     * @param index Index of the element where index 0 references the
     *   bottom (base) of the stack.
     *
     * @return The element.
     */
    function stack_elem(index) {
        return stack[stack_base - index - 1];
    }

    beforeEach(async function() {
        table = make_fn_table(10);
        memory = make_memory(1);
        runtime = await load_runtime(table, memory, stack_size);

        exports = runtime.exports;

        init_memory();
    });

    describe('alloc(size)', function() {
        it('Should allocate first block at start of heap', function() {
            var addr = exports.alloc(8);
            assert.equal(addr, heap_start);
        });

        it('Should allocate blocks in non-overlapping regions', function() {
            var addr1 = exports.alloc(4);
            mem_buf.set([1,2,3,4], addr1);

            var addr2 = exports.alloc(8);
            mem_buf.set([11,12,13,14,15,16,17,18], addr2);

            assert.deepEqual(mem_buf.slice(addr1, addr1 + 4),
                             Uint8Array.from([1,2,3,4]),
                            "Contents of first object overwritten");
        });

        it('Should allocate blocks at 32-bit word boundaries', function() {
            var addr1 = exports.alloc(2);
            var addr2 = exports.alloc(1);
            var addr3 = exports.alloc(4);

            assert(is_aligned(addr1), "First object not word aligned")
            assert(is_aligned(addr2), "Second object not word aligned")
            assert(is_aligned(addr3), "Thid object not word aligned")
        });
    });

    describe('Garbage Collection', function() {
        describe('Objects Reachable from Root Set', function() {
            it('Should copy objects in root set to new heap', function() {
                var ref1 = exports.alloc(8);
                box_int(memory, ref1, 0x0AF68735);
                stack_push(ref1);

                var ref2 = exports.alloc(8);
                box_int(memory, ref2, 0x05003110);

                // Run Garbage Collection
                exports.run_gc();

                var new_ref1 = stack_elem(0);
                assert.notEqual(ref1, new_ref1, "Reference to object 1 not changed in root set");
                assert(is_aligned(new_ref1), "New object address not word aligned");

                assert.deepEqual(
                    new Uint32Array(memory.buffer, new_ref1, 2),
                    Uint32Array.from([2, 0x0AF68735]),
                    "Object 1 not copied correctly"
                );
            });

            it('Should not copy garbage objects to new heap', function() {
                var ref1 = exports.alloc(8);
                box_int(memory, ref1, 0x0AF68735);
                stack_push(ref1);

                var ref2 = exports.alloc(8);
                box_int(memory, ref2, 0x05003110);

                // Run Garbage Collection
                exports.run_gc();
                ref1 = stack_elem(0);

                assert.deepEqual(
                    mem_buf.slice(ref1+8, ref1+16),
                    Uint8Array.from([0,0,0,0,0,0,0,0]),
                    "Garbage object copied to new heap"
                );
            });

            it('Should copy all objects in root set to new heap', function() {
                // Create thre boxed integers

                var ref1 = exports.alloc(8);
                stack_push(ref1);
                box_int(memory, ref1, 0x12345678);

                var ref2 = exports.alloc(8);
                box_int(memory, ref2, 0x87654321);

                var ref3 = exports.alloc(8);
                stack_push(ref3);
                box_int(memory, ref3, 0x90340124);

                // Run Garbage Collection
                exports.run_gc();

                var new_ref1 = stack_elem(0);
                assert.notEqual(ref1, new_ref1, "Reference to object 1 not replaced in root set");

                var new_ref3 = stack_elem(1);
                assert.notEqual(ref3, new_ref3, "Reference to object 2 not replaced in root set");

                assert.deepEqual(
                    new Uint32Array(memory.buffer, new_ref1, 2),
                    Uint32Array.from([2, 0x12345678]),
                    "Object 1 not copied correctly"
                );

                assert.deepEqual(
                    new Uint32Array(memory.buffer, new_ref3, 2),
                    Uint32Array.from([2, 0x90340124]),
                    "Object 3 not copied correctly"
                );
            });

            it('Should not traverse immediate integer operands', function() {
                var ref1 = im(1);
                var ref2 = im(34);

                stack_push(ref1);
                stack_push(ref2);

                exports.run_gc();

                assert.equal(stack_elem(0), ref1, "Immediate operand 1 not preserved");
                assert.equal(stack_elem(1), ref2, "Immediate operand 2 not preserved");
            });

            it('Should copy boxed floats', function() {
                var ref = exports.alloc(8);
                stack_push(ref);

                box_float(memory, ref, 6.73);
                var fbox = mem_buf.slice(ref, ref+8);

                // Run Garbage Collection
                exports.run_gc();

                var new_ref = stack[stack_base - 1];
                assert.notEqual(ref, new_ref, "Reference to object not updated in root set");

                assert.deepEqual(mem_buf.slice(new_ref, new_ref+8), fbox, "Object not copied correctly");
            });
        });

        describe('Objects Referenced by Thunks', function() {
            it('Should copy thunks with objects in their closure', function() {
                // Create boxed integer on heap
                var int_ref = exports.alloc(2 * 4);
                box_int(memory, int_ref, 0x11223344);

                // Create thunk with two values in closure
                var thunk_ref = exports.alloc(5 * 4);
                make_thunk(thunk_ref, 0, 5, im(3), int_ref);
                stack_push(thunk_ref);


                // Run garbage collection
                exports.run_gc();

                // Check that thunk was actually copied to new heap
                assert.notEqual(thunk_ref, stack_elem(0),
                                "Reference to thunk not updated in root set");

                thunk_ref = stack_elem(0);
                thunk = new Uint32Array(memory.buffer, thunk_ref, 5);

                // Check that thunk object properties were copied
                // correctly

                assert.equal(thunk[0], 0, "Object type != Thunk");
                assert.equal(thunk[1], 5, "Incorrect thunk function");
                assert.equal(thunk[2], 2, "Incorrect closure size");

                // Check that closure was copied correctly
                assert.equal(thunk[3], im(3), "Immediate integer not copied in closure");

                // Check that reference to boxed integer was updated
                // in closure
                assert.notEqual(thunk[4], int_ref, "Reference to boxed integer not updated in closure");

                // Check that boxed integer was copied

                int_ref = thunk[4];
                assert.deepEqual(
                    new Uint32Array(memory.buffer, int_ref, 2),
                    Uint32Array.from([2, 0x11223344]),
                    "Referenced boxed integer not copied correctly"
                );
            });

            it('Should copy object, referenced by multiple other objects, once', function() {
                // Create boxed integer on heap
                var int_ref = exports.alloc(2 * 4);
                box_int(memory, int_ref, 0x11223344);

                // Create thunk with two values in closure
                var thunk_ref = exports.alloc(5 * 4);
                make_thunk(thunk_ref, 0, 5, im(3), int_ref);

                stack_push(thunk_ref);
                stack_push(int_ref);

                // Run garbage collection
                exports.run_gc();

                // Check that thunk was actually copied to new heap
                assert.notEqual(thunk_ref, stack_elem(0),
                                "Reference to thunk not updated in root set");

                thunk_ref = stack_elem(0);
                thunk = new Uint32Array(memory.buffer, thunk_ref, 5);

                // Check that thunk object properties were copied
                // correctly

                assert.equal(thunk[0], 0, "Object type != Thunk");
                assert.equal(thunk[1], 5, "Incorrect thunk function");
                assert.equal(thunk[2], 2, "Incorrect closure size");

                // Check that closure was copied correctly
                assert.equal(thunk[3], im(3), "Immediate integer not copied in closure");

                // Check that reference to boxed integer was updated
                // in closure
                assert.notEqual(thunk[4], int_ref, "Reference to boxed integer not updated in closure");
                assert.equal(thunk[4], stack_elem(1), "Reference to boxed integer in closure not equal to reference in root set");

                // Check that boxed integer was copied

                int_ref = thunk[4];
                assert.deepEqual(
                    new Uint32Array(memory.buffer, int_ref, 2),
                    Uint32Array.from([2, 0x11223344]),
                    "Referenced boxed integer not copied correctly"
                );
            });

            it('Should copy thunks referenced by thunks', function() {
                // Create boxed integer on heap
                var int_ref = exports.alloc(2 * 4);
                box_int(memory, int_ref, 0x10023044);

                // Allocate memory for thunks
                var thunk1_ref = exports.alloc(5*4);
                var thunk2_ref = exports.alloc(4 * 4);

                // Create thunks
                make_thunk(thunk1_ref, 0, 5, im(3), thunk2_ref);
                make_thunk(thunk2_ref, 0, 17, int_ref);

                // Add thunk1 to root set
                stack_push(thunk1_ref);


                // Run garbage collection
                exports.run_gc();

                // Check that thunk was actually copied to new heap
                assert.notEqual(thunk1_ref, stack_elem(0), "Reference to thunk 1 not updated in root set");

                thunk1_ref = stack_elem(0);
                thunk1 = new Uint32Array(memory.buffer, thunk1_ref, 5);

                // Check that thunk object properties were copied
                // correctly

                assert.equal(thunk1[0], 0, "Object type != Thunk");
                assert.equal(thunk1[1], 5, "Incorrect thunk function");
                assert.equal(thunk1[2], 2, "Incorrect closure size");

                // Check that closure was copied correctly
                assert.equal(thunk1[3], im(3), "Immediate integer not copied in closure");

                // Check that reference to thunk2 was updated in
                // closure
                assert.notEqual(thunk1[4], thunk2_ref, "Reference to thunk 2 not updated in closure");


                // Test thunk 2
                thunk2_ref = thunk1[4]
                thunk2 = new Uint32Array(memory.buffer, thunk2_ref, 4);

                // Check that thunk object properties were copied
                // correctly

                assert.equal(thunk2[0], 0, "Object type != Thunk");
                assert.equal(thunk2[1], 17, "Incorrect thunk function");
                assert.equal(thunk2[2], 1, "Incorrect closure size");


                // Check that reference to boxed integer was updated in
                // closure
                assert.notEqual(thunk2[3], int_ref, "Reference to boxed integer not updated in closure");

                // Check that boxed integer was copied

                int_ref = thunk2[3];
                assert.deepEqual(
                    new Uint32Array(memory.buffer, int_ref, 2),
                    Uint32Array.from([2, 0x10023044]),
                    "Referenced boxed integer not copied correctly"
                );
            });


            it('Should replace resolved thunk with its result (immediate values)', function() {
                // Allocate storage for resolved thunk with 1 closure
                // value
                var thunk_ref = exports.alloc(4 * 4);
                var thunk = new Uint32Array(memory.buffer, thunk_ref, 4);

                // Create thunk
                make_thunk(thunk_ref, 1, im(7), im(87));
                stack_push(thunk_ref);


                // Run garbage collection
                exports.run_gc();

                // Check that the resolved thunk was replaced with its
                // result
                assert.equal(stack_elem(0), im(7), "Thunk not replaced with result in root set");
            });

            it('Should copy result of resolved thunk', function() {
                // Create boxed integer result
                var result_ref = exports.alloc(2 * 4);
                box_int(memory, result_ref, 87006500);

                // Create thunk with no closure
                var thunk_ref = exports.alloc(3 * 4);
                make_thunk(thunk_ref, 1, result_ref);
                stack_push(thunk_ref);


                // Run garbage collection
                exports.run_gc();

                // Check that the thunk reference was replaced with a
                // reference to the boxed integer result

                assert.deepEqual(
                    new Uint32Array(memory.buffer, stack_elem(0), 2),
                    Uint32Array.from([2, 87006500]),
                    "Reference to thunk not replaced with reference to result"
                );

                assert.notEqual(result_ref, stack_elem(0), "Thunk result not copied to new heap");
            });
        });
    });
});

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
 *
 * @return The module instance.
 */
function load_runtime(table, memory) {
    return load_module(
        './runtime/runtime.wasm',
        {
            env: {
                table: table,
                memory: memory
            }
        }
    );
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

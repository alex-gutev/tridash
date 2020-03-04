/**
 * loader.js
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
 * Return the size of the memory requested by a WebAssembly Module.
 *
 * @param module The compiled WebAssembly.Module
 *
 * @return Size of the memory in bytes.
 */
function dylink_mem_size(module) {
    const sections = WebAssembly.Module.customSections(module, 'dylink');

    if (sections.length === 0)
        throw "No Dylink Section in Runtime";

    var section = new Uint8Array(sections[0], 0, sections[0].byteLength);
    return decode_u32(section, 0) + 4;
}

/**
 * Decode an unsigned 32-bit integer, encoded in LEB128 format.
 *
 * @param bytes The bytes array from which to decode the integer.
 *
 * @param offset Offset within the bytes array at which to start
 *   decoding.
 *
 * @return The decoded integer value.
 */
function decode_u32(bytes, offset) {
    var result = 0;
    var shift = 0;

    var i = offset;

    do {
        result |= (bytes[i] & 0x7F) << shift;

        i++;
        shift += 1;
    } while (bytes[i] & 0x80);

    return result;
}


/**
 * Synchronously load a Tridash WebAssembly module, along with the
 * runtime library module, from a file.
 *
 * This assumes that the 'require' function and 'fs' module are
 * present.
 *
 * @param runtime_path Path to the runtime library module.
 *
 * @param module_Path Path to the Tridash module.
 *
 * @param table_size Minimum size of the table object.
 *
 * @param memory_size Minimum size of the memory object in pages.
 *
 * @param memory_base Offset, within memory, at which the runtime
 *   module's internal data should be stored.
 *
 * @param stack_size Size of the memory region (in bytes) to reserve
 *   for the stack.
 *
 * @param num_nodes Number of nodes in the module.
 *
 * @return An object with the following properties:
 *
 *   module:  Tridash Module Instance
 *   runtime: Runtime Module Instance
 *   memory:  Memory object
 */
async function load_module_sync({
    runtime_path,
    module_path,
    table_size,
    memory_size,
    memory_base,

    stack_size,
    num_nodes,
}) {
    const fs = require('fs');

    function load_bytes(path) {
        var buf = fs.readFileSync(path);
        return new Uint8Array(buf);
    };

    var table = new WebAssembly.Table({
        element: "anyfunc",
        initial: table_size
    });

    var memory = new WebAssembly.Memory({
        initial: memory_size
    });

    var runtime = await WebAssembly.compile(load_bytes(runtime_path));
    var runtime_mem_size = dylink_mem_size(runtime);

    runtime = await WebAssembly.instantiate(
        await runtime,
        {
            env: {
                table: table,
                memory: memory,
                __memory_base: memory_base + 4,
                "g$stack_top": () => memory_base
            }
        }
    );

    runtime.exports.initialize(
        stack_size - 4,
        memory_base + runtime_mem_size,
        (memory_size * 64 * 1024) - (memory_base + runtime_mem_size)
    );

    // Reserve Space for Node Values
    var bytes = new Uint32Array(memory.buffer, memory_base, 1);
    bytes[0] = stack_size - num_nodes * 4;

    var imports = {
        runtime: {
            table: table,
            memory: memory
        }
    };

    Object.assign(imports.runtime, runtime.exports);

    var module = await WebAssembly.instantiate(
        load_bytes(module_path),
        imports
    );

    return {
        module: module.instance,
        runtime: runtime,
        memory: memory
    };
};

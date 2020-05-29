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
        shift += 7;
    } while (bytes[i++] & 0x80);

    return result;
}


/**
 * Load a Tridash WebAssembly module from a file.
 *
 * This function requires that the 'require' function and 'fs' module
 * are available.
 *
 * @param module_path Path to the Tridash module.
 * @param runtime_path Path to the runtime library module.
 *
 * @param options Load options object:
 *
 *    table_size:  Minimum size of the table object
 *
 *    memory_size: Minimum size of the memory object in pages
 *
 *    memory_base: Offset, within memory, at which the runtime
 *                 module's internal data should be stored
 *
 *    stack_size:  Size of the memory region (in bytes) to reserve
 *                 for the stack
 *
 *    imports:     Module imports object, containing additional functions
 *                 to import
 *
 * @return An object with the following properties:
 *
 *   module:     Tridash Module Instance
 *   runtime:    Runtime Module Instance
 *   memory:     Memory object
 *   marshaller: Marshaller object
 */
async function load_module_file(module_path, runtime_path, options) {
    const fs = require('fs');

    function load_bytes(path) {
        var buf = fs.readFileSync(path);
        return new Uint8Array(buf);
    };

    return load_module_bytes(load_bytes(module_path), load_bytes(runtime_path), options);
};

/**
 * Load a Tridash WebAssembly module directly from the raw bytes
 * comprising the module.
 *
 * @param module_bytes Array of bytes comprising the Tridash module.
 * @param runtime_path Array of bytes comprising the runtime library module.
 *
 * @param options Load options object:
 *
 *    table_size:  Minimum size of the table object
 *
 *    memory_size: Minimum size of the memory object in pages
 *
 *    memory_base: Offset, within memory, at which the runtime
 *                 module's internal data should be stored
 *
 *    stack_size:  Size of the memory region (in bytes) to reserve
 *                 for the stack
 *
 *    imports:     Module imports object, containing additional functions
 *                 to import
 *
 * @return An object with the following properties:
 *
 *   module:     Tridash Module Instance
 *   runtime:    Runtime Module Instance
 *   memory:     Memory object
 *   marshaller: Marshaller object
 *
 */
async function load_module_bytes(module_bytes, runtime_bytes, {
    table_size,
    memory_size,
    memory_base,
    stack_size,
    imports
}) {
    // Create WebAssembly Table and Memory objects

    var table = new WebAssembly.Table({
        element: "anyfunc",
        initial: table_size
    });

    var memory = new WebAssembly.Memory({
        initial: memory_size
    });


    // Load Runtime Library Module

    var runtime = await WebAssembly.compile(runtime_bytes);
    var runtime_mem_size = dylink_mem_size(runtime);

    runtime = await WebAssembly.instantiate(
        runtime,
        {
            env: {
                table: table,
                memory: memory,
                __memory_base: memory_base + 4,
                "g$stack_top": () => memory_base
            }
        }
    );


    // Load Tridash WebAssembly Module

    imports.runtime = {
        table: table,
        memory: memory
    };

    Object.assign(imports.runtime, runtime.exports);

    const module = await WebAssembly.instantiate(module_bytes, imports);

    return init_modules(runtime, module, {
        memory: memory,
        stack_size: stack_size,
        memory_size: memory_size,
        memory_base: memory_base,
        runtime_mem_size: runtime_mem_size
    });
}

/**
 * Initialize the loaded runtime and Tridash WebAssembly modules.
 *
 * @param runtime Runtime library WebAssembly Module
 * @param module Tridash WebAssembly Module
 *
 * @return An object with the following properties:
 *
 *   module:     Tridash Module Instance
 *   runtime:    Runtime Module Instance
 *   memory:     Memory object
 *   marshaller: Marshaller object
 */
function init_modules(runtime, module, {
    memory,
    stack_size,
    memory_size,
    memory_base,
    runtime_mem_size
}) {
    runtime.exports.__post_instantiate();

    // Initialize Garbage Collector

    runtime.exports.initialize(
        stack_size - 4,
        memory_base + runtime_mem_size,
        (memory_size * 64 * 1024) - (memory_base + runtime_mem_size)
    );

    const marshaller = new Marshaller(runtime, memory, memory_base, stack_size - 4);

    return {
        module: module.instance,
        marshaller: marshaller,
        runtime: runtime,
        memory
    };
}

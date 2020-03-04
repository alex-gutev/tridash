/**
 * test_util.js
 *
 * Tridash WebAssembly runtime library Tests.
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

const assert = require('assert');

/**
 * Resolve a linked list.
 *
 * Asserts that @a thing is a linked list node.
 *
 * @param thing The value.
 * @param n If provided, maximum number of elements to resolve.
 *
 * @return An array containing the elements of the resolved
 *   list.
 */
function resolve_list(thing, n) {
    var result = [];

    assert(thing instanceof Tridash.Marshaller.ListNode, 'Value not linked list node');

    while (thing instanceof Tridash.Marshaller.ListNode) {
        if (n === 0)
            return result;
        else if (n !== undefined)
            n--;

        thing.resolve();
        result.push(thing.head);

        thing = thing.tail;
    };

    return thing;
}

exports.resolve_list = resolve_list;

/**
 * test_util.js
 *
 * Tridash JavaScript runtime library Tests.
 *
 * Copyright 2019-2020 Alexander Gutev
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
 * Returns a promise which is resolved with the next value of @a node
 * when it is computed.
 *
 * This function adds a watch function to the node.
 *
 * @param node The Node
 *
 * @return The Promise
 */
function node_value(node) {
    var called = false;

    return new Promise((resolve) => {
        node.add_watch((value) => {
            if (!called) {
                called = true;
                resolve(Tridash.resolve(value));
            }
        });
    });
}

exports.node_value = node_value;

/**
 * Resolve a list value.
 *
 * If @a value is a list, the elements of the list are resolved.
 *
 * @param value The thunk
 * @param n If provided, maximum number of elements to resolve.
 *
 * @return An array containing the elements of the resolved
 *   list. If @a value does not resolve to a list, returns the
 *   resolved value directly.
 */
function resolve_list(value, n) {
    var result = [];

    try {
        while (Tridash.resolve(Tridash.is_cons(value))) {
            if (n === 0)
                return result;
            else if (n !== undefined)
                n--;

            result.push(Tridash.resolve(Tridash.head(value)));
            value = Tridash.resolve(Tridash.tail(value));
        }
    }
    catch (e) {
        if (e instanceof Tridash.Fail &&
            Tridash.resolve(e.type) == Tridash.Empty)
            return result;

        throw e;
    }

    return Tridash.resolve(value);
}

exports.resolve_list = resolve_list;

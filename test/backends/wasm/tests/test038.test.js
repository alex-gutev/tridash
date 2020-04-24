/**
 * test038.test.js
 *
 * Tridash WebAssembly Backend Tests
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

const Tridash = require('./tridash.min.js');
const assert = require('assert');
const util = require('./test_util.js');

const mod = require('./test038.js');

describe('Integration Test 38', function() {
    var module, input, delta, output;

    before(async () => {
        module = await mod.module;

        input = module.nodes.input;
        delta = module.nodes.delta;
        output = module.nodes.output;
    });

    describe('Higher Order Meta-Node Closures', function() {
        describe('Set `input` = [1,2,3], `delta` = 1', function () {
            it('`output` = [2,3,4]', function() {
                module.set_values([[input, [1,2,3]], [delta, 1]]);

                const value = util.resolve_list(output.get_value());
                assert.deepEqual(value, [2,3,4]);
            });
        });

        describe('Set `delta` = 10', function () {
            it('`output` = [11,12,13]', function() {
                delta.set_value(10);

                const value = util.resolve_list(output.get_value());
                assert.deepEqual(value, [11,12,13]);
            });
        });
    });
});

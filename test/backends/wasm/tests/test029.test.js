/**
 * test029.test.js
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

const mod = require('./test029.js');

describe('Integration Test 29', function() {
    var module, x, y, output;

    before(async () => {
        module = await mod.module;

        x = module.nodes.x;
        y = module.nodes.y;
        output = module.nodes.output;
    });

    describe('Local Nodes: Cyclic Bindings', function() {
        describe('Set `x` = 1, `y` = 2', function () {
            it('`output` = [1, 2, 1, ...]', function() {
                module.set_values([[x, 1], [y, 2]]);

                assert.deepEqual(output.get_value(), [1, 2, 1]);
            });
        });

        describe('Set `x` = 2, `y` = 3', function () {
            it('`output` = [2, 3, 2, ...]', function() {
                module.set_values([[x, 2], [y, 3]]);

                assert.deepEqual(output.get_value(), [2, 3, 2]);
            });
        });
    });
});

/**
 * test027.test.js
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

const mod = require('./test027.js');

describe('Integration Test 27', function() {
    var module, x, y, output1, output2, output3;

    before(async () => {
        module = await mod.module;

        x = module.nodes.x;
        y = module.nodes.y;
        output1 = module.nodes.output1;
        output2 = module.nodes.output2;
        output3 = module.nodes.output3;
    });

    describe('Rest Arguments', function() {
        describe('Set `x` = 1, `y` = 2', async function () {
            before(() => {
                module.set_values([[x, 1], [y, 2]]);
            });

            it('`output1` = 4', function() {
                assert.equal(output1.get_value(), 4);
            });
            it('`output2` = 3', function() {
                assert.equal(output2.get_value(), 3);
            });
            it('`output3` = 3', function() {
                assert.equal(output3.get_value(), 3);
            });
        });

        describe('Set `x` = 3, `y` = 7', function () {
            before(() => {
                module.set_values([[x, 3], [y, 7]]);
            });

            it('`output1` = 11', function() {
                assert.equal(output1.get_value(), 11);
            });
            it('`output2` = 10', function() {
                assert.equal(output2.get_value(), 10);
            });
            it('`output3` = 10', function() {
                assert.equal(output3.get_value(), 10);
            });
        });
    });
});

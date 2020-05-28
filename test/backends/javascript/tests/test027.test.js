/**
 * test027.test.js
 *
 * Tridash JavaScript runtime library Tests.
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

const Tridash = require('../runtime/tridash.js');
const assert = require('assert');

const mod = require('./test027.js');

const x = mod.nodes.x;
const y = mod.nodes.y;
const output1 = mod.nodes.output1;
const output2 = mod.nodes.output2;
const output3 = mod.nodes.output3;

describe('Integration Test 27', function() {
    describe('Rest Arguments', function() {
        describe('Set `x` = 1, `y` = 2', async function () {
            mod.set_values([[x, 1], [y, 2]]);

            const out1 = output1.get_value();
            const out2 = output2.get_value();
            const out3 = output3.get_value();

            it('`output1` = 4', function() {
                assert.equal(out1, 4);
            });
            it('`output2` = 3', function() {
                assert.equal(out2, 3);
            });
            it('`output3` = 3', function() {
                assert.equal(out3, 3);
            });
        });

        describe('Set `x` = 3, `y` = 7', function () {
            mod.set_values([[x, 3], [y, 7]]);

            const out1 = output1.get_value();
            const out2 = output2.get_value();
            const out3 = output3.get_value();

            it('`output1` = 11', function() {
                assert.equal(out1, 11);
            });
            it('`output2` = 10', function() {
                assert.equal(out2, 10);
            });
            it('`output3` = 10', function() {
                assert.equal(out3, 10);
            });
        });
    });
});

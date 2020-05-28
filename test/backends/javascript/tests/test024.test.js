/**
 * test024.test.js
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

const mod = require('./test024.js');
const n = mod.nodes.n;
const output1 = mod.nodes.output1;
const output2 = mod.nodes.output2;

describe('Integration Test 24', function() {
    describe('Optional Arguments', function() {
        describe('Set `n` = 1', function() {
            n.set_value(1);

            const out1 = output1.get_value();
            const out2 = output2.get_value();

            it('`output1` = 2', async function() {
                assert.equal(await out1, 2);
            });

            it('`output2` = 3', async function() {
                assert.equal(await out2, 3);
            });
        });

        describe('Set `n` = 5', function() {
            n.set_value(5);

            const out1 = output1.get_value();
            const out2 = output2.get_value();

            it('`output1` = 6', async function() {
                assert.equal(await out1, 6);
            });

            it('`output2` = 7', async function() {
                assert.equal(await out2, 7);
            });
        });
    });
});

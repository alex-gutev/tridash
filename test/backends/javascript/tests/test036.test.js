/**
 * test036.test.js
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
const util = require('./test_util.js');

const mod = require('./test036.js');

const input = mod.nodes.input;
const delta = mod.nodes.delta;
const output1 = mod.nodes.output1;
const output2 = mod.nodes.output2;

describe('Integration Test 36', function() {
    describe('Higher Order Meta-Nodes with Optional Arguments', function() {
        describe('Set `input` = [1,2,3], `delta` = 1', function () {
            mod.set_values([[input, [1,2,3]], [delta, 1]]);

            const out1 = output1.get_value();
            const out2 = output2.get_value();

            it('`output1` = [2,3,4]', function() {
               const value = util.resolve_list(out1);
               assert.deepEqual(value, [2,3,4]);
            });

            it('`output2` = [3,4,5]', function() {
                const value = util.resolve_list(out2);
                assert.deepEqual(value, [3,4,5]);
            });
        });

        describe('Set `delta` = 10', function () {
            delta.set_value(10);

            const out1 = output1.get_value();
            const out2 = output2.get_value();

            it('`output1` = [11,12,13]', function() {
               const value = util.resolve_list(out1);
               assert.deepEqual(value, [11,12,13]);
            });

            it('`output2` = [3,4,5]', function() {
                const value = util.resolve_list(out2);
                assert.deepEqual(value, [3,4,5]);
            });
        });
    });
});

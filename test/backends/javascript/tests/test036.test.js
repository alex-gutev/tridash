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

var Tridash = require('../runtime/tridash.js');
var assert = require('assert');
var util = require('./test_util.js');

require('./test036.js');
var input = Tridash.nodes.input;
var delta = Tridash.nodes.delta;
var output1 = Tridash.nodes.output1;
var output2 = Tridash.nodes.output2;

describe('Integration Test 36', function() {
    describe('Higher Order Meta-Nodes with Optional Arguments', function() {
        describe('Set `input` = [1,2,3], `delta` = 1', function () {
            Tridash.set_values([[input, [1,2,3]], [delta, 1]]);

            var out1 = output1.next_value;
            var out2 = output2.next_value;

            it('`output1` = [2,3,4]', async function() {
               var value = util.resolve_list(await out1);
               assert.deepEqual(value, [2,3,4]);
            });

            it('`output2` = [3,4,5]', async function() {
                var value = util.resolve_list(await out2);
                assert.deepEqual(value, [3,4,5]);
            });
        });

        describe('Set `delta` = 10', function () {
            delta.set_value(10);

            var out1 = output1.next_value;
            var out2 = output2.next_value;

            it('`output1` = [11,12,13]', async function() {
               var value = util.resolve_list(await out1);
               assert.deepEqual(value, [11,12,13]);
            });

            it('`output2` = [3,4,5]', async function() {
                var value = util.resolve_list(await out2);
                assert.deepEqual(value, [3,4,5]);
            });
        });
    });
});

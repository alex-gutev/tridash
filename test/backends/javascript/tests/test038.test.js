/**
 * test038.test.js
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

require('./test038.js');
var input = Tridash.nodes.input;
var delta = Tridash.nodes.delta;
var output = Tridash.nodes.output;

describe('Integration Test 38', function() {
    describe('Higher Order Meta-Node Closures', function() {
        describe('Set `input` = [1,2,3], `delta` = 1', function () {
            it('`output` = [2,3,4]', async function() {
                Tridash.set_values([[input, [1,2,3]], [delta, 1]]);

                var value = util.resolve_list(await output.next_value);
                assert.deepEqual(value, [2,3,4]);
            });
        });

        describe('Set `delta` = 10', function () {
            it('`output` = [11,12,13]', async function() {
                delta.set_value(10);

                var value = util.resolve_list(await output.next_value);
                assert.deepEqual(value, [11,12,13]);
            });
        });
    });
});

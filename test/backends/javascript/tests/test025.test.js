/**
 * test025.test.js
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

require('./test025.js');
var n = Tridash.nodes.n;
var delta = Tridash.nodes.delta;
var output1 = Tridash.nodes.output1;
var output2 = Tridash.nodes.output2;

describe('Integration Test 25', function() {
    describe('Optional Arguments with Node Default Values', function() {
        describe('Set `n` = 1, `delta` = 1', function() {
            Tridash.set_values([[n, 1], [delta, 1]]);

            var out1 = util.node_value(output1);
            var out2 = util.node_value(output2);

            it('`output1` = 2', async function() {
                assert.equal(await out1, 2);
            });

            it('`output2` = 2', async function() {
                assert.equal(await out2, 2);
            });

        });

        describe('Set `delta` = 2', function() {
            delta.set_value(2);

            var out1 = util.node_value(output1);
            var out2 = util.node_value(output2);

            it('`output1` = 3', async function() {
                assert.equal(await out1, 3);
            });

            it('`output2` = 2', async function() {
                assert.equal(await out2, 2);
            });
        });
    });
});

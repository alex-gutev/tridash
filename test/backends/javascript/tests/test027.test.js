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

var Tridash = require('../runtime/tridash.js');
var assert = require('assert');
var util = require('./test_util.js');

require('./test027.js');
var x = Tridash.nodes.x;
var y = Tridash.nodes.y;
var output1 = Tridash.nodes.output1;
var output2 = Tridash.nodes.output2;
var output3 = Tridash.nodes.output3;

describe('Integration Test 27', function() {
    describe('Rest Arguments', function() {
        describe('Set `x` = 1, `y` = 2', async function () {
            Tridash.set_values([[x, 1], [y, 2]]);

            var out1 = util.node_value(output1);
            var out2 = util.node_value(output2);
            var out3 = util.node_value(output3);

            it('`output1` = 4', async function() {
                assert.equal(await out1, 4);
            });
            it('`output2` = 3', async function() {
                assert.equal(await out2, 3);
            });
            it('`output3` = 3', async function() {
                assert.equal(await out3, 3);
            });
        });

        describe('Set `x` = 3, `y` = 7', function () {
            Tridash.set_values([[x, 3], [y, 7]]);

            var out1 = util.node_value(output1);
            var out2 = util.node_value(output2);
            var out3 = util.node_value(output3);

            it('`output1` = 11', async function() {
                assert.equal(await out1, 11);
            });
            it('`output2` = 10', async function() {
                assert.equal(await out2, 10);
            });
            it('`output3` = 10', async function() {
                assert.equal(await out3, 10);
            });
        });
    });
});

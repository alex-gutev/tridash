/**
 * test033.test.js
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

require('./test033.js');
var x = Tridash.nodes.x;
var y = Tridash.nodes.y;
var delta = Tridash.nodes.delta;
var output = Tridash.nodes.output;

describe('Integration Test 33', function() {
    describe('Outer Node References from Nested Meta-Nodes', function() {
        describe('Set `x` = 1, `y` = 2, `delta` = 1', function () {
            it('`output` = 25', async function() {
                Tridash.set_values([[x, 1], [y, 2], [delta, 1]]);

                assert.equal(await util.node_value(output), 25);
            });
        });
        
        describe('Set `delta` = 2', function () {
            it('`output` = 27', async function() {
                delta.set_value(2);

                assert.equal(await util.node_value(output), 27);
            });
        });

        describe('Set `x` = 5', function () {
            it('`output` = 31', async function() {
                x.set_value(5);

                assert.equal(await util.node_value(output), 31);
            });
        });        
    });
});

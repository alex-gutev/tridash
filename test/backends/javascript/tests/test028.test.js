/**
 * test028.test.js
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

require('./test028.js');
var x = Tridash.nodes.x;
var y = Tridash.nodes.y;
var output = Tridash.nodes.output;

describe('Integration Test 28', function() {
    describe('Local Nodes', function() {
        describe('Set `x` = 5, `y` = 9', function () {
            it('`output` = 7', async function() {
                Tridash.set_values([[x, 5], [y, 9]]);            
                assert.equal(await util.node_value(output), 7);
            });
        });

        describe('Set `x` = 2, `y` = 3', function () {
            it('`output` = 2.5', async function() {
                Tridash.set_values([[x, 2], [y, 3]]);
                assert.equal(await util.node_value(output), 2.5);
            });
        });        
    });
});

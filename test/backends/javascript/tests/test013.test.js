/**
 * test013.test.js
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

require('./test013.js');

var a = Tridash.nodes.a;
var output = Tridash.nodes.output;
var fails = Tridash.nodes.fails;

describe('Integration Test 13', function() {
    describe('Failure Types', function() {
        it('`output` is set to `a` when `a > 3`', async function() {
            a.set_value(5);
            assert.equal(await util.node_value(output), 5);
        });

        it('`fails?` is true when `a < 3`', async function() {
            a.set_value(1);
            assert(await util.node_value(fails));
        });

        it('`fails?` is false when failure not caused by `a < 3`', async function() {
            // Set a to a non-number value in order to trigger a failure
           a.set_value("hello");
           assert(!await util.node_value(fails));
        });        
    });
});

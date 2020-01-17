/**
 * test039.test.js
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

require('./test039.js');
var out1 = Tridash.nodes.out1;
var out2 = Tridash.nodes.out2;
var out3 = Tridash.nodes.out3;

describe('Core Module Integration Test 39', function() {
    describe('Macro-Node: `c`', function() {
        it('`out1` = Char("h")', async function() {
            var value = await util.node_value(out1);

            assert(value instanceof Tridash.Char);
            assert.equal(value.chr, "h");
        });


        it('`out2` = Char("\\n")', async function() {
            var value = await util.node_value(out2);

            assert(value instanceof Tridash.Char);
            assert.equal(value.chr, "\n");
        });

        it('`out3` = Char("0")', async function() {
            var value = await util.node_value(out3);

            assert(value instanceof Tridash.Char);
            assert.equal(value.chr, "0");
        });
    });
});

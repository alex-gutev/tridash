/**
 * test015.test.js
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

require('./test015.js');

var x = Tridash.nodes.x;
var y = Tridash.nodes.y;
var input = Tridash.nodes.input;
var dict = Tridash.nodes.dict;

describe('Integration Test 15', function() {
    function test_fail_type(type) {
        return (e) => {
            return (e instanceof Tridash.Fail) && Tridash.resolve(e.type) === type;
        };
    }
    
    describe('Subnodes', function() {
        it('`dict` is set to object containing fields `a` and `b`, set to `x` and `y`', async function() {
            Tridash.set_values([[x, 1], [y, 2]]);

            var value = await util.node_value(dict);

            assert.equal(Tridash.resolve(value.a), 1);
            assert.equal(Tridash.resolve(value.b), 2);
        });

        it('`x` is set to `dict.a`, and `y` to `dict.b` when `input` is set', async function() {
            input.set_value({a: "hello", b: "world"});

            assert.equal(await util.node_value(x), "hello");
            assert.equal(await util.node_value(y), "world");
        });

        it('`x` is set to a failure when `dict` set to object without `a` field', async function() {
            input.set_value({b : "world"});
            await assert.rejects(util.node_value(x), Tridash.Fail);
        });

        it('`x` and `y` are set to failures when `dict` set to non object', async function() {
            input.set_value(1);

            await assert.rejects(util.node_value(x), Tridash.Fail);
            await assert.rejects(util.node_value(y), test_fail_type(Tridash.TypeError));
        });        
    });
});

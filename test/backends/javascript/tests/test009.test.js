/**
 * test009.test.js
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

require('./test009.js');

var a = Tridash.nodes.a;
var b = Tridash.nodes.b;
var d = Tridash.nodes.d;
var output = Tridash.nodes.output;

describe('Integration Test 9', function() {
    function test_fail_type(type) {
        return (e) => {
            return (e instanceof Tridash.Fail) && Tridash.resolve(e.type) === type;
        };
    }

    describe('Explicit Contexts', function() {
        it('`output` is set to value of `a` when it does not fail', async function() {
            a.set_value("a-value");
            assert.equal(await util.node_value(output), "a-value");
        });

        it('`output` is set to value of `b` when `a` fails', async function() {
            Tridash.set_values([[a, Tridash.fail()], [b, "b-value"]]);
            assert.equal(await util.node_value(output), "b-value");
        });

        it('`output` is set to value of `d` when `a` and `b` fail', async function() {
            Tridash.set_values([[b, Tridash.fail('x')], [d, "hello"]]);
            assert.equal(await util.node_value(output), "hello");
        });

        it('`output` is set to value of `d` when `a` and `b` fail', async function() {
            Tridash.set_values([[b, Tridash.fail('b')], [d, "hello"]]);
            assert.equal(await util.node_value(output), "hello");
        });

        it('`output` is set to failure value of `d` when `a`,`b` and `d` fail', async function() {
            d.set_value(Tridash.fail('d'));
            await assert.rejects(util.node_value(output), test_fail_type('d'));
        });
    });
});

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

const Tridash = require('../runtime/tridash.js');
const assert = require('assert');
const util = require('./test_util.js');

const mod = require('./test009.js');

const a = mod.nodes.a;
const b = mod.nodes.b;
const d = mod.nodes.d;
const output = mod.nodes.output;

describe('Integration Test 9', function() {
    describe('Explicit Contexts', function() {
        describe('Set `a` = "a-value"', function() {
            it('`output` = `a` = "a-value"', function() {
                a.set_value("a-value");
                assert.equal(output.get_value(), "a-value");
            });
        });

        describe('Set `a` = failure', function() {
            it('`output` = `b` = "b-value"', function() {
                mod.set_values([[a, Tridash.fail()], [b, "b-value"]]);
                assert.equal(output.get_value(), "b-value");
            });
        });

        describe('Set `b` = fail("x"), `d` = "hello"', function() {
            it('`output` = `a` = "hello"', function() {
                mod.set_values([[b, Tridash.fail('x')], [d, "hello"]]);
                assert.equal(output.get_value(), "hello");
            });
        });

        describe('Set `b` = fail("b"), `d` = "hello"', function() {
            it('`output` = `d` = "hello"', function() {
                mod.set_values([[b, Tridash.fail('b')], [d, "hello"]]);
                assert.equal(output.get_value(), "hello");
            });
        });

        describe('Set `d` = fail("d")', function() {
            it('`output` = `d` = fail("d")', function() {
                d.set_value(Tridash.fail('d'));
                assert.throws(() => output.get_value(), util.test_fail_type('d'));
            });
        });
    });
});

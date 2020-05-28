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

const Tridash = require('../runtime/tridash.js');
const assert = require('assert');
const util = require('./test_util.js');

const mod = require('./test015.js');

const x = mod.nodes.x;
const y = mod.nodes.y;
const input = mod.nodes.input;
const dict = mod.nodes.dict;

describe('Integration Test 15', function() {
    describe('Subnodes', function() {
        describe('Set `x` = 1, `y` = 2', function() {
            it('`dict` = `{ a : 1, b : 2 }`', function() {
                mod.set_values([[x, 1], [y, 2]]);

                const value = dict.get_value();

                assert.equal(Tridash.resolve(value.a), 1);
                assert.equal(Tridash.resolve(value.b), 2);
            });
        });

        describe('Set `dict` = `{a : "hello", b : "world" }`', function() {
            it('`x` = `dict.a` = "hello, `y` = `dict.b` = "world""', function() {
                input.set_value({a: "hello", b: "world"});

                assert.equal(x.get_value(), "hello");
                assert.equal(y.get_value(), "world");
            });
        });

        describe('Set `dict` = `{ b : "world" }`', function() {
            it('`x` = `dict.a` = fail', function() {
                input.set_value({b : "world"});
                assert.throws(() => x.get_value(), Tridash.Fail);
            });
        });

        describe('Set `dict` = 1', function() {
            it('`x` = `y` = Type Error Failure', function() {
                input.set_value(1);

                assert.throws(() => x.get_value(), Tridash.Fail);
                assert.throws(() => y.get_value(), util.test_fail_type(Tridash.TypeError()));
            });
        });
    });
});

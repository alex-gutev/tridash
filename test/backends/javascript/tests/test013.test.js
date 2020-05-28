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

const Tridash = require('../runtime/tridash.js');
const assert = require('assert');

const mod = require('./test013.js');

const a = mod.nodes.a;
const output = mod.nodes.output;
const fails = mod.nodes.fails;

describe('Integration Test 13', function() {
    describe('Failure Types', function() {
        describe('Set `a` = 5', function() {
            it('`output` = `a` = 5', function() {
                a.set_value(5);
                assert.equal(output.get_value(), 5);
            });
        });

        describe('Set `a` = 1', function() {
            it('`fails?` is true', function() {
                a.set_value(1);
                assert(fails.get_value());
            });
        });

        describe('Set `a` = "hello"', function() {
            it('`fails?` is false', function() {
                // Set a to a non-number value in order to trigger a failure
                a.set_value("hello");
                assert(!fails.get_value());
            });
        });
    });
});

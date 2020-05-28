/**
 * test003.test.js
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

const mod = require('./test003.js');

const a = mod.nodes.a;
const b = mod.nodes.b;
const sum = mod.nodes.sum;

describe('Integration Test 3', function() {
    describe('Functor Nodes', function() {
        describe('Set `a` = 1, `b` = 2', function() {
            it('`sum` = `a + b` = 3', function() {
                a.set_value(1);
                b.set_value(2);

                assert.equal(sum.get_value(), 3);
            });
        });

        describe('Set `a` = 7, `b` = 4', function() {
            it('`sum` = `a + b` = 11', function() {
                a.set_value(7);
                b.set_value(4);

                assert.equal(sum.get_value(), 11);
            });
        });
    });
});

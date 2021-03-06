/**
 * test002.test.js
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

const mod = require('./test002.js');

const x = mod.nodes.x;
const z = mod.nodes.z;
const w = mod.nodes.w;

describe('Integration Test 2', function() {
    describe('Simple Two-Way Bindings', function() {
        describe('Set `x` = 1', function() {
            before(() => {
                x.set_value(1);
            });

            it('`z` = 1', function() {
                assert.equal(z.get_value(), 1);
            });

            it('`w` = 1', function() {
                assert.equal(w.get_value(), 1);
            });
        });

        describe('Set `x` = 15', function() {
            before(() => {
                z.set_value(15);
            });

            it('`z` = 15', function() {
                assert.equal(x.get_value(), 15);
            });

            it('`w` = 15', function() {
                assert.equal(w.get_value(), 15);
            });
        });
    });
});

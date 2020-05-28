/**
 * test001.test.js
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

const mod = require('./test001.js');

const a = mod.nodes.a;
const out1 = mod.nodes.out1;
const out2 = mod.nodes.out2;

describe('Integration Test 1', function() {
    describe('Simple One-Way Binding', function() {
        describe('Set `a` = 1', function() {
            before(() => {
                a.set_value(1);
            });

            it('`out1` = 1', function() {
                assert.equal(out1.get_value(), 1);
            });

            it('`out2` = 1', function() {
                assert.equal(out2.get_value(), 1);
            });
        });

        describe('Set `a` = 14', function() {
            before(() => {
                a.set_value(14);
            });

            it('`out1` = 14', function() {
                assert.equal(out1.get_value(), 14);
            });

            it('`out2` = 14', function() {
                assert.equal(out2.get_value(), 14);
            });
        });
    });
});

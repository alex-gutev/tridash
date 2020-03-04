/**
 * test001.test.js
 *
 * Tridash WebAssembly Backend Tests
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

const Tridash = require('./tridash.min.js');
const assert = require('assert');

const mod = require('./test001.js');

describe('Integration Test 1', async function() {
    var module, a, out1, out2;

    before(async () => {
        module = await mod.module;

        a = module.nodes.a;
        out1 = module.nodes.out1;
        out2 = module.nodes.out2;
    });

    describe('Simple One-Way Binding', function() {
        describe('Set `a` = 23', function() {
            before(() => {
                a.set_value(23);
            });

            it('`out1` = 23', async function() {
                assert.equal(out1.get_value(), 23);
            });

            it('`out2` = 23', async function() {
                assert.equal(out2.get_value(), 23);
            });
        });

        describe('Set `a` = 567', function() {
            before(() => {
                a.set_value(567);
            });

            it('`out1` = 567', async function() {
                assert.equal(out1.get_value(), 567);
            });

            it('`out2` = 567', async function() {
                assert.equal(out2.get_value(), 567);
            });
        });

        describe('Set `a` = 2^30 + 1267', function() {
            const value = Math.pow(2, 30) + 1267;

            before(() => {
                a.set_value(value);
            });

            it('`out1` = 2^30 + 1267', async function() {
               assert.equal(out1.get_value(), value);
            });

            it('`out2` = 2^30 + 1267', async function() {
                assert.equal(out2.get_value(), value);
            });
        });

        describe('Set `a` = 20.125', function() {
            before(() => {
                a.set_value(20.125);
            });

            it('`out1` = 20.125', async function() {
               assert.equal(out1.get_value(), 20.125);
            });

            it('`out2` = 20.125', async function() {
                assert.equal(out2.get_value(), 20.125);
            });
        });

        describe('Set `a` = "hello"', function() {
            before(() => {
                a.set_value("hello");
            });

            it('`out1` = "hello"', async function() {
               assert.equal(out1.get_value(), "hello");
            });

            it('`out2` = "hello"', async function() {
                assert.equal(out2.get_value(), "hello");
            });
        });
    });
});

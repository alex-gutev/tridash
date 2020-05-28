/**
 * test003.test.js
 *
 * Tridash WebAssembly Backend Tests.
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

const mod = require('./test003.js');

describe('Integration Test 3', async function() {
    var module, a, b, sum;

    before(async () => {
        module = await mod.module;

        a = module.nodes.a;
        b = module.nodes.b;
        sum = module.nodes.sum;
    });

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

        describe('Set `a` = 2**30, `b` = 1236', function() {
            it('`sum` = `a + b` = 2**30 + 1236', function() {
                const val_a = Math.pow(2, 30);

                a.set_value(val_a);
                b.set_value(1236);

                assert.equal(sum.get_value(), val_a + 1236);
            });
        });

        describe('Set `a` = 12.5, `b` = 120', function() {
            it('`sum` = 132.5', function() {
                a.set_value(12.5);
                b.set_value(120);

                assert.equal(sum.get_value(), 132.5);
            });
        });

        describe('Set `a` = 23.25, `b` = 120.25', function() {
            it('`sum` = 143.5', function() {
                a.set_value(23.25);
                b.set_value(120.25);

                assert.equal(sum.get_value(), 143.5);
            });
        });
    });
});

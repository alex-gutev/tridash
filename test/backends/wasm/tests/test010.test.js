/**
 * test010.test.js
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

const mod = require('./test010.js');

describe('Integration Test 10', function() {
    var module, a, b, d, output;

    before(async () => {
        module = await mod.module;

        a = module.nodes.a;
        b = module.nodes.b;
        d = module.nodes.d;
        output = module.nodes.output;
    });

    describe('Failure Propagation', function() {
        describe('Set `a` = 1, `b` = 1.5, `d` = 2', function() {
            it('`output` = `a + b * d` = 4', function() {
                module.set_values([[a, 1], [b, 1.5], [d, 2]]);
                assert.equal(output.get_value(), 4);
            });
        });

        describe('Set `d` = "x"', function() {
            it('`output` = fail', function() {
                d.set_value("x");

                assert(output.get_value() instanceof Tridash.Marshaller.Fail);
            });
        });
    });
});

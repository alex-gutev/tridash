/**
 * test004.test.js
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

const mod = require('./test004.js');

describe('Integration Test 4', function() {
    var module, a, b, output;

    before(async () => {
        module = await mod.module;

        a = module.nodes.a;
        b = module.nodes.b;
        output = module.nodes.output;
    });

    describe('Multiple Node Contexts', function() {
        describe('Set `a` = 1', function() {
            it('`output` = 1', function() {
                a.set_value(1);
                assert.equal(output.get_value(), 1);
            });
        });

        describe('Set `b` = 4', function() {
            it('`output` = 4', function() {
                b.set_value(4);
                assert.equal(output.get_value(), 4);
            });
        });
    });
});

/**
 * test011.test.js
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

const mod = require('./test011.js');

describe('Integration Test 11', function() {
    var module, a, output;

    before(async () => {
        module = await mod.module;

        a = module.nodes.a;
        output = module.nodes.output;
    });

    describe('Conditional Bindings', function() {
        describe('Set `a` = 5', function() {
            it('`output` = 5', function() {
                a.set_value(5);

                assert.equal(output.get_value(), 5);
            });
        });

        describe('Set `a` = 7.5', function() {
            it('`output` = 7.5', function() {
                a.set_value(7.5);

                assert.equal(output.get_value(), 7.5);
            });
        });

        describe('Set `a` = 1', function() {
            it('`output` = fail', function() {
                a.set_value(1);
                assert(output.get_value() instanceof Tridash.Marshaller.Fail);
            });
        });

        describe('Set `a` = 0.125', function() {
            it('`output` = fail', function() {
                a.set_value(0.125);
                assert(output.get_value() instanceof Tridash.Marshaller.Fail);
            });
        });
    });
});

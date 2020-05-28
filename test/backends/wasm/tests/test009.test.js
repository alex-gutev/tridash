/**
 * test009.test.js
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

const mod = require('./test009.js');

describe('Integration Test 9', function() {
    var module, a, b, d, output;

    before(async () => {
        module = await mod.module;

        a = module.nodes.a;
        b = module.nodes.b;
        d = module.nodes.d;
        output = module.nodes.output;
    });

    describe('Explicit Contexts', function() {
        describe('Set `a` = "a-value"', function() {
            it('`output` = `a` = "a-value"', function() {
                a.set_value("a-value");
                assert.equal(output.get_value(), "a-value");
            });
        });

        describe('Set `a` = failure', function() {
            it('`output` = `b` = "b-value"', function() {
                module.set_values([[a, new Tridash.Marshaller.Fail(0)], [b, "b-value"]]);
                assert.equal(output.get_value(), "b-value");
            });
        });

        describe('Set `b` = fail("x"), `d` = "hello"', function() {
            it('`output` = `a` = "hello"', function() {
                module.set_values([[b, new Tridash.Marshaller.Fail('x')], [d, "hello"]]);
                assert.equal(output.get_value(), "hello");
            });
        });

        describe('Set `b` = fail("b"), `d` = "hello"', function() {
            it('`output` = `d` = "hello"', function() {
                module.set_values([[b, new Tridash.Marshaller.Fail('b')], [d, "hello"]]);
                assert.equal(output.get_value(), "hello");
            });
        });

        describe('Set `d` = fail("d")', function() {
            it('`output` = `d` = fail("d")', function() {
                d.set_value(new Tridash.Marshaller.Fail('d'));

                assert.deepStrictEqual(
                    output.get_value(),
                    new Tridash.Marshaller.Fail('d')
                );
            });
        });
    });
});

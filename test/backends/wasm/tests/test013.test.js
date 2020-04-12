/**
 * test013.test.js
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

const mod = require('./test013.js');

describe('Integration Test 13', function() {
    var module, a, output, fails;

    before(async () => {
        module = await mod.module;

        a = module.nodes.a;
        output = module.nodes.output;

        fails = module.nodes.fails;
    });

    describe('Failure Types', function() {
        describe('Set `a` = 5', function() {
            it('`output` = `a` = 5', function() {
                a.set_value(5);
                assert.equal(output.get_value(), 5);
            });
        });

        describe('Set `a` = 1', function() {
            before(() => {
                a.set_value(1);
            });

            it('`fails?` is true', function() {
                assert.strictEqual(fails.get_value(), true);
            });

            it('`output` = fail("my-type")', function() {
                assert.deepStrictEqual(
                    output.get_value(),
                    new Tridash.Marshaller.Fail("my-type")
                );
            });
        });

        describe('Set `a` = "hello"', function() {
            it('`fails?` is false', function() {
                // Set a to a non-number value in order to trigger a failure
                a.set_value("hello");
                assert.strictEqual(fails.get_value(), false);
            });
        });
    });
});

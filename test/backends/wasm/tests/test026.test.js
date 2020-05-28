/**
 * test026.test.js
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
const util = require('./test_util.js');

const mod = require('./test026.js');

describe('Integration Test 26', function() {
    var module, n, output1, output2;

    before(async () => {
        module = await mod.module;

        n = module.nodes.n;
        output1 = module.nodes.output1;
        output2 = module.nodes.output2;
    });

    describe('Optional Arguments with No Default Values', function() {
        describe('Set `n` = 1', function() {
            before(() => {
                n.set_value(1);
            });

            it('`output1` fails with Type No-Value', function() {
                assert.deepStrictEqual(
                    output1.get_value(),
                    new Tridash.Marshaller.Fail(
                        Tridash.Marshaller.FailTypes.NoValue
                    )
                );
            });

            it('`output2` = 2', function() {
                assert.equal(output2.get_value(), 2);
            });
        });

        describe('Set `n` = 2', function() {
            before(() => {
                n.set_value(2);
            });

            it('`output1` fails with type No-Value', function() {
                assert.deepStrictEqual(
                    output1.get_value(),
                    new Tridash.Marshaller.Fail(
                        Tridash.Marshaller.FailTypes.NoValue
                    )
                );
            });

            it('`output2` = 3', function() {
                assert.equal(output2.get_value(), 3);
            });
        });
    });
});

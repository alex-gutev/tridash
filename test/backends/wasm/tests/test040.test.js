/**
 * test040.test.js
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

const mod = require('./test040.js');

describe('Integration Test 40', function() {
    var module, in_x, in_y, in_z, output;

    before(async () => {
        module = await mod.module;

        in_x = module.nodes.in_x;
        in_y = module.nodes.in_y;
        in_z = module.nodes.in_z;

        output = module.nodes.output;
    });

    describe('Handling Failures with Explicit Contexts', function() {
        describe('Set `in_x` = "1", `in_y` = "2", `in_z` = "3"', function() {
            before(() => {
                module.set_values([[in_x, "1"], [in_y, "2"], [in_z, "3"]]);
            });

            it('`output` = 1', function() {
                assert.strictEqual(output.get_value(), 1);
            });
        });

        describe('Set `in_x` = "foo"', function() {
            before(() => {
                in_x.set_value("foo");
            });

            it('`output` = `in_y` = 2', function() {
                assert.equal(output.get_value(), 2);
            });
        });

        describe('Set `in_y` = "-1"', function() {
            before(() => {
                in_y.set_value("-1");
            });

            it('`output` = `in_z` = 3', function() {
                assert.equal(output.get_value(), 3);
            });
        });

        describe('Set `in_y` = "10"', function() {
            before(() => {
                in_y.set_value("10");
            });

            it('`output` = `in_y` = 10', function() {
                assert.equal(output.get_value(), 10);
            });
        });

        describe('Set `in_x` = "-5"', function() {
            before(() => {
                in_x.set_value("-5");
            });

            it('`output` = `in_z` = 3', function() {
                assert.equal(output.get_value(), 3);
            });
        });
    });
});

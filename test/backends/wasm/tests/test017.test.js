/**
 * test017.test.js
 *
 * Tridash WebAssembly Backend Tests
 *
 * Copyright 2019-2020 Alexander Gutev
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

const mod = require('./test017.js');

describe('Integration Test 17', function() {
    var module, counter, start, delta, clicked;

    before(async () => {
        module = await mod.module;

        counter = module.nodes.counter;
        start = module.nodes.start;
        delta = module.nodes.delta;
        clicked = module.nodes['clicked?'];
    });

    describe('Explicit States Counter With Delta', function() {
        describe('Set Initial Counter Value = 0', function() {
            it('`counter` = 0', function() {
                start.set_value(0);
                assert.equal(counter.get_value(), 0);
            });
        });

        describe('Incrementing Counter', function() {
            describe('Set `clicked` = true', function() {
                it('`counter` = 1', function() {
                    delta.set_value(1);
                    clicked.set_value(true);

                    assert.equal(counter.get_value(), 1);
                });
            });

            describe('Set `clicked` = false', function() {
                it('`counter` = 1', function() {
                    clicked.set_value(false);
                    assert.equal(counter.get_value(), 1);
                });
            });

            describe('Set `clicked` = true', function() {
                it('`counter` = 2', function() {
                    clicked.set_value(true);
                    assert.equal(counter.get_value(), 2);
                });
            });
        });

        describe('Changing Delta (Set `delta` = 10)', function() {
            it('`counter` = 2', function() {
                delta.set_value(10);
                clicked.set_value(false);

                assert.equal(counter.get_value(), 2);
            });

            describe('Set `clicked` = true', function() {
                it('`counter` = 2 + 10 = 12', function() {
                    clicked.set_value(true);
                    assert.equal(counter.get_value(), 12);
                });
            });
        });

        describe('Resetting Counter Value (Set `start` = 5)', function() {
            it('`counter` = 5', function() {
                start.set_value(5);
                assert.equal(counter.get_value(), 5);
            });
        });
    });
});

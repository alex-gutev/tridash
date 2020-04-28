/**
 * test041.test.js
 *
 * Tridash JavaScript runtime library Tests.
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

const Tridash = require('../runtime/tridash.js');
const assert = require('assert');

const mod = require('./test041.js');

const x = mod.nodes.x;
const y = mod.nodes.y;
const z = mod.nodes.z;

const output1 = mod.nodes.output1;
const output2 = mod.nodes.output2;

describe('Integration Test 41', function() {
    describe('Interfacing with JavaScript', function() {
        describe('Node Value Change Callbacks', function() {
            var x_changed = 0;
            var last_x;

            var output1_changed = 0;
            var last_output1;

            function watch_x(value) {
                x_changed++;
                last_x = Tridash.resolve(value);
            }

            function watch_output1(value) {
                output1_changed++;
                last_output1 = Tridash.resolve(value);
            }

            x.watch(watch_x);
            output1.watch(watch_output1);

            describe('Callbacks for `x` and `output1`', function() {
                before(() => {
                    mod.set_values([[x, 1], [y, 2], [z, 3]]);
                });

                it('`x` callback called', function() {
                    assert.equal(x_changed, 1);
                    assert.equal(last_x, 1);
                });

                it('`output1` callback called', function() {
                    assert.equal(output1_changed, 1);
                    assert.equal(last_output1, 3);
                });
            });

            describe('Change `z`', function() {
                before(() => z.set_value(5));

                it('Callback for `x` not called', function() {
                    assert.equal(x_changed, 1);
                    assert.equal(last_x, 1);
                });

                it('Callback for `output1` not called', function() {
                    assert.equal(output1_changed, 1);
                    assert.equal(last_output1, 3);
                });
            });

            describe('Add Callback for `output2` and second `x` callback', function() {
                var x2_changed = 0;
                var last_x2;

                var output2_changed = 0;
                var last_output2;

                function watch_x2(value) {
                    x2_changed++;
                    last_x2 = Tridash.resolve(value);
                }

                function watch_output2(value) {
                    output2_changed++;
                    last_output2 = Tridash.resolve(value);
                }

                before(() => {
                    x.watch(watch_x2);
                    output2.watch(watch_output2);

                    mod.set_values([[x, 5], [y, 10], [z, 3]]);
                });

                it('First Callback for `x` called', function() {
                    assert.equal(x_changed, 2);
                    assert.equal(last_x, 5);
                });

                it('Second Callback for `x` called', function() {
                    assert.equal(x2_changed, 1);
                    assert.equal(last_x2, 5);
                });

                it('Callback for `output2` called', function() {
                    assert.equal(output2_changed, 1);
                    assert.equal(last_output2, 7);
                });
            });
        });
    });
});

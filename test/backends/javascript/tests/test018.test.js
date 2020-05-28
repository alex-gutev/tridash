/**
 * test018.test.js
 *
 * Tridash JavaScript runtime library Tests.
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

const Tridash = require('../runtime/tridash.js');
const assert = require('assert');

const mod = require('./test018.js');

const counter = mod.nodes.counter;
const start = mod.nodes.start;
const state = mod.nodes.state;

describe('Integration Test 18', function() {
    describe('Counter Using Explicit State Transitions', function() {
        describe('Set Initial Counter Value = 0', function() {
            it('`counter` = 0', function() {
                start.set_value(0);
                assert.equal(counter.get_value(), 0);
            });
        });

        describe('Incrementing Counter', function() {
            describe('Set Counter State = `state1` (1)', function() {
                it('`counter` = 0', function() {
                    state.set_value(1);
                    assert.equal(counter.get_value(), 0);
                });
            });

            describe('Set Counter State = `state2`', function() {
                it('`counter` = 1', function() {
                    state.set_value(2);
                    assert.equal(counter.get_value(), 1);
                });
            });

            describe('Set Counter State = `default`', function() {
                it('`counter` = 1', function() {
                    state.set_value(3);
                    assert.equal(counter.get_value(), 1);
                });
            });

            describe('Set Counter State = `state2`', function() {
                it('`counter` = 1', function() {
                    state.set_value(2);
                    assert.equal(counter.get_value(), 1);
                });
            });

            describe('Set Counter State = `state1`', function() {
                it('`counter` = 1', function() {
                    state.set_value(1);
                    assert.equal(counter.get_value(), 1);
                });
            });

            describe('Set Counter State = `state2`', function() {
                it('`counter` = 2', function() {
                    state.set_value(2);
                    assert.equal(counter.get_value(), 2);
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

/**
 * states3.test.js
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

var Tridash = require('../runtime/tridash.js');
var assert = require('assert');
var util = require('./test_util.js');

require('./states3.js');

var counter = Tridash.nodes['counter'];
var start = Tridash.nodes['start'];
var state = Tridash.nodes['state'];

describe('Integration Test: Counter Using Explicit State Transitions', function() {
    describe('Setting Initial Counter Value', function() {
        it('Should set initial value of counter to 0', async function() {
            start.set_value(0);
            assert.equal(await util.node_value(counter), 0);
        });
    });

    describe('Incrementing Counter Value', function() {
        it('Should retain value when changing from `default` to `state1`', async function() {
            state.set_value(1);
            assert.equal(await util.node_value(counter), 0);
        });

        it('Should increment when changing from `state1` to `state2`', async function() {
            state.set_value(2);
            assert.equal(await util.node_value(counter), 1);
        });

        it('Should retain value when changing back to `default`', async function() {
            state.set_value(3);
            assert.equal(await util.node_value(counter), 1);
        });

        it('Should retain value when changing from `default` to `state2`', async function() {
            state.set_value(2);
            assert.equal(await util.node_value(counter), 1);
        });

        it('Should retain value when changing from `state2` to `state1`', async function() {
            state.set_value(1);
            assert.equal(await util.node_value(counter), 1);
        });

        it('Should increment when changing back to `state2` from `state1`', async function() {
            state.set_value(2);
            assert.equal(await util.node_value(counter), 2);
        });

    });

    describe('Resetting Counter Value', function() {
        it('Should reset counter value when changing start', async function() {
            start.set_value(5);
            assert.equal(await util.node_value(counter), 5);
        });
    });
});

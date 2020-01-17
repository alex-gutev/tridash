/**
 * test023.test.js
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

var Tridash = require('../runtime/tridash.js');
var assert = require('assert');
var util = require('./test_util.js');

require('./test023.js');

var n = Tridash.nodes.n;
var output = Tridash.nodes.output;

describe('Integration Test 23', function() {
    describe('Mutually Recursive Meta-Nodes', function() {
        describe('Set `n` = 1', function() {
            it('`output` = 1', async function() {
                n.set_value(1);
                assert.equal(await util.node_value(output), 1);
            });
        });

        describe('Set `n` = 2', function() {
            it('`output` = 2', async function() {
                n.set_value(2);
                assert.equal(await util.node_value(output), 2);
            });
        });

        describe('Set `n` = 3', function() {
            it('`output` = 3', async function() {
                n.set_value(3);
                assert.equal(await util.node_value(output), 3);
            });
        });

        describe('Set `n` = 5', function() {
            it('`output` = 8', async function() {
                n.set_value(5);
                assert.equal(await util.node_value(output), 8);
            });
        });
    });
});

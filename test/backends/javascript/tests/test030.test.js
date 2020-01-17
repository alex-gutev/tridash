/**
 * test030.test.js
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

require('./test030.js');
var name = Tridash.nodes.name;
var output = Tridash.nodes.output;

describe('Integration Test 30', function() {
    describe('Self Node Bindings', function() {
        describe('Set `name` to "John"', function () {
            it('`output` = { first: "John"; last: "Smith" }', async function() {
                name.set_value("John");
                var value = await util.node_value(output);

                assert.equal(Tridash.resolve(value.first), "John");
                assert.equal(Tridash.resolve(value.last), "Smith");
            });
        });
        
        describe('Set `name` to "Bob"', function () {
            it('`output` = { first: "Bob"; last: "Smith" }', async function() {
                name.set_value("Bob");
                var value = await util.node_value(output);

                assert.equal(Tridash.resolve(value.first), "Bob");
                assert.equal(Tridash.resolve(value.last), "Smith");
            });
        });
    });
});

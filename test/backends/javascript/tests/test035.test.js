/**
 * test035.test.js
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

require('./test035.js');
var input = Tridash.nodes.input;
var f = Tridash.nodes.f;
var output = Tridash.nodes.output;
var output2 = Tridash.nodes.output2;

describe('Integration Test 35', function() {
    function test_fail_type(type) {
        return (e) => {
            return (e instanceof Tridash.Fail) && Tridash.resolve(e.type) === type;
        };
    }

    describe('Higher Order Meta-Nodes', function() {
        describe('Set `input` = [1,2,3]', function () {
            it('`output` = [2,3,4]', async function() {
                input.set_value([1, 2, 3]);
                var value = util.resolve_list(await output.next_value);
                assert.deepEqual(value, [2, 3, 4]);
            });
        });

        describe('Set `input` = [1,1,1]', function () {
            it('`output` = [2,2,2]', async function() {
                input.set_value([1, 1, 1]);
                var value = util.resolve_list(await output.next_value);
                assert.deepEqual(value, [2, 2, 2]);
            });
        });


        describe('Type Errors', function() {
            describe('Set `f` = 1', function() {
                it('`output2` fails with type Tridash.TypeError', async function() {
                    f.set_value(1);

                    await assert.rejects(util.node_value(output2), test_fail_type(Tridash.TypeError));
                });
            });

            describe('Set `f` = "hello"', function() {
                it('`output2` fails with type Tridash.TypeError', async function() {
                    f.set_value("hello");

                    await assert.rejects(util.node_value(output2), test_fail_type(Tridash.TypeError));
                });
            });

            describe('Set `f` = { x : 1, y : 2 }', function() {
                it('`output2` fails with type Tridash.TypeError', async function() {
                    f.set_value({x : 1, y : 2});

                    await assert.rejects(util.node_value(output2), test_fail_type(Tridash.TypeError));
                });
            });
        });
    });
});

/**
 * test026.test.js
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

require('./test026.js');
var n = Tridash.nodes.n;
var output1 = Tridash.nodes.output1;
var output2 = Tridash.nodes.output2;

describe('Integration Test 26', function() {
    function test_fail_type(type) {
        return (e) => {
            return (e instanceof Tridash.Fail) && Tridash.resolve(e.type) === type;
        };
    }

    describe('Optional Arguments with No Default Values', function() {
        describe('Set `n` = 1', function() {
            n.set_value(1);

            var out1 = util.node_value(output1);
            var out2 = util.node_value(output2);

            it('`output1` fails', async function() {
                await assert.rejects(out1, test_fail_type(Tridash.NoValue));
            });

            it('`output2` = 2', async function() {
                assert.equal(await out2, 2);
            });
        });

        describe('Set `n` = 2', function() {
            n.set_value(2);

            var out1 = util.node_value(output1);
            var out2 = util.node_value(output2);

            it('`output1` fails', async function() {
                await assert.rejects(out1, test_fail_type(Tridash.NoValue));
            });

            it('`output2` = 3', async function() {
                assert.equal(await out2, 3);
            });
        });
    });
});

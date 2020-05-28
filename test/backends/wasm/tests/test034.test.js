/**
 * test034.test.js
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

var marshaller;

global.external_inc = function(n) {
    n = marshaller.to_js(marshaller.resolve(n));

    return marshaller.to_tridash(n + 1);
};

global.Lib = {
    decrement: function(n) {
        n = marshaller.to_js(marshaller.resolve(n));

        return marshaller.to_tridash(n - 1);
    }
};

const Tridash = require('./tridash.min.js');
const assert = require('assert');

const mod = require('./test034.js');

describe('Integration Test 34', function() {
    var module, x, output1, output2;

    before(async () => {
        module = await mod.module;

        marshaller = module.module.marshaller;

        x = module.nodes.x;
        output1 = module.nodes.output1;
        output2 = module.nodes.output2;
    });

    describe('External Meta-Nodes', function() {
        describe('Set `x` = 1', function () {
            before(() => {
                x.set_value(1);
            });

            it('`output1` = 2', function() {
                assert.equal(output1.get_value(), 2);
            });

            it('`output2` = 0', function() {
                assert.equal(output2.get_value(), 0);
            });
        });

        describe('Set `x` = 2', function () {
            before(() => {
                x.set_value(2);
            });

            it('`output1` = 3', function() {
                assert.equal(output1.get_value(), 3);
            });

            it('`output2` = 1', function() {
                assert.equal(output2.get_value(), 1);
            });
        });
    });
});

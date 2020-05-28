/**
 * test039.test.js
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

const mod = require('./test039.js');

describe('Core Module Integration Test 39', function() {
    var module, out1, out2, out3;

    before(async () => {
        module = await mod.module;

        out1 = module.nodes.out1;
        out2 = module.nodes.out2;
        out3 = module.nodes.out3;
    });

    describe('Macro-Node: `c`', function() {
        it('`out1` = Char("h")', function() {
            assert.deepStrictEqual(
                out1.get_value(),
                new Tridash.Marshaller.Char("h".charCodeAt(0))
            );
        });

        it('`out2` = Char("\\n")', function() {
            assert.deepStrictEqual(
                out2.get_value(),
                new Tridash.Marshaller.Char("\n".charCodeAt(0))
            );
        });

        it('`out3` = Char("0")',  function() {
            assert.deepStrictEqual(
                out3.get_value(),
                new Tridash.Marshaller.Char("0".charCodeAt(0))
            );
        });
    });
});

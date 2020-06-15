/**
 * core.test.js
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
const util = require('./test_util.js');

const mod = require('./core.js');

function run_tridash_tests(tests) {
    if (!Array.isArray(tests))
        tests = util.resolve_list(tests);

    tests.forEach(run_tridash_test);
}

function run_tridash_test(test) {
    function run_test(condition) {
        if (condition instanceof Tridash.Marshaller.Fail) {
            assert.fail("Evaluation of test condition failed");
        }
        else {
            assert.strictEqual(condition, true);
        }
    }

    if (test instanceof Tridash.Marshaller.Object) {
        var type = test.subnodes.type;

        if (!(type instanceof Tridash.Marshaller.Symbol)) {
            assert.fail("Test type not a symbol");
        }

        type = type.name;

        if (type === 'group') {
            describe(test.subnodes.description, function() {
                run_tridash_tests(test.subnodes.tests);
            });
        }
        else if (type == 'test') {
            it(test.subnodes.description, function() {
                run_test(test.subnodes.condition);
            });
        }
        else {
            assert.fail("Unrecognized test type: " + type);
        }
    }
    else {
        assert.fail("Not a test object: " + test);
    }
}

describe('Core Module Tests', function() {
    var module;

    before(async () => {
        module = await mod.module;
        marshaller = module.memory;
    });

    it('Core Tests Pass', () => {
        const tests = module.nodes.core_tests.get_value();
        run_tridash_test(tests);
    });
});

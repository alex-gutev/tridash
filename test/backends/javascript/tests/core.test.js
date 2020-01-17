/**
 * core.test.js
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

require('./core.js');

function run_tridash_tests(tests) {
    util.list_to_array(tests).forEach(run_tridash_test);
}

function run_tridash_test(test) {
    function run_test(condition) {
        try {
            assert(Tridash.resolve(condition));
        } catch (e) {
            if (e instanceof Tridash.Fail) {
                assert.fail("Evaluation of test condition failed");
            }
            else {
                throw e;
            }
        }
    }

    test = Tridash.resolve(test);

    var type = Tridash.resolve(test.type);

    if (type == Tridash.get_symbol('group')) {
        describe(Tridash.resolve(test.description), function() {
            run_tridash_tests(test.tests);
        });
    }
    else if (type == Tridash.get_symbol('test')) {
        it(Tridash.resolve(test.description), function() {
            run_test(test.condition);
        });
    }
}

it('Core Module Tests', () => {
    return util.node_value(Tridash.nodes.core_tests).then(test => {
        run_tridash_test(test);
    }).catch(e => {
        assert.fail("Exception when running Tridash tests");
    });
});

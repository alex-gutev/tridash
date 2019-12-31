var Tridash = require('../runtime/tridash.js');
var assert = require('assert');
var util = require('./test_util.js');

require('./cycles.js');

var i = Tridash.nodes['i'];
var j = Tridash.nodes['j'];
var output = Tridash.nodes['output'];

describe("Integration Test: Cyclic Bindings", function() {
    it('Output should resolve to a cyclic list', async function() {
        Tridash.set_values([[i, 0], [j, 1]]);

        assert.deepEqual(util.resolve_list(await output.next_value, 3), [0,1,0]);
    });
});

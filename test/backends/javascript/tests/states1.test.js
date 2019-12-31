var Tridash = require('../runtime/tridash.js');
var assert = require('assert');
var util = require('./test_util.js');

require('./states1.js');

var counter = Tridash.nodes['counter'];
var start = Tridash.nodes['start'];
var clicked = Tridash.nodes['clicked?'];

describe('Explicit States Counter', function() {
    describe('Setting Initial Counter Value', function() {
        it('Should set initial value of counter to 0', async function() {
            var value = util.node_value(counter);
            start.set_value(0);

            assert.equal(await value, 0);
        });
    });

    describe('Incrementing Counter Value', function() {
        it('Should increment after changing to increment state', async function() {
            var value = util.node_value(counter);
            clicked.set_value(true);

            assert.equal(await value, 1);
        });

        it('Should retain value after changing back to default state', async function() {
            var value = util.node_value(counter);
            clicked.set_value(false);

            assert.equal(await value, 1);
        });

        it('Should increment again after changing back to increment state', async function() {
            var value = util.node_value(counter);
            clicked.set_value(true);

            assert.equal(await value, 2);
        });
    });

    describe('Resetting Counter Value', function() {
        it('Should reset counter value when changing start', async function() {
            var value = util.node_value(counter);
            start.set_value(10);

            assert.equal(await value, 10);
        });
    });
});

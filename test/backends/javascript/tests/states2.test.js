var Tridash = require('../runtime/tridash.js');
var assert = require('assert');
var util = require('./test_util.js');

require('./states2.js');

var counter = Tridash.nodes['counter'];
var start = Tridash.nodes['start'];
var clicked = Tridash.nodes['clicked?'];
var delta = Tridash.nodes['delta'];

describe('Explicit States Counter With Delta', function() {
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

            delta.set_value(1);
            clicked.set_value(1);

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

    describe('Changing Delta', function() {
        it('Changing delta should not change counter value', async function() {
            var value = util.node_value(counter);

            delta.set_value(10);
            clicked.set_value(false);

            assert.equal(await value, 2);
        });

        it('Should increment with new delta when changing to increment state', async function() {
           var value = util.node_value(counter);
           clicked.set_value(true);

           assert.equal(await value, 12);
        });
    });

    describe('Resetting Counter Value', function() {
        it('Should reset counter value when changing start', async function() {
            var value = util.node_value(counter);
            start.set_value(5);

            assert.equal(await value, 5);
        });
    });
});

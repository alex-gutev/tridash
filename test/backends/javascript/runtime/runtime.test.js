/**
 * functions.js
 *
 * Tridash JavaScript runtime library Tests.
 *
 * Copyright 2019 Alexander Gutev
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

var assert = require('assert');
var tridash = require('./tridash.js');

describe('Lazy Evaluation', function() {
    describe('Thunk', function() {
	    it('Tridash.resolve should return argument when called with value', function() {
	        assert.equal(tridash.resolve(1), 1);
	    });

	    it('Thunk function should be called, when thunk is resolved', function() {
	        var thunk = new tridash.Thunk(() => 1 + 1);
	        assert.equal(tridash.resolve(thunk), 2);
	    });

	    it('Thunk returned by a thunk should be resolved till a value is returned', function() {
	        var thunk1 = new tridash.Thunk(() => 2 + 3);
	        var thunk2 = new tridash.Thunk(() => thunk1);
	        var thunk3 = new tridash.Thunk(() => thunk2);

	        assert.equal(tridash.resolve(thunk3), 5);
	    });

	    it('Thunk function should only be called once', function() {
	        var times = 0;
	        var thunk = new tridash.Thunk(() => {
		        times++;
		        return 5 + 6;
	        });

	        assert.equal(tridash.resolve(thunk), 11);
	        assert.equal(tridash.resolve(thunk), 11);
	        assert.equal(tridash.resolve(thunk), 11);

	        assert.equal(times, 1);
	    });
    });

    describe('CatchThunk', function() {
	    it('Tridash.resolve should return value returned by CatchThunk when no errors', function() {
	        var thunk = new tridash.CatchThunk(() => 1 + 1, "error");

	        assert.equal(tridash.resolve(thunk), 2);
	    });

	    it('Tridash.resolve should return catch value when error in CatchThunk.compute', function() {
	        var thunk = new tridash.CatchThunk(() => {
		        throw new tridash.Fail();
		        return 1 + 1;
	        }, "error 1");

	        assert.equal(tridash.resolve(thunk), "error 1");
	    });

	    it('Catch value should also be resolved if it is a Thunk', function() {
	        var thunk = new tridash.CatchThunk(() => {
		        throw new tridash.Fail();
	        }, new tridash.Thunk(() => 2 + 3));

	        assert.equal(tridash.resolve(thunk), 5);
	    });

	    it('Catch value should also be resolved if it is a CatchThunk', function() {
	        var thunk = new tridash.CatchThunk(() => {
		        throw new tridash.Fail();
	        }, new tridash.CatchThunk(() => 2 + 3, 0));

	        assert.equal(tridash.resolve(thunk), 5);
	    });

	    it('If catch value is a CatchThunk, its handler should be called', function() {
	        var thunk = new tridash.CatchThunk(() => {
		        throw new tridash.Fail();
	        }, new tridash.CatchThunk(() => {
		        throw new tridash.Fail();
	        }, 45));

	        assert.equal(tridash.resolve(thunk), 45);
	    });

	    it('Chaining of CatchThunk catch values', function() {
	        var catch1 = false;
	        var catch2 = false;

	        var thunk = new tridash.CatchThunk(() => {
		        throw new tridash.Fail();
	        }, new tridash.CatchThunk(() => {
		        catch1 = true;
		        throw new tridash.Fail();
	        }, new tridash.CatchThunk(() => {
		        catch2 = true;
		        throw new tridash.Fail();
	        }, new tridash.CatchThunk(() => 5, 4))));

	        assert.equal(tridash.resolve(thunk), 5);

	        assert(catch1, "First catch handler called");
	        assert(catch2, "Second catch handler called");
	    });

	    it('Nested CatchThunk handlers are called starting from innermost handler', function() {
	        var catch1 = false;

	        var thunk = new tridash.CatchThunk(() => {
		        return new tridash.CatchThunk(() => {
		            return new tridash.CatchThunk(() => {
			            return new tridash.Thunk(() => {
			                throw new tridash.Fail();
			            });
		            }, new tridash.Thunk(() => {
			            catch1 = true;

			            return new tridash.Thunk(() => {
			                throw new tridash.Fail();
			            });
		            }));
		        }, 2);
	        }, 1);

	        assert.equal(tridash.resolve(thunk), 2);
	        assert(catch1, "Inner-most handler called.");
	    });
    });
});
    });
});

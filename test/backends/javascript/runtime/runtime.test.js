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

describe('Builtin Functions', function() {
    describe('Comparison', function() {
        describe('Equal - Tridash.eq', function() {
            it('Should return true for equal numbers', function() {
                assert(tridash.eq(1, 1));
                assert(tridash.eq(new tridash.Thunk(() => 1), new tridash.Thunk(() => 1)));
            });

            it('Should return true for equal characters', function() {
                var c1 = new tridash.Char("c");
                var c2 = new tridash.Char("c");

                assert(tridash.eq(c1, c2));
                assert(tridash.eq(new tridash.Thunk(() => c1), new tridash.Thunk(() => c2)));
            });

            it('Should return true for equal strings', function() {
                var s2 = "hello";
                s2 += " world";

                assert(tridash.eq("hello world", s2));
                assert(tridash.eq(new tridash.Thunk(() => "hello world"), new tridash.Thunk(() => s2)));
            });

            it('Should return true for identical objects', function() {
                var obj = { x : 1 };

                assert(tridash.eq(obj, obj));
                assert(tridash.eq(new tridash.Thunk(() => obj), obj));
            });


            it('Should return false for unequal numbers', function() {
                assert(!tridash.eq(1, 2));
                assert(!tridash.eq(new tridash.Thunk(() => 1), new tridash.Thunk(() => 2)));
            });

            it('Should return false for unequal characters', function() {
                var c1 = new tridash.Char("c");
                var c2 = new tridash.Char("d");

                assert(!tridash.eq(c1, c2));
                assert(!tridash.eq(new tridash.Thunk(() => c1), new tridash.Thunk(() => c2)));
            });

            it('Should return false for unequal strings', function() {
                assert(!tridash.eq("hello world", "hello"));
                assert(!tridash.eq(new tridash.Thunk(() => "hello")), new tridash.Thunk(() => "hello"));
            });

            it('Should return false for non-identical objects', function() {
                var obj1 = { x : 1};
                var obj2 = { x : 1};

                assert(!tridash.eq(obj1, obj2));
                assert(!tridash.eq(new tridash.Thunk(() => obj1)), obj2);
            });

            it("Should return false if types don't match", function() {
                assert(!tridash.eq(1, "x"));
                assert(!tridash.eq("x", new tridash.Char("x")));
            });


            it('Should return failures as thunks', function() {
                var f = tridash.fail();
                var res1 = tridash.eq(f, 1);
                var res2 = tridash.eq(1, f);

                assert.throws(() => tridash.resolve(res1), tridash.Fail);
                assert.throws(() => tridash.resolve(res2), tridash.Fail);
            });
        });

        describe('Not Equal - Tridash.neq', function() {
            it('Should return false for equal numbers', function() {
                assert(!tridash.neq(1, 1));
                assert(!tridash.neq(new tridash.Thunk(() => 1), new tridash.Thunk(() => 1)));
            });

            it('Should return false for equal characters', function() {
                var c1 = new tridash.Char("c");
                var c2 = new tridash.Char("c");

                assert(!tridash.neq(c1, c2));
                assert(!tridash.neq(new tridash.Thunk(() => c1), new tridash.Thunk(() => c2)));
            });

            it('Should return false for equal strings', function() {
                var s2 = "hello";
                s2 += " world";

                assert(!tridash.neq("hello world", s2));
                assert(!tridash.neq(new tridash.Thunk(() => "hello world"), new tridash.Thunk(() => s2)));
            });

            it('Should return false for identical objects', function() {
                var obj = { x : 1 };

                assert(!tridash.neq(obj, obj));
                assert(!tridash.neq(new tridash.Thunk(() => obj), obj));
            });


            it('Should return true for unequal numbers', function() {
                assert(tridash.neq(1, 2));
                assert(tridash.neq(new tridash.Thunk(() => 1), new tridash.Thunk(() => 2)));
            });

            it('Should return true for unequal characters', function() {
                var c1 = new tridash.Char("c");
                var c2 = new tridash.Char("d");

                assert(tridash.neq(c1, c2));
                assert(tridash.neq(new tridash.Thunk(() => c1), new tridash.Thunk(() => c2)));
            });

            it('Should return true for unequal strings', function() {
                assert(tridash.neq("hello world", "hello"));
                assert(tridash.neq(new tridash.Thunk(() => "hello")), new tridash.Thunk(() => "hello"));
            });

            it('Should return true for non-identical objects', function() {
                var obj1 = { x : 1};
                var obj2 = { x : 1};

                assert(tridash.neq(obj1, obj2));
                assert(tridash.neq(new tridash.Thunk(() => obj1)), obj2);
            });

            it("Should return true if types don't match", function() {
                assert(tridash.neq(1, "x"));
                assert(tridash.neq("x", new tridash.Char("x")));
            });


            it('Should return failures as thunks', function() {
                var f = tridash.fail();
                var res1 = tridash.neq(f, 1);
                var res2 = tridash.neq(1, f);

                assert.throws(() => tridash.resolve(res1), tridash.Fail);
                assert.throws(() => tridash.resolve(res2), tridash.Fail);
            });
        });
    });

    describe('Lists', function() {
        describe('Tridash.cons', function() {
            it('Does not resolves its arguments', function() {
                assert(tridash.cons(new tridash.Thunk(() => {
                    throw tridash.Fail();
                }), new tridash.Thunk(() => {
                    throw tridash.Fail();
                })));
            });
        });

        describe('Tridash.head', function() {
            it('Returns head of ConsCell', function() {
                var list = new tridash.Thunk(() => {
                    return tridash.cons(new tridash.Thunk(() => 1), null);
                });

                assert.equal(tridash.resolve(tridash.head(list)), 1);
            });

            it('Returns first element of Array', function() {
                var list = new tridash.Thunk(() => {
                    return [5, 6, 7];
                });

                assert.equal(tridash.resolve(tridash.head(list)), 5);
            });

            it('Returns first element of SubArray', function() {
                var list = new tridash.Thunk(() => {
                    return tridash.tail([5, 6, 7]);
                });

                assert.equal(tridash.resolve(tridash.head(list)), 6);
            });

            it('Fails on `null`', function() {
                assert.throws(() => tridash.resolve(tridash.head(null)), tridash.Fail);
            });

            it('Fails on empty array', function() {
                assert.throws(() => tridash.resolve(tridash.head([])), tridash.Fail);
            });

            it('Fails on empty SubArray', function() {
                var sub = tridash.tail([1]);
                assert.throws(() => tridash.resolve(tridash.head(sub)), tridash.Fail);
            });

            it('Fails on non-array', function() {
                assert.throws(() => tridash.resolve(tridash.head(1)), tridash.Fail);
            });
        });

        describe('Tridash.tail', function() {
            it('Returns tail of ConsCell', function() {
                var list = new tridash.Thunk(() => {
                    return tridash.cons(
                        new tridash.Thunk(() => 1), tridash.cons(2, null));
                });

                assert.deepStrictEqual(tridash.resolve(tridash.tail(list)), tridash.cons(2, null));
            });

            it('Returns SubArray of Array', function() {
                var list = new tridash.Thunk(() => {
                    return [5, 6, 7];
                });

                var rest = tridash.tail(list);

                assert.equal(tridash.resolve(tridash.head(rest)), 6, "Second Element");
                assert.equal(
                    tridash.resolve(tridash.head(tridash.tail(rest))),
                    7,
                    "Third Element"
                );
            });

            it('Fails on `null`', function() {
                assert.throws(() => tridash.resolve(tridash.tail(null)), tridash.Fail);
            });

            it('Fails on empty Array', function() {
                assert.throws(() => tridash.resolve(tridash.tail([])), tridash.Fail);
            });

            it('Fails if tail is empty', function() {
                assert.throws(() => tridash.resolve(tridash.tail([1])), tridash.Fail);
            });

            it('Fails if tail is empty SubArray', function() {
                var sub = tridash.tail([1,2]);
                assert.throws(() => tridash.resolve(tridash.tail(sub)), tridash.Fail);
            });

            it('Fails on non-list', function() {
                assert.throws(() => tridash.resolve(tridash.tail(1)), tridash.Fail);
            });
        });

        describe('Tridash.is_cons', function() {
            it('Returns true for conses', () => {
                var cons = new tridash.Thunk(() => {
                    return tridash.cons(1, 2);
                });

                assert(tridash.resolve(tridash.is_cons(cons)));
            });

            it('Returns true for arrays', () => {
                var list = new tridash.Thunk(() => {
                    return [1,2,3];
                });

                assert(tridash.resolve(tridash.is_cons(list)));
            });

            it('Returns true for SubArrays', () => {
                var list = tridash.tail([1,2,3]);

                assert(tridash.resolve(tridash.is_cons(list)));
            });

            it('Returns false for non-lists', () => {
                assert(!tridash.resolve(tridash.is_cons(1)));
            });

            it('Returns false for empty arrays', () => {
                assert(!tridash.resolve(tridash.is_cons([])));
            });
        });
    });

    describe('Symbols', function() {
        describe('Tridash.get_symbol', function() {
            it('Should return a symbol with same identifier', function() {
                var sym = tridash.get_symbol("x");
                assert.equal(sym.name, "x");
            });

            it('Should always return the same symbol object', function() {
                var sym1 = tridash.get_symbol("field");
                var sym2 = tridash.get_symbol("field");

                assert.equal(sym1, sym2);
            });

            it('Should return a different object for different symbols', function() {
                var sym1 = tridash.get_symbol("field1");
                var sym2 = tridash.get_symbol("field2");

                assert.notEqual(sym1, sym2);
            });
        });
    });

    describe('Type Checking', function() {
        describe('API Functions', function() {
            describe('int? - is_int', function() {
                it('Should return true when given an integer', function() {
                    assert(tridash.resolve(tridash.is_int(1)));
                    assert(tridash.resolve(tridash.is_int(new tridash.Thunk(() => 1))));
                });

                it('Should return false when not given an integer', function() {
                    assert(!tridash.resolve(tridash.is_int(1.2)));
                    assert(!tridash.resolve(tridash.is_int("x")));
                    assert(!tridash.resolve(tridash.is_int({ x: 1})));
                });

                it('Should return failures as Thunks', function() {
                    var thunk = tridash.is_int(new tridash.Thunk(() => {
                        throw new tridash.Fail();
                    }));

                    assert(thunk, "Returns a thunk instead of throwing exception");

                    assert.throws(() => { tridash.resolve(thunk); }, tridash.Fail);
                });
            });

            describe('real? - is_real', function() {
                it('Should return true when given an integer', function() {
                    assert(tridash.resolve(tridash.is_real(1)));
                    assert(tridash.resolve(tridash.is_real(new tridash.Thunk(() => 1))));
                });

                it('Should return true when given a float', function() {
                    assert(tridash.resolve(tridash.is_real(1.2)));
                });

                it('Should return false when not given a number', function() {
                    assert(!tridash.resolve(tridash.is_real("x")));
                    assert(!tridash.resolve(tridash.is_real({ x: 1})));
                });

                it('Should return failures as Thunks', function() {
                    var thunk = tridash.is_real(new tridash.Thunk(() => {
                        throw new tridash.Fail();
                    }));

                    assert(thunk, "Returns a thunk instead of throwing exception");

                    assert.throws(() => { tridash.resolve(thunk); }, tridash.Fail);
                });
            });

            describe('string? - is_string', function() {
                it('Should return true when given a string', function() {
                    assert(tridash.resolve(tridash.is_string("hello")));
                    assert(tridash.resolve(tridash.is_string(new tridash.Thunk(() => "hello"))));
                });

                it('Should return false when not given a string', function() {
                    assert(!tridash.resolve(tridash.is_string(1)));
                    assert(!tridash.resolve(tridash.is_string(1.2)));
                    assert(!tridash.resolve(tridash.is_string({ x: 1})));
                });

                it('Should return failures as Thunks', function() {
                    var thunk = tridash.is_string(new tridash.Thunk(() => {
                        throw new tridash.Fail();
                    }));

                    assert(thunk, "Returns a thunk instead of throwing exception");

                    assert.throws(() => { tridash.resolve(thunk); }, tridash.Fail);
                });
            });

            describe('inf? - is_inf', function() {
                it('Should return true when given Infinity', function() {
                    assert(tridash.resolve(tridash.is_inf(Infinity)));
                    assert(tridash.resolve(tridash.is_inf(-Infinity)));
                    assert(tridash.resolve(tridash.is_inf(new tridash.Thunk(() => Infinity))));
                });

                it('Should return false when not given Infinity', function() {
                    assert(!tridash.resolve(tridash.is_inf(1)));
                    assert(!tridash.resolve(tridash.is_inf(1.2)));
                    assert(!tridash.resolve(tridash.is_inf(NaN)));
                    assert(!tridash.resolve(tridash.is_inf("hello")));
                    assert(!tridash.resolve(tridash.is_inf({ x: 1})));
                });

                it('Should return failures as Thunks', function() {
                    var thunk = tridash.is_inf(new tridash.Thunk(() => {
                        throw new tridash.Fail();
                    }));

                    assert(thunk, "Returns a thunk instead of throwing exception");

                    assert.throws(() => { tridash.resolve(thunk); }, tridash.Fail);
                });
            });

            describe('NaN? - is_nan', function() {
                it('Should return true when given NaN', function() {
                    assert(tridash.resolve(tridash.is_nan(NaN)));
                    assert(tridash.resolve(tridash.is_nan(new tridash.Thunk(() => NaN))));
                });

                it('Should return false when given a number', function() {
                    assert(!tridash.resolve(tridash.is_nan(1)));
                    assert(!tridash.resolve(tridash.is_nan(1.2)));
                });

                it('Should return failures as Thunks', function() {
                    var thunk = tridash.is_nan(new tridash.Thunk(() => {
                        throw new tridash.Fail();
                    }));

                    assert(thunk, "Returns a thunk instead of throwing exception");

                    assert.throws(() => { tridash.resolve(thunk); }, tridash.Fail);
                });
            });
        });

        describe('Internal Type Checks', function() {
            describe('check_number', function() {
                it('Should return argument if it is a number', function() {
                    assert.equal(tridash.check_number(1), 1);
                    assert.equal(tridash.check_number(23), 23);
                    assert.equal(tridash.check_number(2.4), 2.4);
                });

                it('Should throw Fail exception if not a number', function() {
                    assert.throws(() => { tridash.check_number("x"); }, tridash.Fail);
                    assert.throws(() => { tridash.check_number({ x: 1}); }, tridash.Fail);
                });
            });

            describe('check_value', function() {
                it('Should return argument if it is a number', function() {
                    assert.equal(tridash.check_value(1), 1);
                    assert.equal(tridash.check_value(23), 23);
                    assert.equal(tridash.check_value(2.4), 2.4);
                });

                it('Should return argument if it is a string', function() {
                    assert.equal(tridash.check_value("hello world"), "hello world");
                });

                it('Should return argument if it is a Symbol', function() {
                    assert.equal(tridash.check_value(tridash.get_symbol('x')), tridash.get_symbol('x'));
                });

                it('Should throw Fail exception if not a primitive value', function() {
                    assert.throws(() => { tridash.check_value({ x: 1}); }, tridash.Fail);
                });
            });

            describe('check_string', function() {
                it('Should return argument if it is a string', function() {
                    assert.equal(tridash.check_string("hello"), "hello");
                });

                it('Should throw Fail exception if not a string', function() {
                    assert.throws(() => { tridash.check_string(1); }, tridash.Fail);
                    assert.throws(() => { tridash.check_string({ x: 1}); }, tridash.Fail);
                });
            });
        });
    });

    describe('Type casts', function() {
        describe('Tridash.cast_string', function() {
            it('Should return integer as string', function() {
                assert.equal(tridash.cast_string(10), "10");
            });

            it('Should return character as string', function() {
                assert.equal(tridash.cast_string(new tridash.Char("x")), "x");
            });

            it('Should return string as string', function() {
                assert.equal(tridash.cast_string("hello"), "hello");
            });
        });
    });

    describe('Dictionaries', function() {
        describe('Tridash.member', function() {
            it('Should return the value of the member', function() {
                assert.equal(tridash.member({ name : 'Bob' }, 'name'), 'Bob');
            });

            it('Should resolve Thunk arguments', function() {
                var obj = new tridash.Thunk(() => { return { name : 'Bob' } });
                var key = new tridash.Thunk(() => 'name');

                assert.equal(tridash.member(obj, key), 'Bob');
            });

            it('Should return failures as Thunks', function() {
                var res = tridash.member(new Tridash.Thunk(() => {
                    throw new tridash.Fail();
                }, 'type'));

                assert(res, 'Returns a thunk instead of throwing exception');
                assert.throws(() => tridash.resolve(res), tridash.Fail);
            });

            it('Should fail if key not in dictionary', function() {
                assert.throws(() => tridash.resolve(tridash.member({ x : 1 }, 'y')), tridash.Fail);
            });

            it('Should fail if not dictionary', function() {
                assert.throws(() => tridash.resolve(tridash.member(1, 'z'), tridash.Fail));
                assert.throws(() => tridash.resolve(tridash.member("hello", 'z'), tridash.Fail));
            });
        });
    });

    describe('Strings', function() {
        describe('string-at - string_at', function() {
            it('Should return character at given index', function() {
                assert.equal(tridash.resolve(tridash.string_at("hello", 0).chr), "h");
                assert.equal(tridash.resolve(tridash.string_at(new tridash.Thunk(() => "hello"), 2).chr), "l");
            });

            it('Should fail if not given a string and integer', function() {
                var thunk1 = tridash.string_at(0,0);
                var thunk2 = tridash.string_at("hello", "world");

                assert.throws(() => { tridash.resolve(thunk1); }, tridash.Fail);
                assert.throws(() => { tridash.resolve(thunk2); }, tridash.Fail);
            });
        });

        describe('string-concat - string_concat', function() {
            it('Should return the concatenation of two strings', function() {
                assert.equal(tridash.resolve(tridash.string_concat("hello ", "world")), "hello world");
                assert.equal(
                    tridash.resolve(
                        tridash.string_concat(new tridash.Thunk(() => "hello "), "world")
                    ),
                    "hello world"
                );
            });

            it('Should fail if arguments are not strings', function() {
                var thunk1 = tridash.string_concat("hello ", 1);
                var thunk2 = tridash.string_concat(1, "hello");
                var thunk3 = tridash.string_concat(1, 2);

                assert.throws(() => tridash.resolve(thunk1), tridash.Fail);
                assert.throws(() => tridash.resolve(thunk2), tridash.Fail);
                assert.throws(() => tridash.resolve(thunk3), tridash.Fail);
            });
        });
    });

    describe('Failures', function() {
        describe('Tridash.fail', function() {
            it('Should throw a Fail exception when resolved', function() {
                var fail = tridash.fail();
                assert(fail, "Does not throw exception when called.");
                assert.throws(() => tridash.resolve(fail), tridash.Fail);
            });

            it('Should throw a Fail exception with correct type', function() {
                var fail = tridash.fail('x');
                assert(fail, "Does not throw exception when called.");
                assert.throws(() => tridash.resolve(fail), (e) => e instanceof tridash.Fail && e.type == 'x');
            });
        });

        describe('Tridash.fail_type', function() {
            it('Should return failure type on failure', function() {
                assert.equal(tridash.fail_type(tridash.fail()), null);
                assert.equal(tridash.fail_type(tridash.fail('x')), 'x');
                assert.equal(tridash.fail_type(tridash.fail(1)), 1);
            });

            it('Should fail if argument does not fail', function() {
                assert.throws(() => tridash.resolve(tridash.fail_type(1)), tridash.Fail);

                var thunk = new tridash.Thunk(() => 1);
                assert.throws(() => tridash.resolve(tridash.fail_type(thunk)), tridash.Fail);
            });
        });

        describe('Tridash.make_catch_thunk', function() {
            it('Should resolve to the value of try, if it does not fail', function() {
                var value = new tridash.Thunk(() => 1);
                assert.equal(tridash.resolve(tridash.make_catch_thunk(value, 'fail')), 1);
            });

            it('Should resolve to the value of catch, if try fails', function() {
                var value = new tridash.Thunk(() => 'fail-value');
                assert.equal(tridash.resolve(tridash.make_catch_thunk(tridash.fail(), value)), 'fail-value');
            });

            it('Should resolve to the value of catch only if test returns true', function() {
                var value = new tridash.Thunk(() => 'fail-value');
                var test = (type) => type == 'my-type';

                console.log(tridash.resolve(
                    tridash.make_catch_thunk(tridash.fail('my-type'), value, test)));

                assert.equal(
                    tridash.resolve(
                        tridash.make_catch_thunk(tridash.fail('my-type'), value, test)),
                    'fail-value'
                );

                assert.throws(
                    () => tridash.resolve(tridash.make_catch_thunk(tridash.fail(), value, test)),
                    tridash.Fail
                );
            });

            it('Should resolve test function', function() {
                var value = new tridash.Thunk(() => 'fail-value');
                var test = new tridash.Thunk(() => ((type) => type == 'my-type'));

                console.log(tridash.resolve(
                    tridash.make_catch_thunk(tridash.fail('my-type'), value, test)));

                assert.equal(
                    tridash.resolve(
                        tridash.make_catch_thunk(tridash.fail('my-type'), value, test)),
                    'fail-value'
                );

                assert.throws(
                    () => tridash.resolve(tridash.make_catch_thunk(tridash.fail(), value, test)),
                    tridash.Fail
                );
            });

            it('Should resolve CatchThunks in try_value', function() {
                var try1 = false;
                var catch1 = false;

                var thunk = tridash.make_catch_thunk(
                    tridash.make_catch_thunk(new tridash.Thunk(() => {
                        try1 = true;
                        return tridash.fail();
                    }), new tridash.Thunk(() => {
                        catch1 = true;
                        return tridash.fail();
                    })), 24);

                assert.equal(tridash.resolve(thunk), 24);
                assert(try1, "First try_value resolved");
                assert(catch1, "First catch_value resolved");
            });
        });
    });
});

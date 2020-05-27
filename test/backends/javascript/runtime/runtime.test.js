/**
 * runtime.test.js
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
});

describe('Builtin Functions', function() {
    function test_fail_type(type) {
        return (e) => {
            return (e instanceof tridash.Fail) && tridash.resolve(e.type) === type;
        };
    }

    describe('Arithmetic', function() {
        describe('Subtract or Negate - sub_neg', function() {
            it('Should negate its first argument if second argument not provided', function() {
                assert.equal(tridash.sub_neg(1), -1);
                assert.equal(tridash.sub_neg(new tridash.Thunk(() => 1)), -1);
            });

            it('Should return difference if both arguments provided', function() {
                assert.equal(tridash.sub_neg(3, 1), 2);
                assert.equal(tridash.sub_neg(
                    new tridash.Thunk(() => 3),
                    new tridash.Thunk(() => 2)
                ), 1);
            });

            it('Should return failures as thunks', function() {
                var res1 = tridash.sub_neg(tridash.fail());
                var res2 = tridash.sub_neg(2, tridash.fail());

                assert.throws(() => tridash.resolve(res1), tridash.Fail);
                assert.throws(() => tridash.resolve(res2), tridash.Fail);
            });

            it('Should fail if arguments not numbers', function() {
                var res1 = tridash.sub_neg("hello");
                var res2 = tridash.sub_neg(1, "x");

                assert.throws(() => tridash.resolve(res1), test_fail_type(tridash.TypeError()));
                assert.throws(() => tridash.resolve(res2), test_fail_type(tridash.TypeError()));
            });
        });
    });

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

            it('Fails on empty list', function() {
                assert.throws(() => tridash.resolve(tridash.head(tridash.Empty())), test_fail_type(tridash.Empty()));
            });

            it('Fails on non-array', function() {
                assert.throws(() => tridash.resolve(tridash.head(1)), test_fail_type(tridash.TypeError()));
            });
        });

        describe('Tridash.tail', function() {
            it('Returns tail of ConsCell', function() {
                var list = new tridash.Thunk(() => {
                    return tridash.cons(
                        new tridash.Thunk(() => 1), tridash.cons(2, tridash.Empty()));
                });

                assert.deepStrictEqual(tridash.resolve(tridash.tail(list)), tridash.cons(2, tridash.Empty()));
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

            it('Fails on empty list', function() {
                assert.throws(() => tridash.resolve(tridash.tail(tridash.Empty())), test_fail_type(tridash.Empty()));
            });

            it('Fails if tail is empty SubArray', function() {
                var sub = tridash.tail([1]);
                assert.throws(() => tridash.resolve(tridash.tail(sub)), test_fail_type(tridash.Empty()));
            });

            it('Fails on non-list', function() {
                assert.throws(() => tridash.resolve(tridash.tail(1)), test_fail_type(tridash.TypeError()));
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
                    assert.throws(() => { tridash.check_number("x"); }, test_fail_type(tridash.TypeError()));
                    assert.throws(() => { tridash.check_number({ x: 1}); }, test_fail_type(tridash.TypeError()));
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
                    assert.throws(() => { tridash.check_value({ x: 1}); }, test_fail_type(tridash.TypeError()));
                });
            });

            describe('check_string', function() {
                it('Should return argument if it is a string', function() {
                    assert.equal(tridash.check_string("hello"), "hello");
                });

                it('Should throw Fail exception if not a string', function() {
                    assert.throws(() => { tridash.check_string(1); }, test_fail_type(tridash.TypeError()));
                    assert.throws(() => { tridash.check_string({ x: 1}); }, test_fail_type(tridash.TypeError()));
                });
            });
        });
    });

    describe('Type casts', function() {
        describe('Tridash.cast_int', function() {
            it('Should return integer argument directly', function() {
                assert.equal(tridash.cast_int(5), 5);

                // Test that thunks are resolved
                assert.equal(tridash.cast_int(new tridash.Thunk(() => 6)), 6);
            });
            it('Should return truncated real argument', function() {
                assert.equal(tridash.cast_int(4.5), 4);
            });
            it('Should parse integer from string', function() {
                assert.equal(tridash.cast_int('18'), 18);
            });
            it('Should return failure if integer could not be parsed from string', function() {
                assert.throws(() => tridash.resolve(tridash.cast_int('foo')), test_fail_type(tridash.InvalidInteger()));
            });
            it('Should return failures as thunks', function() {
                var f = tridash.fail();
                var res = tridash.cast_int(f);

                assert.ok(res);
                assert.throws(() => tridash.resolve(res), tridash.Fail);
            });
        });

        describe('Tridash.cast_real', function() {
            it('Should return integer argument directly', function() {
                assert.equal(tridash.cast_real(5), 5);

                // Test that thunks are resolved
                assert.equal(tridash.cast_real(new tridash.Thunk(() => 6)), 6);
            });
            it('Should return real argument directly', function() {
                assert.equal(tridash.cast_real(4.5), 4.5);
            });
            it('Should parse real from string', function() {
                assert.equal(tridash.cast_real('18'), 18);
                assert.equal(tridash.cast_real('15.5'), 15.5);
            });
            it('Should return failure if integer could not be parsed from string', function() {
                assert.throws(
                    () => tridash.resolve(tridash.cast_real('foo')),
                    test_fail_type(tridash.InvalidReal())
                );
            });
            it('Should return failures as thunks', function() {
                var f = tridash.fail();
                var res = tridash.cast_real(f);

                assert.ok(res);
                assert.throws(() => tridash.resolve(res), tridash.Fail);
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
                }), 'type');

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
                var thunk3 = tridash.string_at("hello", 1.5);

                assert.throws(() => { tridash.resolve(thunk1); }, test_fail_type(tridash.TypeError()));
                assert.throws(() => { tridash.resolve(thunk2); }, test_fail_type(tridash.TypeError()));
                assert.throws(() => { tridash.resolve(thunk3); }, test_fail_type(tridash.TypeError()));
            });

            it('Should fail if index is less than 0', function() {
                assert.throws(() => {
                    tridash.resolve(tridash.string_at("hello", -1));
                }, test_fail_type(tridash.IndexOutBounds()));
            });

            it('Should fail if index is greater than or equal to string length', function() {
                assert.throws(() => {
                    tridash.resolve(tridash.string_at("hello", 5));
                }, test_fail_type(tridash.IndexOutBounds()));

                assert.throws(() => {
                    tridash.resolve(tridash.string_at("hello", 10));
                }, test_fail_type(tridash.IndexOutBounds()));
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

                assert.throws(() => tridash.resolve(thunk1), test_fail_type(tridash.TypeError()));
                assert.throws(() => tridash.resolve(thunk2), test_fail_type(tridash.TypeError()));
                assert.throws(() => tridash.resolve(thunk3), test_fail_type(tridash.TypeError()));
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
                assert.equal(tridash.fail_type(tridash.fail('x')), 'x');
                assert.equal(tridash.fail_type(tridash.fail(1)), 1);

                assert.throws(() => tridash.resolve(tridash.fail_type(tridash.fail())),
                              tridash.Fail);
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

                assert.equal(
                    tridash.resolve(
                        tridash.make_catch_thunk(tridash.fail('my-type'), value, test)
                    ),
                    'fail-value',
                    'With type satisfying test function'
                );

                assert.throws(
                    () => tridash.resolve(tridash.make_catch_thunk(tridash.fail('other-type'), value, test)),
                    test_fail_type('other-type'),
                    'With type not satisfying test function'
                );
            });

            it('Should resolve test function', function() {
                var value = new tridash.Thunk(() => 'fail-value');
                var test = new tridash.Thunk(() => ((type) => type == 'my-type'));

                assert.equal(
                    tridash.resolve(
                        tridash.make_catch_thunk(tridash.fail('my-type'), value, test)
                    ),
                    'fail-value',
                    'With type satisfying test function'
                );

                assert.throws(
                    () => tridash.resolve(tridash.make_catch_thunk(tridash.fail('other-type'), value, test)),
                    test_fail_type('other-type'),
                    'With type not satisfying test function'
                );
            });

            it('Should return failures in test function', function() {
                var test = tridash.fail();
                var catch1 = tridash.make_catch_thunk(tridash.fail(), 'catch-1', test);
                var catch2 = tridash.make_catch_thunk(catch1, 'catch-2');

                assert.equal(tridash.resolve(catch2), 'catch-2');
            });

            it('Should resolve thunks with handlers in try_value', function() {
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

        describe('Tridash.uncatch_thunk', function() {
            it('Should return value when no error', function() {
                var thunk1 = tridash.uncatch_thunk(1);
                var thunk2 = tridash.uncatch_thunk(new tridash.Thunk(() => 1));

                assert.equal(tridash.resolve(thunk1), 1, "Immediate Value");
                assert.equal(tridash.resolve(thunk2), 1, "Thunk Value");
            });

            it('Errors should not be handled in uncatch thunk', function () {
                var fail_thunk = tridash.uncatch_thunk(tridash.fail('type-x'));
                var catch_thunk = tridash.make_catch_thunk(fail_thunk, 'failure');

                assert.throws(() => tridash.resolve(catch_thunk), test_fail_type('type-x'));
            });

            it('Errors should be handled when surrounded in two handlers', function() {
                var fail = tridash.uncatch_thunk(tridash.fail());
                var catch1 = tridash.make_catch_thunk(fail, "failure 1");
                var catch2 = tridash.make_catch_thunk(catch1, "failure 2");

                assert.equal(tridash.resolve(catch2), "failure 2");
            });

            it('Errors should not be handled if uncatch thunks greater than or equal to number of handlers', function() {
                var fail = tridash.uncatch_thunk(tridash.uncatch_thunk(tridash.fail('type-y')));
                var catch1 = tridash.make_catch_thunk(fail, "failure 1");
                var catch2 = tridash.make_catch_thunk(catch1, "failure 2");

                assert.throws(() => tridash.resolve(catch2), test_fail_type('type-y'));
            });
        });
    });

    describe('Functional Utilities', function() {
        function add(a, b, c) {
            return a + b + c;
        };

        function f() {
            return "hello";
        }

        describe('Tridash.mapply', function() {
            it('Applies a function on an array of arguments', function() {
                assert.equal(tridash.mapply(add, [1, 2, 3]), 6);
                assert.equal(tridash.mapply(add, new tridash.Thunk(() => [1, 2, 3])), 6);
                assert.equal(tridash.mapply(new tridash.Thunk(() => add), new tridash.Thunk(() => [1, 2, 3])), 6);
            });

            it('Applies a function on a linked list of arguments', function() {
                assert.equal(
                    tridash.mapply(add, tridash.cons(4, tridash.cons(5, tridash.cons(6, tridash.Empty())))),
                    15
                );
                assert.equal(
                    tridash.mapply(
                        add,
                        new tridash.Thunk(() => tridash.cons(4, new tridash.Thunk(() => tridash.cons(5, tridash.cons(6, tridash.Empty())))))
                    ),
                    15
                );
            });

            it('Applies a function on a SubArray of arguments', function() {
                assert.equal(
                    tridash.mapply(add, tridash.tail([1, 2, 3, 4])),
                    9
                );
                assert.equal(
                    tridash.mapply(add, new tridash.Thunk(() => tridash.tail([1, 2, 3, 4]))),
                    9
                );
            });

            it('Applies a function on an empty list', function() {
                assert.equal(tridash.mapply(f, []), "hello");
                assert.equal(tridash.mapply(f, tridash.Empty()), "hello");
                assert.equal(tridash.mapply(f, tridash.tail([1])), "hello");
                assert.equal(tridash.mapply(f, tridash.tail(tridash.cons(1, tridash.Empty()))), "hello");
            });

            it('Returns `TypeError` if first argument is not a function', function() {
                assert.throws(() => tridash.resolve(tridash.mapply("x", [1, 2, 3])), test_fail_type(tridash.TypeError()));
                assert.throws(() => tridash.resolve(tridash.mapply(new tridash.Thunk(() => "x"), [1, 2, 3])), test_fail_type(tridash.TypeError()));
            });

            it('Returns `TypeError` if second argument is not a list', function() {
                assert.throws(() => tridash.resolve(tridash.mapply(add, 12)), test_fail_type(tridash.TypeError()));
            });

            it('Returns `TypeError` if argument list is malformed', function() {
                assert.throws(() => tridash.resolve(tridash.mapply(add, tridash.cons(1, 2))), test_fail_type(tridash.TypeError()));
                assert.throws(() => tridash.resolve(tridash.mapply(add, tridash.cons(1, new tridash.Thunk(() => 2)))), test_fail_type(tridash.TypeError()));
            });
        });
    });
});

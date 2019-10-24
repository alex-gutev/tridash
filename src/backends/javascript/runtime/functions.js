/**
 * functions.js
 *
 * Tridash JavaScript runtime library.
 *
 * Copyright 2017-2019 Alexander Gutev
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

/** Functions */

/* Comparison */


/* Arithmetic */

/**
 * Returns a - b or -a if b == undefined.
 */
function sub_neg(a, b) {
    if (arguments.length < 1 || arguments.length > 2) {
        return ArityError();
    }

    try {
        a = check_number(resolve(a));
        b = resolve(b);

        if (b === undefined) {
            return -a;
        }

        return a - check_number(b);
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}


/* Comparison */

function eq(ta, tb) {
    try {
        var a = resolve(ta);
        var b = resolve(tb);

        if (a instanceof Char && b instanceof Char)
            return a.chr === b.chr;

        return a === b;
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}

function neq(ta, tb) {
    try {
        var a = resolve(ta);
        var b = resolve(tb);

        if (a instanceof Char && b instanceof Char)
            return a.chr !== b.chr;

        return a !== b;
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}


/* Boolean Expressions */

function and(a, b) {
    try {
        return resolve(a) ? b : false;
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}
function or(a, b) {
    try {
        if ((a = resolve(a)))
            return a;

        return b;
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}

function not(x) {
    try {
        return !resolve(x);
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}


/* Failures */

function fail(type = null) {
    return new Thunk(() => { throw new Fail(type); });
}

function fail_type(value) {
    try {
        resolve(value);
    }
    catch (e) {
        if (e instanceof Fail)
            return e.type;
    }

    return fail(TridashTypeError);
}

function make_catch_thunk(try_value, catch_value, test) {
    if (test) {
        var old_catch = catch_value;

        catch_value = new Thunk(() => {
            try {
                test = resolve(test);
                var type = fail_type(try_value);

                return test(type) ? old_catch : fail(type);
            }
            catch (e) {
                return new Thunk(() => { throw e; });
            }
        });
    }

    return combine_catch_thunk(try_value, catch_value);
}


/* Type Conversions */

function cast_int(x) {
    try {
        var intx = parseInt(resolve(x));

        return !isNaN(intx) ? intx : fail(InvalidInteger);
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
};
function cast_real(x) {
    try {
        var realx = parseFloat(resolve(x));

        return !isNaN(realx) ? realx : fail(InvalidReal);
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
};
function cast_string(x) {
    try {
        x = resolve(x);

        if (x instanceof Char)
            return x.chr;
        else
            return String(x);
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
};


/* Querying Types */

var is_integer = Number.isInteger || function(value) {
    return typeof value === 'number' &&
        isFinite(value) && Math.floor(value) === value;
};

function is_int(x) {
    try {
        return is_integer(resolve(x));
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
};

function is_real(value) {
    try {
        return typeof resolve(value) === 'number';
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
};

function is_string(value) {
    try {
        return typeof resolve(value) === 'string';
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
};

function is_inf(tvalue) {
    try {
        var value = resolve(tvalue);
        return typeof value === 'number' && !isFinite(value) && !isNaN(value);
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
};

function is_nan(value) {
    try {
        return isNaN(resolve(value));
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
};


/* Lists */

function cons(head, tail) {
    return new ConsCell(head, tail);
}

function ConsCell(head, tail) {
    this.head = head;
    this.tail = tail;
}

function SubArray(array, start) {
    this.array = array;
    this.start = start;
}


function head(list) {
    try {
        var l = resolve(list);

        if (l instanceof ConsCell) {
            return l.head;
        }
        else if (Array.isArray(l)) {
            if (l.length > 0)
                return l[0];

            return Empty();
        }
        else if (l instanceof SubArray) {
            return l.array.length > l.start ? l.array[l.start] : Empty();
        }
        else {
            return l === null ? Empty() : fail(TridashTypeError);
        }
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}

function tail(list) {
    try {
        var l = resolve(list);

        if (l instanceof ConsCell) {
            return l.tail;
        }
        else if (Array.isArray(l)) {
            if (l.length > 1)
                return new SubArray(l, 1);

            return Empty();
        }
        else if (l instanceof SubArray) {
            return l.array.length > (l.start + 1) ?
                new SubArray(l.array, l.start + 1) :
                Empty();
        }
        else {
            return l === null ? Empty() : fail(TridashTypeError);
        }
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}

function is_cons(thing) {
    try {
        var value = resolve(thing);

        return value instanceof ConsCell ||
            (value instanceof SubArray && value.array.length > value.start) ||
            (Array.isArray(value) && value.length > 0);
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}


function Empty() {
    return fail(Empty);
}


/* Strings */

/**
 * Wraps a character so that it is a distinct type.
 */
function Char(chr) {
    this.chr = chr;
}

function string_at(tstr, tindex) {
    try {
        var str = check_string(resolve(tstr));
        var index = check_number(resolve(tindex));

        return index < str.length ? new Char(str.charAt(index)) : fail();
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}

function string_concat(tstr1, tstr2) {
    try {
        var str1 = check_string(resolve(tstr1));
        var str2 = check_string(resolve(tstr2));

        return str1 + str2;
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}


/* Internal Type Checking Functions */

/**
 * Checks that @a value is a number.
 *
 * @param value The value to check.
 *
 * @return @a value if it is a number, otherwise throws a 'Fail'
 *   exception.
 */
function check_number(value) {
    if (typeof value === 'number') {
        return value;
    }

    throw new Fail(TridashTypeError);
}

/**
 * Checks that @a value is a number or string.
 *
 * @param value The value to check.
 *
 * @return @a value if value is of the correct type, otherwise throws
 *   a 'Fail' exception.
 */
function check_value(value) {
    if (typeof value === 'number' ||
        typeof value === 'string' ||
        value instanceof Symbol) {
        return value;
    }

    throw new Fail(TridashTypeError);
}

function check_string(value) {
    if (typeof value === 'string') {
        return value;
    }

    throw new Fail(TridashTypeError);
}


/* Symbols */

/**
 * Encapsulates a symbol.
 *
 * @param name The symbol name.
 */
function Symbol(name) {
    this.name = name;
}

/**
 * Table containing all symbols.
 */
var symbol_table = {};

/**
 * Returns the symbol with name @a id. If there is no such symbol in
 * 'symbol_table' a new Symbol object is created.
 *
 * @param id The symbol name.
 *
 * @return The symbol's Symbol object.
 */
function get_symbol(id) {
    return symbol_table[id] || (symbol_table[id] = new Symbol(id));
}


/* Dictionaries */

function member(dict, key) {
    try {
        dict = resolve(dict);
        key = resolve(key);

        return key in dict ? dict[key] : fail();
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}


/* Builtin Failure Types */

function NoValue() {
    return fail(NoValue);
}

function TridashTypeError() {
    return fail(TridashTypeError);
}

function InvalidInteger() {
    return fail(InvalidInteger);
}

function InvalidReal() {
    return fail(InvalidReal);
}

function ArityError() {
    return fail(ArityError);
}

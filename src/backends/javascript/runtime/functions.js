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


/* Fail Exception Type */

/**
 * Used as the fail exception type, which indicates that a value
 * computation has failed.
 *
 * @param type Failure type.
 *
 * @param uncatch Number of failures handlers which should be
 *   bypassed, before this failure is handled.
 */
function Fail(type = null, uncatch = 0) {
    this.type = type;
    this.uncatch = uncatch;
};


/* Thunk */

/**
 * Creates a thunk for lazily computing a value. When the thunk
 * function is called for the first time @a compute is called and the
 * value returned is stored. Subsequent invocations of the thunk
 * function will simply return the stored value.
 *
 * @param compute Function of no arguments, which computes the value.
 *
 * @param fail_handler Function which is invoked when @a compute
 *   returns a failure value. The function is passed the Fail
 *   exception (the function is not called for other exceptions), and
 *   the return value becomes the return value of the thunk.
 *
 * @return The thunk function.
 */
function Thunk(compute, fail_handler = null) {
    this.computed = false;

    this.compute = compute;
    this.fail_handler = fail_handler;
};


/**
 * Computes the value of the thunk if it has not already been
 * computed.
 *
 * @return The value of the thunk
 */
Thunk.prototype.resolve = function() {
    if (this.computed) {
        return this.result;
    }
    else {
        this.result = this.compute();
        this.computed = true;

        return this.result;
    }
};

/**
 * If @a thing is a Thunk, computes the thunk's value, otherwise
 * returns @a thing.
 *
 * @param thing The thing to resolve.
 *
 * @return The resolved value.
 */
function resolve(thing) {
    function Handler(current, next) {
        this.handler = current;
        this.next = next;
    }

    var handler = null;

    while (thing instanceof Thunk) {
        if (thing.fail_handler) {
            handler = new Handler(thing.fail_handler, handler);
        }

        try {
            thing = thing.resolve();
        }
        catch (e) {
            if (e instanceof Fail && handler) {
                thing = handler.handler(e);
                handler = handler.next;
            }
            else {
                throw e;
            }
        }
    }

    return thing;
}

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
            return e.type ? e.type : NoValue();
    }

    return fail(TridashTypeError);
}

/**
 * Creates a thunk with a handler than returns @a catch_value.
 *
 * @param try_value The main value to return.
 *
 * @param catch_value The value to return if @a try_value evaluates to
 *   a failure.
 *
 * @param test Failure type test function. Only failures for which
 *   this function returns true are handled.
 *
 * @return The thunk.
 */
function make_catch_thunk(try_value, catch_value, test) {
    test = test || (() => true);

    var thunk = new Thunk(
        () => try_value,
        (e) => {
            if (e.uncatch === 0) {
                return new Thunk(() => {
                    test = resolve(test);

                    if (test(e.type))
                        return catch_value;

                    throw e;
                });
            }

            return new Thunk(() => {
                throw new Fail(e.type, e.uncatch - 1);
            });
        }
    );

    return thunk;
}

/**
 * Creates a thunk with a failure handler that increments the uncatch
 * count of the failure.
 *
 * @param The primary value to return by the thunk.
 *
 * @param The thunk.
 */
function uncatch_thunk(thing) {
    return new Thunk(
        () => thing,
        (e) => {
            return new Thunk(() => {
                throw new Fail(e.type, e.uncatch + 1);
            });
        }
    );
}


/* Type Conversions */

function cast_int(x) {
    try {
        x = resolve(x);

        if (is_int(x)) {
            return x;
        }
        else if (is_real(x)) {
            return Math.trunc(x);
        }
        else if (is_string(x)) {
            return /^[-+]?(\d+)$/.test(x) ? parseInt(x) :
                InvalidInteger();
        }

        return TridashTypeError();
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
};
function cast_real(x) {
    try {
        x = resolve(x);

        if (is_real(x)) {
            return x;
        }
        else if (is_string(x)) {
            var realx = Number(x);

            return !isNaN(realx) ? realx : InvalidReal();
        }

        return TridashTypeError();
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

            return TridashTypeError();
        }
        else if (l instanceof SubArray) {
            return l.array[l.start];
        }
        else if (l === Empty) {
            return fail(Empty);
        }
        else {
            return fail(TridashTypeError);
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
            var tail = resolve(l.tail);
            return is_list(tail) || tail === Empty ?
                l.tail :
               TridashTypeError();
        }
        else if (Array.isArray(l)) {
            if (l.length > 1)
                return new SubArray(l, 1);
            else if (l.length === 1)
                return Empty;

            return TridashTypeError();
        }
        else if (l instanceof SubArray) {
            return l.array.length > (l.start + 1) ?
                new SubArray(l.array, l.start + 1) :
                Empty;
        }
        else if (l === Empty) {
            return fail(Empty);
        }
        else {
            return TridashTypeError();
        }
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}

function is_cons(thing) {
    try {
        return is_list(thing);
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}

function is_list(value) {
    value = resolve(value);

    return value instanceof ConsCell ||
        (value instanceof SubArray && value.array.length > value.start) ||
        (Array.isArray(value) && value.length > 0);
}


function Empty() {
    return Empty;
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
        var index = check_integer(resolve(tindex));

        return index >= 0 && index < str.length ?
            new Char(str.charAt(index)) : fail(IndexOutBounds);
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
 * Checks that @a value is an integer.
 *
 * @param value The value to check.
 *
 * @return @a value if it is an integer, otherwise throws a 'Fail'
 *   exception.
 */
function check_integer(value) {
    if (is_integer(value)) {
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

/**
 * Checks that @a value is a string.
 *
 * @param value The value to check.
 *
 * @return @a value if it is a string, otherwise throws a 'Fail'
 *   exception.
 */
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

        if (typeof dict !== 'object')
            return TridashTypeError();

        key = resolve(key);

        return key in dict ? dict[key] : fail();
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
}


/* Function Application */

function mapply(f, args) {
    function resolve_list(list) {
        res = [];
        list = resolve(list);

        while (list !== Empty) {
            if (list instanceof ConsCell) {
                res.push(list.head);
                list = resolve(list.tail);
            }
            else if (list instanceof SubArray) {
                res.push(...list.array.slice(list.start));
                break;
            }
            else if (Array.isArray(list)) {
                res.push(...list);
                break;
            }
            else {
                throw new Fail(TridashTypeError);
            }
        }

        return res;
    }

    try {
        f = resolve(f);

        if (typeof f !== "function") {
            return TridashTypeError();
        }

        return f.apply(null, resolve_list(args));
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

function IndexOutBounds() {
    return fail(IndexOutBounds);
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

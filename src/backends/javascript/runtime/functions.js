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


/* Type Conversions */

function cast_int(x) {
    try {
        var intx = parseInt(resolve(x));

        return !isNaN(intx) ? intx :
            new Thunk(() => { throw Fail(); });
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
};
function cast_real(x) {
    try {
        var realx = parseFloat(resolve(x));

        return !isNaN(realx) ? realx :
            new Thunk(() => { throw Fail(); });
    }
    catch (e) {
        return new Thunk(() => { throw e; });
    }
};
function cast_string(x) {
    try {
        return String(resolve(x));
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

            return FailThunk();
        }
        else if (l instanceof SubArray && l.array.length > l.start) {
            return l.array[l.start];
        }
        else {
            return FailThunk();
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

            return FailThunk();
        }
        else if (l instanceof SubArray && l.array.length > (l.start + 1)) {
            return new SubArray(l.array, l.start + 1);
        }
        else {
            return FailThunk();
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

    throw new Fail();
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

    throw new Fail();
}

function check_string(value) {
    if (typeof value === 'string') {
        return value;
    }

    throw new Fail();
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

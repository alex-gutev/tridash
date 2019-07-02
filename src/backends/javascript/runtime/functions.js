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
    return resolve(a) ? b : false;
};
function or(a, b) {
    if ((a = resolve(a)))
        return a;

    return b;
};


/* Type Conversions */

function cast_int(x) {
    return parseInt(resolve(x));
};
function cast_real(x) {
    return parseFloat(resolve(x));
};
function cast_string(x) {
    return String(resolve(x));
};


/* Querying Types */

var is_integer = Number.isInteger || function(value) {
    return typeof value === 'number' &&
        isFinite(value) && Math.floor(value) === value;
};

function is_int(x) {
    return is_integer(resolve(x));
};

function is_real(value) {
    return !isNaN(resolve(value));
};

function is_string(value) {
    return typeof resolve(value) === 'string';
};

function is_inf(value) {
    return !isFinite(resolve(value));
};

function is_nan(value) {
    return isNaN(resolve(value));
};

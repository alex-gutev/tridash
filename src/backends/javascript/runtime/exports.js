/**
 * exports.js
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

/* Export Runtime Symbols */

exports.Node = Node;
exports.NodeContext = NodeContext;

exports.set_values = set_values;

exports.Fail = Fail;

exports.Thunk = Thunk;
exports.CatchThunk = CatchThunk;
exports.resolve = resolve;


/* Functions */

exports.and = and;
exports.or = or;
exports.not = not;

exports.cast_int = cast_int;
exports.cast_real = cast_real;
exports.cast_string = cast_string;

exports.is_int = is_int;
exports.is_real = is_real;
exports.is_string = is_string;
exports.is_inf = is_inf;
exports.is_nan = is_nan;

exports.cons = cons;
exports.head = head;
exports.tail = tail;
exports.is_cons = is_cons;

exports.string_at = string_at;
exports.string_concat = string_concat;


/* Internal Use Functions */

exports.check_number = check_number;
exports.check_value = check_value;
exports.check_string = check_string;

exports.get_symbol = get_symbol;
exports.member = member;

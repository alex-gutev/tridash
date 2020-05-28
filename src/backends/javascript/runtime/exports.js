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


/* Tridash Modules */

exports.Module = TridashModule;
exports.Node = Node;
exports.NodeRef = NodeRef;


/* Thunks and Failure Types */

exports.Fail = Fail;

exports.Thunk = Thunk;
exports.resolve = resolve;


/* Functions */

exports.sub_neg = sub_neg;

exports.eq = eq;
exports.neq = neq;

exports.and = and;
exports.or = or;
exports.not = not;

exports.fail = fail;
exports.fail_type = fail_type;
exports.make_catch_thunk = make_catch_thunk;
exports.uncatch_thunk = uncatch_thunk;

exports.cast_int = cast_int;
exports.cast_real = cast_real;

exports.int_to_string = int_to_string;
exports.real_to_string = real_to_string;
exports.char_to_string = char_to_string;
exports.symbol_name = symbol_name;

exports.is_int = is_int;
exports.is_real = is_real;
exports.is_string = is_string;
exports.is_char = is_char;
exports.is_symbol = is_symbol;
exports.is_inf = is_inf;
exports.is_nan = is_nan;

exports.cons = cons;
exports.head = head;
exports.tail = tail;
exports.is_cons = is_cons;
exports.Empty = Empty;

exports.Char = Char;
exports.string_at = string_at;
exports.string_concat = string_concat;


/* Internal Use Functions */

exports.check_number = check_number;
exports.check_value = check_value;
exports.check_string = check_string;
exports.check_function = check_function;
exports.check_bool = check_bool;

exports.get_symbol = get_symbol;
exports.member = member;


/* Functional Utilities */

exports.mapply = mapply;


/* Builtin Failure Types */

exports.NoValue = NoValue;
exports.TypeError = TridashTypeError;
exports.IndexOutBounds = IndexOutBounds;
exports.InvalidInteger = InvalidInteger;
exports.InvalidReal = InvalidReal;
exports.ArityError = ArityError;

/**
 * queue.js
 *
 * Tridash WebAssembly Loader
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

/**
 * A simple implementation of an unbounded FIFO queue.
 */
function Queue() {
    this.head = null;
    this.tail = null;

    function QueueNode(elem) {
        this.elem = elem;
        this.next = null;
        return this;
    }

    this.enqueue = function(elem) {
        var new_node = new QueueNode(elem);

        if (this.tail) {
            this.tail.next = new_node;
            this.tail = new_node;
        }
        else {
            this.head = this.tail = new_node;
        }

        return elem;
    };
    this.dequeue = function() {
        if (this.head) {
            var elem = this.head.elem;
            this.head = this.head.next;

            if (!this.head) this.tail = null;

            return elem;
        }

        return null;
    };

    this.empty = function() {
        return this.head == null;
    };

    this.clear = function() {
        this.head = this.tail = null;
    };
};

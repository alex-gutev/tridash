/**
 * tridash.js
 *
 * Tridash JavaScript runtime library.
 *
 * Copyright 2017 Alexander Gutev
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
 * Tridash Namespace.
 */
function Tridash() {}

/**
 * Object storing publicly accessible nodes.
 */
Tridash.nodes = {};

/**
 * Runtime Node.
 *
 * Stores the node link information (dependencies, observers, wait
 * sets), and the node's runtime state.
 */
Tridash.Node = function() {
    /**
     * Node contexts.
     */
    this.contexts = {};

    /**
     * The queue to which path reservations are queued.
     */
    this.reserve_queue = new Tridash.Queue();

    /**
     * Node Value.
     */
    this.value = null;
    /**
     * True if there is an ongoing update loop.
     */
    this.running = false;
};

/**
 * Node Context.
 *
 * Contains the value computation function of a node at a particular
 * context.
 *
 * @param node        The node to which the context belongs.
 * @param context_id  Global unique context identifier.
 */
Tridash.NodeContext = function(node, context_id) {
    /**
     * The node to which this context belongs.
     */
    this.node = node;

    /**
     * Unique global context identifier.
     */
    this.context_id = context_id;

    /**
     * Value computation function. Takes an array of the operand
     * values as its first and only parameter.
     */
    this.compute = ([a]) => a;

    /**
     * Array of the values of the operands to the value computation
     * function.
     *
     * Each operand node stores its value directly at a particular
     * index within this array.
     */
    this.operands = [];

    /**
     * Array of observers.
     *
     * Each element is an array of two elements: the first element is
     * the node context of the observer node, the second element is
     * the index within the observer context's 'operands' array.
     */
    this.observers = [];
};

/**
 * Creates a new node context of node @a node, with a given number of
 * operands.
 *
 * @param node         The node to which the context belongs.
 * @param num_operands The number of operands of the context.
 * @param context_id   Global unique context identifier.
 *
 * @return The node context.
 */
Tridash.NodeContext.create = function(node, num_operands, context_id) {
    var context = new Tridash.NodeContext(node, context_id);

    context.operands.length = num_operands;
    context.operands.fill(null, 0);

    return context;
};

/**
 * Adds a new observer to the context.
 *
 * @param context The observer node's context.
 * @param index Index within the operands array of @a context.
 */
Tridash.NodeContext.prototype.add_observer = function(context, index) {
    this.observers.push([context, index]);
};

/**
 * Reserves a path through the node (of the context).
 *
 * @param start A promise which is resolved when the entire path has
 *    been determined.
 *
 * @param value A promise for the operand node's value.
 *
 * @param index Index, within the context's operands array, of the
 *    operand.
 *
 * @param visited Visited set.
 */
Tridash.NodeContext.prototype.reserve = function(start, value, index, visited = {}) {
    if (!visited[this.context_id]) {
        var promise = new Tridash.ValuePromise();

        promise.promise = promise.promise
            .then(() => value)
            .then((value) => this.operands[index] = value);

        var reserve = {
            context: this,
            reserved: promise,
            value: new Tridash.ValuePromise(),
            start: start.promise
        };

        visited[this.context_id] = reserve;

        this.reserveObservers(start, reserve.value.promise, visited);

        this.node.reserve_queue.enqueue(reserve);
        this.node.add_update();
    }
    else {
        var reserved = visited[this.context_id].reserved;

        reserved.promise = reserved.promise
            .then(() => value)
            .then((value) => this.operands[index] = value);
    }
};

/**
 * Reserves a path from a node, with this context, to the observer
 * nodes.
 *
 * @param start Promise which is resolved when the entire path has
 *    been determined.
 *
 * @param value Promise for the value of the node of this context.
 *
 * @param visited Visited set.
 */
Tridash.NodeContext.prototype.reserveObservers = function(start, value, visited) {
    this.observers.forEach(([obs, index]) => obs.reserve(start, value, index, visited));
};

/**
 * Computes the value of the node, using the context's value
 * computation function, with the current value of the operands.
 *
 * @return The new value of the node.
 */
Tridash.NodeContext.prototype.compute_value = function() {
    return this.compute(this.operands);
};



/* Update Loop */

/**
 * Runs the update loop if it is not already running.
 */
Tridash.Node.prototype.add_update = function() {
    if (!this.running) this.update();
};

/**
 * Runs the update loop of the node, until the reserve queue is empty.
 */
Tridash.Node.prototype.update = function() {
    this.running = true;

    var reserve = this.reserve_queue.dequeue();

    if (reserve) {
        var {context, reserved, start} = reserve;
        reserved.resolve(true);

        start.then(() => reserved.promise)
            .then(() => context.compute_value())
            .then((value) => {
                reserve.value.resolve(value);
                this.update_value(value);
            })
            .finally(this.update.bind(this));
    } else {
        this.running = false;
    }
};

/**
 * Method which is called after the new value of the node has been
 * computed.
 *
 * By default this method does nothing, it should be overridden if
 * the new value needs to be reflected somewhere.
 *
 * @param value The new value of the node.
 */
Tridash.Node.prototype.update_value = function(value) {};

/**
 * Sets the value of the node. The node must be an input node and must
 * have an input context.
 *
 * A path, throughout the entire graph, is reserved, starting at the
 * current node with the input context.
 */
Tridash.Node.prototype.set_value = function(value) {
    var start = new Tridash.ValuePromise();

    this.contexts["input"].reserve(start, value, 0);
    start.resolve(true);
};

/**
 * Reserves a path in the graph, starting at a particular set of input
 * nodes and sets the values of those input nodes.
 *
 * @param node_values Array of the input nodes and their values to
 *   set. Each element is an array of two elements: the input node and
 *   its value.
 */
Tridash.Node.set_values = function(node_values) {
    var start = new Tridash.ValuePromise();
    var visited = {};

    node_values.forEach(([node, value]) => {
        node.contexts["input"].reserve(start, value, 0, visited);
    });

    start.resolve(true);
};

/**
 * Used as an exception type, which is thrown in order to skip the
 * invocation of the set_value method. The exception is caught in
 * order to prevent errors showing up in the console.
 */
Tridash.EndUpdate = function() {};


/**
 * Creates a thunk for lazily computing a value. When the thunk
 * function is called for the first time @a compute is called and the
 * value returned is stored. Subsequent invocations of the thunk
 * function will simply return the stored value.
 *
 * @param compute Function of no arguments, which computes the value.
 *
 * @return The thunk function.
 */
Tridash.Thunk = function(compute) {
    var value;

    return () => {
        return value || (value = compute());
    };
};

/**
 * Value Promise.
 *
 * Creates a new promise object with resolve and reject methods.
 */
Tridash.ValuePromise = function() {
    this.promise = new Promise((resolve, reject) => {
        this.resolve = resolve;
        this.reject = reject;
    });
};

/**
 * A simple implementation of an unbounded FIFO queue.
 */

Tridash.Queue = function() {
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

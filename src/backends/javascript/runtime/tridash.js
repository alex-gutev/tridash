/**
 * tridash.js
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
 * Stores the runtime state of a node.
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
     * Array of the operand nodes.
     */
    this.operands = [];

    /**
     * Array of observer node contexts.
     */
    this.observers = [];

    /**
     * Array storing saved previous values.
     */
    this.saved_values = [];
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
 * Creates an input context for node @a node.
 *
 * @param node The node to which the context belongs.
 * @param context_id Global context identifier.
 *
 * @return The node context.
 */
Tridash.NodeContext.create_input = function(node, context_id) {
    var context = Tridash.NodeContext.create(node, 0, context_id);

    context.compute_value = (value) => {
        return context.compute([value]);
    };

    return context;
};

/**
 * Adds a new observer to the context.
 *
 * @param context The observer node's context.
 * @param index Index within the operands array of @a context.
 */
Tridash.NodeContext.prototype.add_observer = function(context, index) {
    this.observers.push(context);
    context.operands[index] = this.node;
};

/**
 * Creates a Reserve object.
 *
 * @param context The context through which to reserve a path.
 *
 * @param start Promise which is resolved when the context's
 *   dependency has computed its value.
 */
Tridash.Reserve = function(context, start) {
    /**
     * The context.
     */
    this.context = context;

    /**
     * Promise indicating when the node may begin computing its value,
     * however not necessarily all its dependency nodes have computed
     * their values.
     */
    this.start = start;

    /**
     * Promise which is resolved once all dependency nodes have
     * computed their value, thus the context can definitely begin
     * computing its value.
     */
    this.all_deps = start;

    /**
     * Promise which should be resolved by the node once it has
     * computed its value.
     */
    this.next = new Tridash.ValuePromise();


    /**
     * Promise indicating when the path through the node has been
     * reserved.
     *
     * This promise should be reserved when the node has dequeued this
     * reserve object from the queue.
     */
    this.path = new Tridash.ValuePromise();

    /**
     * Promise which is resolved when the path through all of the
     * context's observer nodes has been reserved.
     */
    this.observers = null;
};

/**
 * Adds a new dependency to the reserve object, for which the node
 * waits prior to computing its value.
 *
 * @param start Promise which is resolved once the dependency's value
 *   has been computed.
 */
Tridash.Reserve.prototype.add_dep = function(start) {
    // Save promise for previous dependencies
    var prev = this.all_deps;

    // Add finally handler which resolves to the promise of the
    // current dependency regardless of whether the promises of the
    // previous dependencies were resolved or rejected.

    // Add a final catch handler which resolves to the promise of the
    // previous dependency if the promise of the current dependency is
    // rejected.

    this.all_deps =
        this.all_deps.finally(() => start)
        .catch(() => prev);
};

/**
 * Reserves a path through the node (in the context).
 *
 * @param start Promise which once resolved, the node's dependency has
 *   computed its value.
 *
 * @param visited Visited set.
 */
Tridash.NodeContext.prototype.reserve = function(start, visited = {}) {
    if (!visited[this.context_id]) {
        var reserve = new Tridash.Reserve(this, start);

        visited[this.context_id] = reserve;

        reserve.observers = this.reserveObservers(reserve.next.promise, visited);

        this.node.reserve_queue.enqueue(reserve);
        this.node.add_update();

        return reserve.path.promise;
    }
    else {
        reserve = visited[this.context_id];

        reserve.add_dep(start);
        return reserve.path.promise;
    }
};

/**
 * Reserves a path from a node, in this context, to the observer
 * nodes.
 *
 * @param start Promise which is resolved when the dependency node has
 *   computed its value.
 *
 * @param visited Visited set.
 */
Tridash.NodeContext.prototype.reserveObservers = function(start, visited) {
    return Promise.all(this.observers.map((obs) => obs.reserve(start, visited)));
};

/**
 * Computes the value of the node, using the context's value
 * computation function, with the current value of the operands.
 *
 * @return The new value of the node.
 */
Tridash.NodeContext.prototype.compute_value = function() {
    return this.compute(this.operands.map((n) => n.value));
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
        var {context, start, observers, next} = reserve;

        reserve.path.resolve(true);

        observers.then(() => start)
            .then(() => reserve.all_deps)
            .then((value) => context.compute_value(value))
            .then((value) => {
                this.value = value;

                next.resolve(value);
                this.update_value(value);
            })
            .catch((e) => next.reject(e))
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
 * Adds a function which is called when the value of the node changes.
 *
 * @param fn A function of one argument - the new value of the node.
 */
Tridash.Node.prototype.add_watch = function(fn) {
    var update_value = this.update_value;

    this.update_value = (value) => {
        update_value(value);
        fn(value);
    };
};

/**
 * Sets the value of the node. The node must be an input node and must
 * have an input context.
 *
 * A path, throughout the entire graph, is reserved, starting at the
 * current node with the input context.
 */
Tridash.Node.prototype.set_value = function(value) {
    var start = new Tridash.ValuePromise();

    this.contexts["input"].reserve(start.promise);
    start.resolve(value);
};

/**
 * Reserves a path in the graph, starting at a particular set of input
 * nodes and sets the values of those input nodes.
 *
 * @param node_values Array of the input nodes and their values to
 *   set. Each element is an array of two elements: the input node and
 *   its value.
 */
Tridash.set_values = function(node_values) {
    var start = new Tridash.ValuePromise();
    var visited = {};

    node_values.forEach(([node, value]) => {
        node.contexts["input"].reserve(start.promise.then(() => value), visited);
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
    this.computed = false;

    this.compute = compute;
};

/**
 * Computes the value of the thunk if it has not already been
 * computed.
 *
 * @return The value of the thunk
 */
Tridash.Thunk.prototype.resolve = function() {
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
Tridash.resolve = function(thing) {
    while (thing instanceof Tridash.Thunk) {
        thing = thing.resolve();
    }

    return thing;
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


/** Functions */

/* Boolean Expressions */

Tridash.and = function(a, b) {
    return Tridash.resolve(a) ? b : false;
};
Tridash.or = function(a, b) {
    if ((a = Tridash.resolve(a)))
        return a;

    return b;
};


/* Type Conversions */

Tridash.castInt = function(x) {
    return parseInt(Tridash.resolve(x));
};
Tridash.castReal = function(x) {
    return parseFloat(Tridash.resolve(x));
};
Tridash.castString = function(x) {
    return String(Tridash.resolve(x));
};


/* Querying Types */

Tridash.isInteger = Number.isInteger || function(value) {
    return typeof value === 'number' &&
        isFinite(value) && Math.floor(value) === value;
};

Tridash.isInt = function(x) {
    return Tridash.isInteger(Tridash.resolve(x));
};

Tridash.isReal = function(value) {
    return !isNaN(Tridash.resolve(value));
};

Tridash.isString = function(value) {
    return typeof Tridash.resolve(value) === 'string';
};

Tridash.isInf = function(value) {
    return !isFinite(Tridash.resolve(value));
};

Tridash.isNaN = function(value) {
    return isNaN(Tridash.resolve(value));
};

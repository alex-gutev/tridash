/**
 * runtime.js
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

/* Node Object */

/**
 * Runtime Node.
 *
 * Stores the runtime state of a node.
 */
function Node() {
    /**
     * Node contexts.
     */
    this.contexts = {};

    /**
     * The queue to which path reservations are queued.
     */
    this.reserve_queue = new Queue();

    /**
     * Node Value.
     */
    this.value = null;
    /**
     * True if there is an ongoing update loop.
     */
    this.running = false;
};


/* Node Context */

/**
 * Node Context.
 *
 * Responsible for computing the node's value at a particular moment
 * in time.
 *
 * @param node        The node to which the context belongs.
 * @param context_id  Global unique context identifier.
 */
function NodeContext(node, context_id) {
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
NodeContext.create = function(node, num_operands, context_id) {
    var context = new NodeContext(node, context_id);

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
NodeContext.create_input = function(node, context_id) {
    var context = NodeContext.create(node, 0, context_id);

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
NodeContext.prototype.add_observer = function(context, index) {
    this.observers.push(context);
    context.operands[index] = this.node;
};


/* Reserving Paths through the Graph */

/**
 * Creates a Reserve object.
 *
 * @param context The context through which to reserve a path.
 *
 * @param start Promise which is resolved when the context's
 *   dependency has computed its value.
 */
function Reserve(context, start) {
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
    this.next = new ValuePromise();


    /**
     * Promise indicating when the path through the node has been
     * reserved.
     *
     * This promise should be reserved when the node has dequeued this
     * reserve object from the queue.
     */
    this.path = new ValuePromise();

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
Reserve.prototype.add_dep = function(start) {
    this.all_deps = this.all_deps.then(() => start);
};

/**
 * Reserves a path through the node (in the context).
 *
 * @param start Promise which once resolved, the node's dependency has
 *   computed its value.
 *
 * @param visited Visited set.
 */
NodeContext.prototype.reserve = function(start, visited = {}) {
    if (!visited[this.context_id]) {
        var reserve = new Reserve(this, start);

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
NodeContext.prototype.reserveObservers = function(start, visited) {
    return Promise.all(this.observers.map((obs) => obs.reserve(start, visited)));
};


/* Update Loop */

/**
 * Computes the value of the node, using the context's value
 * computation function, with the current value of the operands.
 *
 * @return The new value of the node.
 */
NodeContext.prototype.compute_value = function() {
    return this.compute(this.operands.map((n) => n.value));
};


/**
 * Runs the update loop if it is not already running.
 */
Node.prototype.add_update = function() {
    if (!this.running) this.update();
};

/**
 * Runs the update loop of the node, until the reserve queue is empty.
 */
Node.prototype.update = function() {
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
Node.prototype.update_value = function(value) {};


/* Observing Changes in Node Value */

/**
 * Adds a function which is called when the value of the node changes.
 *
 * @param fn A function of one argument - the new value of the node.
 */
Node.prototype.add_watch = function(fn) {
    var update_value = this.update_value;

    this.update_value = (value) => {
        update_value(value);
        fn(value);
    };
};


/* Setting Initial Values */

/**
 * Sets the value of the node. The node must be an input node and must
 * have an input context.
 *
 * A path, throughout the entire graph, is reserved, starting at the
 * current node with the input context.
 */
Node.prototype.set_value = function(value) {
    var start = new ValuePromise();

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
function set_values(node_values) {
    var start = new ValuePromise();
    var visited = {};

    node_values.forEach(([node, value]) => {
        node.contexts["input"].reserve(start.promise.then(() => value), visited);
    });

    start.resolve(true);
};


/* Fail Exception Type */

/**
 * Used as the fail exception type, which indicates that a value
 * computation has failed.
 */
function Fail() {};


/* Thunk */

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
function Thunk(compute) {
    this.computed = false;

    this.compute = compute;
};

/**
 * A CatchThunk is a Thunk which always has a "catch value" associated
 * with it which is returned if the compute function throws the Fail
 * exception.
 *
 * @param compute The thunk's compute function.
 *
 * @param catch_value The value to return if @a compute throws a Fail
 *    exception. May be a value, Thunk or CatchThunk.
 */
function CatchThunk(compute, catch_value) {
    Thunk.call(this, compute);

    this.catch_value = catch_value;
};
CatchThunk.prototype = Object.create(Thunk.prototype);
CatchThunk.prototype.constructor = CatchThunk;

/**
 * Creates a thunk which throws a Fail exception.
 *
 * @return The thunk.
 */
function FailThunk() {
    return new Thunk(() => {
        throw new Fail();
    });
}

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
    // Catch value to return in case a Fail exception is thrown. If
    // null, then there is no catch value and the exception is
    // rethrown.
    var catch_value = null;

    while (thing instanceof Thunk) {
        if (thing instanceof CatchThunk) {
            catch_value = combine_catch_thunk(thing.catch_value, catch_value);
        }

        try {
            thing = thing.resolve();
        }
        catch (e) {
            if (e instanceof Fail && catch_value !== null) {
                thing = catch_value;
                catch_value = null;
            }
            else {
                throw e;
            }
        };
    }

    return thing;
}

/**
 * Creates a CatchThunk with a compute function that returns @a
 * try_value and a catch value of @a catch value.
 *
 * @param try_value The value which should be returned by the thunk's
 *    compute function. May be a value, Thunk or CatchThunk.
 *
 * @param catch_value The catch value.
 *
 * @return The CatchThunk. If try_value is a value, it is returned
 *     without creating a new thunk.
 */
function combine_catch_thunk(try_value, catch_value) {
    if (catch_value === null) {
        return try_value;
    }
    if (try_value instanceof CatchThunk) {
        return new CatchThunk(try_value.compute,
                              combine_catch_thunk(try_value.catch_thunk, catch_value));
    }
    else if (try_value instanceof Thunk) {
        return new CatchThunk(try_value.compute, catch_value);
    }
    else {
        return try_value;
    }
}

/* Promise */

/**
 * Value Promise.
 *
 * Creates a new promise object with resolve and reject methods.
 */
function ValuePromise() {
    this.promise = new Promise((resolve, reject) => {
        this.resolve = resolve;
        this.reject = reject;
    });
};

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
     *
     * Each element is an array in which the first element is the
     * observer node context, and the second element is the index of
     * this node within the context's operands array.
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
    return NodeContext.create(node, 0, context_id);
};

/**
 * Adds a new observer to the context.
 *
 * @param context The observer node's context.
 * @param index Index within the operands array of @a context.
 */
NodeContext.prototype.add_observer = function(context, index) {
    this.observers.push([context, index]);
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
     * Promise which is resolved when the entire path has been
     * reserved. Once this promise is resolved `operands' is
     * guaranteed to contain the updated value of each 'dirtied'
     * operand node.
     */
    this.start = start;

    /**
     * Array of the values of the operand nodes
     */
    this.operands = context.operands.map((n) => n.next_value);

    /**
     * Thunk which computes the value of the node.
     */
    this.value = new Thunk(() => {
        return context.compute_value(this.operands);
    });

    /**
     * Promise which is resolved when the value of this node is
     * updated.
     */
    this.value_promise = new ValuePromise();


    /**
     * Promise indicating when the path through the node has been
     * reserved, that is the update method, for this reserve, has been
     * called.
     *
     * This promise should be reserved when the node has dequeued this
     * reserve object from the queue.
     */
    this.path = new ValuePromise();

    /**
     * Promise which is resolved when the path through all of the
     * context's observer nodes has been reserved.
     */
    this.observers = Promise.resolve(true);

    /**
     * Optional promise which for which the update loop waits to be
     * resolved prior to updating its value.
     */
    this.preconditions = null;

    /**
     * Flag for whether this reserve object has been queued to the
     * node's observers.
     */
    this.queued = false;
};

/**
 * Add this Reserve object to the reserve queue of @a node if it has
 * not been queue already.
 *
 * @param node The node, the reserve queue, of which, to add this
 *   Reserve object.
 */
Reserve.prototype.queue = function(node) {
    if (!this.queued) {
        // Set next_value promise of node to indicate that the node's
        // value is being updated.
        node.next_value = this.value_promise.promise;

        node.reserve_queue.enqueue(this);
        node.add_update();

        this.queued = true;
    }
};

/**
 * Adds a promise which is to be resolved prior to updating the node's
 * value
 *
 * @param condition A promise.
 */
Reserve.prototype.add_precondition = function(condition) {
    this.preconditions = this.preconditions ?
        this.preconditions.then(() => condition) :
        condition;
};


/**
 * Reserves a path through the node (in the context).
 *
 * @param start Promise which once resolved, indicates that `reserve'
 *   has been called for each dirtied operand node.
 *
 * @param index Index of the operand node within the context's
 *   operands array.
 *
 * @param path Path promise of the dependency node.
 *
 * @param value Promise which is resolved with the value of the
 *   operand when it is computed.
 *
 * @param visited Visited set.
 *
 * @param weak True if the binding is a weak binding, in which case
 *   the Reserve object is not added to the node's reserve queue.
 *
 * @return Promise which is resolved once the path through this node
 *   has been resolved (the reserve object has been dequeued in the
 *   node's update loop).
 */
NodeContext.prototype.reserve = function(start, index, path, value, visited = {}, weak = false) {
    if (!visited[this.context_id]) {
        var reserve = new Reserve(this, start);

        visited[this.context_id] = reserve;

        // Do not set weak operands as if they are part of a cycle,
        // they may cause an infinite promise waiting chain.

        // This assumes that operands which are weakly bound are not
        // actually used within the context's value function.
        reserve.operands[index] = weak ? null : value;

        this.reserve_hook(reserve, index, path, true);

        if (!weak) {
            reserve.observers = this.reserveObservers(start, reserve.value_promise.promise, reserve.path.promise, visited);
            reserve.queue(this.node);

            return reserve.path.promise;
        }
    }
    else {
        reserve = visited[this.context_id];

        // Do not set weak operands as if they are part of a cycle,
        // they may cause an infinite promise waiting chain.

        // This assumes that operands which are weakly bound are not
        // actually used within the context's value function.
        reserve.operands[index] = weak ? null : value;

        this.reserve_hook(reserve, index, path, false);

        if (!weak) {
            if (!this.queued)
                reserve.observers = this.reserveObservers(start, reserve.value_promise.promise, reserve.path.promise, visited);

            reserve.queue(this.node);
            return reserve.path.promise;
        }
    }

    return Promise.resolve(true);
};

/**
 * Hook function which is called when 'reserve' is called.
 *
 * This allows a node to add preconditions to a reserve.
 *
 * @param reserve The reserve object.
 *
 * @param index Index of the dependency node within the operands
 *   array.
 *
 * @param path Path promise of the dependency node.
 *
 * @param first If true the hook is called for the first time for this
 *   reserve object.
 */
NodeContext.prototype.reserve_hook = function(reserve, index, path, first) {};

/**
 * Reserves a path from a node, in this context, to the observer
 * nodes.
 *
 * @param start Promise which once resolved, indicates that `reserve'
 *   has been called for each dirtied operand node.
 *
 * @param value Promise which is resolved with the value of the node
 *   when it is computed.
 *
 * @param path Promise which is resolved when the node's update loop
 *   is run.
 *
 * @param visited Visited set.
 *
 * @return Promise which is resolved when a path has been reserved
 *   through each of the observers.
 */
NodeContext.prototype.reserveObservers = function(start, value, path, visited) {
    return Promise.all(
        this.observers.map(([obs, index]) => obs.reserve(start, index, path, value, visited))
    );
};


/* Update Loop */

/**
 * Computes the value of the node, using the context's value
 * computation function, with the current value of the operands.
 *
 * @param operands Values (either immediate or thunks) of the operand
 *   nodes.
 *
 * @return The new value of the node.
 */
NodeContext.prototype.compute_value = function(operands) {
    return this.compute(operands);
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
        var {context, start, observers, value} = reserve;

        observers.then(() => start)
            .then(() => {
                reserve.path.resolve(true);
                return reserve.preconditions;
            })
            .then(() => {
                // Wait for operand nodes to resolve. When resolved
                // update node value.
                var p = Promise.all(reserve.operands).then((operands) => {
                    reserve.operands = operands;

                    this.value = value;
                    this.update_value(value);

                    return value;
                });

                // Resolve node value promise with the operands nodes
                // promise. This ensures that cyclic bindings will not
                // result in a circular wait.
                reserve.value_promise.resolve(p);

                return p;
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
 *
 * @param value The value.
 */
Node.prototype.set_value = function(value) {
    var start = new ValuePromise();

    this.contexts["input"].reserve(start.promise, 0, null, value);
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
function set_values(node_values) {
    var start = new ValuePromise();
    var visited = {};

    node_values.forEach(([node, value]) => {
        node.contexts["input"].reserve(start.promise, 0, null, value, visited);
    });

    start.resolve(true);
};


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

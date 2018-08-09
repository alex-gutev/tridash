/**
 * metalink.js
 *
 * Metalink JavaScript runtime library.
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
 * Runtime Node.
 *
 * Stores the node link information (dependencies, observers, wait
 * sets), and the node's runtime state.
 */
function MetaLinkNode() {
    /* Link Information */

    /**
     * Array of dependency queues.
     */
    this.dependencies = [];
    /**
     * The NodeLink object containing the observers of the node.
     */
    this.observers = null;

    /**
     * Value computation function.
     */
    this.compute = ([a]) => a;


    /* Wait Information */

    /**
     * Array of nodes which should be informed when the node's value
     * will change.
     */
    this.wait_nodes = [];


    /* State Information */

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
 * Creates PromiseQueue's and adds them to the 'dependencies' array.
 *
 * @param num The number of queues to create.
 */
MetaLinkNode.prototype.make_dependencies = function(num) {
    for (var i = 0; i < num; i++) {
        this.dependencies.push(new PromiseQueue(this));
    }
};

/**
 * Creates a new NodeLink object for the observers and stores it in
 * 'observers'.
 *
 * @param observers An array where each element is of the form
 *    [observer, index] where observer is the observer node and index
 *    is the index of the queue, within the observers 'dependencies'
 *    array, to which this nodes value will be queued.
 */
MetaLinkNode.prototype.add_observers = function(observers) {
    this.observers = new NodeLink(observers.map(([obs, ind]) => obs.dependencies[ind]));
};


/* Update Loop */

/**
 * Runs the update loop if it is not already running.
 */
MetaLinkNode.prototype.add_update = function() {
    if (!this.running) this.update();
};

/**
 * Used as an exception type, which is thrown in order to skip the
 * invocation of the set_value method. The exception is caught in
 * order to prevent errors showing up in the console.
 */
function EndUpdate() {}

/**
 * Runs the update loop of the node.
 *
 * If at least one dependency queue has a queued value, a value is
 * dequeued from each dependency, the new value is computed and
 * dispatched, and the next iteration of the update loop is
 * scheduled. If no dependency queue has a new value, the update loop
 * is terminated and the running flag is cleared.
 */
MetaLinkNode.prototype.update = function() {
    this.running = true;

    var deps = this.dependencies;
    if (deps.some(dep => dep.has_value())) {
        Promise.all(deps.map(dep => dep.dequeue()))
            .then(this.compute.bind(this))
            .then(this.set_value.bind(this))
            .catch((e) => {
                if (!(e instanceof EndUpdate))
                    throw e;
            })
            .finally(this.update.bind(this));
    }
    else {
        this.running = false;
    }
};

/**
 * Updates the node's value and dispatches it.
 *
 * @param value The new value of the node.
 */
MetaLinkNode.prototype.set_value = function(value) {
    this.value = value;

    this.send_wait();
    this.dispatch(value);
};


/* Waiting for changes */

/**
 * Informs each node in 'wait_nodes' to wait for a new value (due to
 * the this node's value having changed).
 */
MetaLinkNode.prototype.send_wait = function() {
    this.wait_nodes.forEach(node => node.wait());
};

/**
 * Informs the node to wait for a new value. A pending promise is
 * queued to each observer's dependency queue.
 */
MetaLinkNode.prototype.wait = function() {
    this.observers.promise_value();
};


/* Dispatching values */

/**
 * Dispatches the node's value to each observer nodes.
 */
MetaLinkNode.prototype.dispatch = function(value) {
    this.observers.dispatch(value);
};


/* Links between nodes. */

/**
 * Node Link.
 *
 * Manages dispatching values to a set of observers.
 *
 * @param observers Array of the dependency queues, of each observer
 *    node, to which dispatched values will be queued.
 */
function NodeLink(observers) {
    /**
     * Array of observer dependency queues.
     */
    this.observers = observers;
    /**
     * Queue of pending promises which have been queued to the
     * observers' dependency queues.
     */
    this.promises = new Queue();
};

/**
 * Returns either the next pending promise, in the queue, or creates a
 * new promise and queues it to the observer's dependency queues.
 *
 * @return The ValuePromise object.
 */
NodeLink.prototype.next_promise = function() {
    return this.promises.dequeue() || this.new_promise();
};

/**
 * Creates a new ValuePromise object and queues it to each observer's
 * dependency queue.
 *
 * @return The ValuePromise object.
 */
NodeLink.prototype.new_promise = function() {
    var promise = new ValuePromise();

    this.observers.forEach(obs => obs.enqueue(promise.promise));
    return promise;
};

/**
 * Creates a new pending promise, enqueues it to each observer's
 * dependency queue and enqueues it to the 'promises' queue.
 */
NodeLink.prototype.promise_value = function() {
    this.promises.enqueue(this.new_promise());
};

/**
 * Dispatches a value to each observer, and runs each observer's
 * update loop.
 */
NodeLink.prototype.dispatch = function(value) {
    this.next_promise().resolve(value);
    this.observers.forEach(obs => obs.add_update());
};

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
    var value;

    return () => {
        return value || (value = compute());
    };
}


/* Value Promise */

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
}


/* Dependency Queues */

/**
 * Promise Queue.
 *
 * @param target The target node - the of which this queue is a
 *    dependency queue.
 */
function PromiseQueue(target) {
    /**
     * The target node - the node of which this queue is a dependency
     * queue.
     */
    this.target = target;

    /**
     * The last promise dequeued.
     */
    this.last_value = null;
    /**
     * Queue of promises.
     */
    this.promises = new Queue();
}

/**
 * Adds a new promise to the queue..
 *
 * @param promise The Promise.
 */
PromiseQueue.prototype.enqueue = function(promise) {
    this.promises.enqueue(promise);
};

/**
 * Returns true if the promise queue is not empty.
 */
PromiseQueue.prototype.has_value = function() {
    return !this.promises.empty();
};

/**
 * Dequeues a promise from the queue or returns the last promise
 * dequeued.
 *
 * @return The Promise object.
 */
PromiseQueue.prototype.dequeue = function() {
    var promise = this.promises.dequeue();

    if (promise) {
        return (this.last_value = promise);
    }
    else {
        return this.last_value;
    }
};

/**
 * Runs the update loop of the target node.
 */
PromiseQueue.prototype.add_update = function() {
    this.target.add_update();
};


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
}


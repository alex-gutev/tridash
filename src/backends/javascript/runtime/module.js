/**
 * module.js
 *
 * Tridash JavaScript runtime library.
 *
 * Copyright 2020 Alexander Gutev
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
 * Stores the state of the nodes of a Tridash module and provides
 * functionality for changing their values.
 */
class TridashModule {
    /**
     * Constructor
     *
     * @param compute Main state computation function.
     */
    constructor(compute) {
        this.compute = compute;
        this.state = {};

        this.watches = {};
    }

    /**
     * Set the value of a node.
     *
     * @param index Node value index
     * @param input_index Node input context index
     * @param value The value
     */
    set_value(index, input_index, value) {
        this.state[index] = value;
        this.update([input_index]);
    }

    /**
     * Set the values of a set of nodes, simultaneously.
     *
     * Each element of @a values is an array of the form [input_index,
     * value_index, value] where value_index is the node's values
     * index, input_index is the index of the node's input context and
     * value is the value to which to set the node.
     *
     * @param values Array of values
     */
    set_values(values) {
        values.forEach(([_, index, value]) => {
            this.state[index] = value;
        });

        this.update(values.map(([index]) => index));
    }

    /**
     * Set the values of a set of nodes, simultaneously.
     *
     * Each element of @a values is an array of the form [node, value]
     * where node is the Node object and value is the value to which
     * to set the node.
     *
     * @param values Array of values
     */
    set_node_values(values) {
        this.set_values(
            values.map(([node, value]) => {
                return [node.input_index, node.node_index, value];
            })
        );
    }

    /**
     * Retrieve the value of a node.
     *
     * @param index Node value index.
     *
     * @return The node's value.
     */
    get_value(index, resolve_value = true) {
        var value = this.state[index];
        return resolve_value ? resolve(value) : value;
    }

    /**
     * Recompute the state of the module.
     *
     * @param dirtied Array of indices of the input contexts of the
     *   nodes which have had their values changed.
     */
    update(dirtied) {
        var { state, changed } = this.compute(dirtied, this.state);
        this.state = state;

        this.notify_changes(changed);
    }


    /* Notifying of Value Changes */

    /**
     * Add a callback function which is called whenever the value of a
     * node changes.
     *
     * @param Node value index.
     * @param fn The function.
     */
    watch_node(index, fn) {
        if (!(this.watches[index]))
            this.watches[index] = [];

        this.watches[index].push(fn);
    }


    /**
     * Call all callback functions for the nodes which were changed
     * during the last state update.
     *
     * @param changed Object in which the keys are the node value
     *   indices of the changed nodes.
     */
    notify_changes(changed) {
        Object.keys(changed).forEach((index) => {
            var watches = this.watches[index];

            if (watches) {
                this.notify_watches(watches, this.state[index]);
            }
        });
    }

    /**
     * Call all callback functions for a particular node.
     *
     * @param watches Array of callback functions to call.
     * @param value New node value.
     */
    notify_watches(watches, value) {
        watches.forEach((f) => {
            f(value);
        });
    }
}

/**
 * Provides convenient set_value and get_value methods which call
 * TridashModule.set_value and TridashModule.get_value with the node's
 * indicies.
 */
class Node {
    /**
     * Constructor
     *
     * @param module The TridashModule object.
     *
     * @param node_index Node value index.
     *
     * @param input_index Index of the node's input context. If the
     *   node does not have an input context, this argument can be
     *   omitted.
     */
    constructor(module, node_index, input_index) {
        this.module = module;
        this.node_index = node_index;
        this.input_index = input_index;
    }

    /**
     * Set the value of the node and update the state of the module.
     *
     * @param value The new node value.
     */
    set_value(value) {
        this.module.set_value(this.node_index, this.input_index, value);
    }

    /**
     * Retrieve the node's value.
     *
     * @param resolve_value If true the node's value is resolved prior
     *   to being returned.
     *
     * @return The node's value.
     */
    get_value(resolve_value = true) {
        return this.module.get_value(this.node_index, resolve_value);
    }

    /**
     * Add a callback function which is called whenever the value of
     * the node changes.
     *
     * @param f The callback function.
     */
    watch(f) {
        this.module.watch_node(this.node_index, f);
    }
}

/**
 * Object serving as the "node" object in a raw node reference.
 */
class NodeRef {
    /**
     * Constructor
     *
     * @param index Node value index.
     */
    constructor(index) {
        this.index = index;
    }
}

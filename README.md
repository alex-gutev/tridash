# Tridash

Tridash is a programming language based on bindings.

The fundamental idea of Tridash is that an application is made up of a
number of components, called nodes in Tridash terminology. Each node
has a value, comprising the node's state, which may depend on the
value's of other nodes. This dependency on other nodes is referred to
as a binding.

A node's value is specified by a function which may depend on, as
operands, the values of other nodes. Whenever the values of the
operand nodes change, the function is reevaluated and the value of the
node is updated. It is this dependency, between a node and its operand
nodes, and the automatic updating of a node's value which is referred
to as a binding.


## Examples

_The purpose of these examples is to briefly demonstrate the
capabilities of the language. Detailed documentation will be available
shortly in the wiki._

### Nodes

A node is specified by its name, which can contain any sequence of
characters excluding spaces, the dot, comma, parenthesis, braces and
quotes.

Examples of valid node names:

    a
    node
    some-node
    node1
    1node


### Simple Bindings

Simple bindings between nodes are established using the bind `->`
operator. Whenever the value of the node on the left-hand side of the
operator (the dependency node) changes, the value of the node on the
right-hand side (the observer node) is updated to it.

Example:

    a -> b

Node `a` is bound to node `b` thus whenever the value of node `a`
changes the value of node `b` is updated to it.

**Note:** The space between the nodes and the `->` operator is required
otherwise, due to the flexible node naming rules, `a->b` would be
interpreted as the name of a single node.


### Expressions

Expressions consisting of a function/operator and one or more operand
nodes are themselves nodes, referred to as *expression
nodes*. Bindings between the operand nodes and expression node are
established implicitly. Whenever the value of at least one of the
operand nodes changes, the expression is reevaluated and the value of
the expression node is updated..

Examples:

    a + b

An expression node `a + b` is created with the operand nodes `a` and
`b` implicitly bound to it. When the value of node `a` or `b` changes,
the expression `a + b` is reevaluated and the value of the expression
node is updated.

*Note:* `+` is an infix operator, the expression can alternatively be
written with the operator in prefix position as follows: `+(a, b)`.

Examples of expression nodes:

    a + b * c
    func(a, b, c)
    int(node)


### Conditions

Simple conditions are specified by binding a node to the binding
expression node.

e.g.

    condition -> (a -> b)

Whenever the node `condition`'s value changes to true, node `b`'s
value is updated to the last value of node `a`.


More complicated conditions can be specified by `case` expression,
which takes the following form:

    case (
       condition1 : value1,
       condition2 : value2,
       else-value
    ) -> node


If the value of node `condition1` is true, the result of the `case`
expression is the value of node `value1`, else if the value of
`condition2` is true, the result of the expression is the value of
`value2` otherwise the result of the expression is the value of
`else-value`.


### Defining Functions

Functions are defined using the `:` special operator.

    func(arg1, arg2, ...) : {
        ....
    }

The function name is specified followed by the argument list in
parenthesis, followed by the `:` operator and the function body in
braces `{`. The function body consists of one or more node
declarations/expression, with the value of the last expression being
the value of the function. If the function consists of only one
function the braces may be omitted.

Each argument `arg1`, `arg2`, ... is itself a node which can be bound
to other nodes in the function body. Nodes are automatically created
within the body of the function, when first referenced, and are only
visible within the function.

Examples of functions:

    increment(n) : n -1

    # Factorial

    factorial(n) : {
      case (
        n > 0 : n * factorial(n - 1),
        1
      )
    }

    # Tail-recursive factorial

    factorial(n) : {
      iter(n, acc) : {
        case (
          n > 0 : iter(n - 1, n * acc),
          acc
        )
      }

      iter(n, 1)
    }

The last example demonstrates iteration by tail recursion.

# json-lisp

A Scheme-like Lisp whose syntax is JSON lists.

## Getting Started

### Prerequisites

Building and running the `json-lisp` project requries the following dependencies:

- Rust

Rust can be installed by following the instructions on the Rust project's
[homepage](https://www.rust-lang.org/tools/install).

### Running the Project

The `json-lisp` project provides a simple, REPL-like environment. To build the
project and run the REPL:

```shell
cargo run
```

## The Language

### Primitive Expressions

The simplist possible expressions are primitive expressions. Primitive
expressions evaluate to themselves:

```shell
> true
Boolean(true)
> false
Boolean(false)
> 3.14
Number(3.14)
> "\"hello world\""
String("hello world")
> []
List([])
```

### Symbol Expressions

Symbol expressions evaluate to the value bound to the symbol in the environment. For example, if the symbol `x` is bound to the value `5`:

```shell
> "x"
Number(5.0)
```

### List Expressions

List expressions are catagorised into categorized into normal form and special
form expressions. If first element is a keyword (`define` or `lambda`) then the
expression is a special form and evaluated according to the semantics of the
corresponding special form.

All other list expressions are normal form expressions. Normal form expressions
are evaluated by first recursivly evaluating each of the expressions in the
list, starting from the second element. The resultant expressions are then
passed as arguments to the procedure defined by the first list element.

#### Defining Variables

Top level variables bindings are defined using the `define` special form:

```shell
> ["define", "x", "3"]
Boolean(true)
> "x"
Number(3.0)
```

Strings are defined by double quoting the string value:

```shell
> ["define", "x", "\"hello world\""]
Boolean(true)
> "x"
String("hello world")
```

Variable values can themselves be expressions:

```shell
> ["define", "x", ["+", ["-", 5, 2], 8]]
Boolean(true)
> "x"
Number(11.0)
```

#### Simple Expressions

```shell
> ["+", ["-", ["*", 2, 5], ["+", 3, 2]], 3]
Number(8.0)
```

#### Conditional Expressions

```shell
> ["if", ["and", true, false], ["+", 2.0, 3.0], ["-", 2.0, 3.0]]
Number(-1.0)
```

#### Defining Procedures

Procedures are defined using the `lambda` special form. The `lambda` special
form can be used to define anonymous procedures:

```shell
> [["lambda", ["x"], ["*", "x", "x"]], 5]
Number(25.0)
```

The `define` special form can be used to bind `lambda` expressions to symbols.

To define a procedure called `square` that takes a single argument `x` and
returns the square of that value:

```shell
> ["define", "square", ["lambda", ["x"], ["*", "x", "x"]]]
Boolean(true)
> ["square", 2]
Number(4.0)
```

Procedures (lambdas) are values and can be passed around like normal variables:

```shell
> ["define", "p", ["lambda", ["x"], ["*", "x", "x"]]]
Boolean(true)
> ["define", "g", ["lambda", ["p", "x"], ["p", "x"]]]
Boolean(true)
> ["g", 25]
missing argument
> ["g", "p", 25]
Number(625.0)
```

### Putting It All Together

```shell
> ["define", "x", 3]
Boolean(true)
> ["define", "square", ["lambda", ["x"], ["*", "x", "x"]]]
Boolean(true)
> ["square", "x"]
Number(9.0)
```

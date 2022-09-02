# json-lisp

A Lisp whose syntax is JSON lists.

## Getting Started

### Defining Variables

Top level variables bindings are defined using the `define` keyword:

```shell
> ["define", "x", "3"]
Boolean(true)
> "x"
String("3")
```

Variable values can themselves be expressions:

```shell
> ["define", "x", ["+", ["-", 5, 2], 8]]
Boolean(true)
> "x"
Number(11.0)
```

### Simple Expressions

```shell
> ["+", ["-", ["*", 2, 5], ["+", 3, 2]], 3]
Number(8.0)
```

### Conditional Expressions

```shell
> ["if", ["and", true, false], ["+", 2.0, 3.0], ["-", 2.0, 3.0]]
Number(-1.0)
```

### Defining Procedures

Procedures are defined using the `lambda` keyword.

The `lambda` keyword can be used to define anonymous procedures:

```shell
> [["lambda", ["x"], ["*", "x", "x"]], 5]
Number(25.0)
```

The `define` keyword can be used to bind `lambda` expressions to symbols. To define a procedure called `square` that
takes a single argument `x` and returns the square of that value:

```shell
> ["define", "square", ["lambda", ["x"], ["*", "x", "x"]]]
Boolean(true)
> ["square", 2]
Number(4.0)
```

Procedures are values and can be passed around like normal variables:

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

# json-lisp

A Lisp whose syntax is JSON lists.

## Getting Started

### Defining Variables

Variables are defined using the `define` keyword:

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

Procedures are defined using the `lambda` keyword. To define a procedure called
`square` that takes a single argument `x` and returns the square of that value:

```shell
> ["define", "square", ["lambda", ["x"], ["*", "x", "x"]]]
Boolean(true)
> ["square", 2]
Number(4.0)
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

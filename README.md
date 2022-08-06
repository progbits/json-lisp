# json-lisp

A Lisp that is also valid JSON.


## Getting Started

### Defining Variables

Variables are defined using the `define` keyword. To define a variable `x` who's value is `3`:

```json
["define", "x", "3"]
```

### Defining Procedures

Procedures are defined using the `lambda` keyword. To define a procedure called
`square` that takes a single argument `x` and returns the square of that value:

```json
["define", "foo", ["lambda", ["x"], ["*", "x", "x"]]]
```


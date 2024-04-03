# Meow Reference

## Introduction

Meow is a language designed with front-end development in mind. It's designed to be quick, easy and simple to write.

### Comments

Meow support 2 types of comments:

- Normal comments, which start with `//` and will be ignored by the transpiler.
- Doc comments, which start with `///` will be used to generate documentation.

### Primitive Types

| Type    | JavaScript Type | Description                                                                          |
|---------|-----------------|--------------------------------------------------------------------------------------|
| int     | number          | Positive or negative integer ranging from `Number.MIN_VALUE` to `Number.MAX_VALUE`   |
| float   | number          | Positive or negative float ranging from `Number.MIN_VALUE` to `Number.MAX_VALUE`     |
| string  | string          | Textual data encoded in UTF-16                                                       |
| bool    | boolean         | Boolean values `true` or `false`                                                     |
| nothing | null/undefined  | This represent Nothing. It will be transpiled as null or undefined in the final code |

### Variable

Variables are declared using the `let` keyword and are immutable by default. You can use add the `mut` keyword after `let` to make the variable mutable.

The type of the variable can be specified at the end or omitted to be inferred by the transpiler.

Examples:

```
let name: string; // Explitcit type.
let name = 'Bob'; // Inferred as string by transpiler.
```

### Function

Functions are first-class entities in Meow. Thus, it can be assigned to a variable & passed in functions.

Examples:

```
let myFunction = fn() { ... };

// Function that doesn't take in any arguments can be simplified to:
let myFunction = fn {}

let functionThatReturn = fn(arg: string) -> string {}
let functionThatReturn = fn -> string {}

let find = fn(array: string[], pred: fn(string) -> bool) {}
```
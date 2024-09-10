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

Variable are always immutable in meowscript. You can declare a variable with the syntax:

```
<variable_name> :: <type> = <value>
```

The type of the variable can be specified at the end or omitted to be inferred by the transpiler.

Examples:

```
name :: string = 'Bob'; // Explitcit type.
name = 'Bob'; // Inferred as string by compiler.
```

### Function

Functions are first-class entities in Meow. Thus, it can be assigned to a variable & passed in functions.

Examples:

```
myFunction :: fn() = { ... };

// Function that doesn't take in any arguments can be simplified to:
myFunction :: fn = {}

functionThatReturn :: fn(arg: string) -> string = {}
functionThatReturn :: fn -> string = {}

find :: fn(array: string[], pred: fn(string) -> bool) = {}
```
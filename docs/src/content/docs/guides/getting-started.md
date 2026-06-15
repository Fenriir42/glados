---
title: Getting Started
description: Learn how to write your first Quant program
---

## Introduction

Quant is a statically-typed, compiled language with C-like syntax. It features a powerful type system, standard library modules, and modern programming constructs.

## Your First Program

A simple "Hello, World!" program:

```quant
fn main() -> void {
    println("Hello, World!");
}
```

### Breaking It Down

- **`fn main() -> void`**: Entry point , every program starts here
- **`println(...)`**: Built-in print function with a trailing newline
- **`;`**: All statements end with a semicolon

## Basic Program Structure

Every Quant program consists of optional imports followed by function definitions:

```quant
import math

fn main() -> void {
    r: float = math.sqrt(16.0);
    println(r);   // 4.0
}
```

## Import Styles

Quant supports two import styles.

### Module Import

```quant
import math

fn main() -> void {
    x: float = math.sqrt(9.0);   // 3.0
    n: int = math.abs(-7);        // 7
    println(x);
}
```

Module functions are called with the `module.function` prefix.

### Selective Import

```quant
from math import sqrt, abs

fn main() -> void {
    x: float = sqrt(9.0);   // call without prefix
    n: int = abs(-7);
    println(x);
}
```

`from M import f1, f2` brings specific names into scope without the prefix.

```quant
from math import *   // import every function from math
```

## Standard Library Modules

| Module | Contents |
|--------|----------|
| `math` | `sqrt`, `pow`, `sin`, `cos`, `abs`, `floor`, `ceil`, … |
| `string` | `len`, `concat`, `to_upper`, `contains`, `substring`, … |
| `array` | `len`, `push`, `pop`, `reverse`, … |
| `sys` | `exit`, `time`, `argc`, `platform`, `getcwd`, … |
| `io` | `print`, `println`, `read` |

See the [Standard Library reference](/reference/stdlib/) for the full list.

## Comments

```quant
// Single-line comment
# Also a single-line comment (Python-style)
```

## Statements and Semicolons

All statements end with a semicolon (`;`):

```quant
fn main() -> void {
    x: int = 10;              // variable declaration
    println("%d", x);         // function call
}
```

## Next Steps

- [Language Basics](/guides/language-basics/) , Variables, types, and expressions
- [Control Flow](/guides/control-flow/) , Conditionals and loops
- [Standard Library](/reference/stdlib/) , Complete function reference

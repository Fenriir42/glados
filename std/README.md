# Quant Standard Library

The Quant standard library provides modules for I/O, math, strings, arrays, system calls, and variadic utilities.

## Structure

```
std/
├── io.qa       # Console I/O
├── math.qa     # Mathematical functions
├── string.qa   # String manipulation
├── array.qa    # Array utilities
├── sys.qa      # System operations
├── varargs.qa  # Variadic helpers (sum, join, format, …)
└── README.md
```

## Importing

```quant
import math                         # all functions as math.sqrt, math.abs, …
from string import concat, to_upper # specific names, unqualified
from string import *                # wildcard — all names unqualified
```

---

## `io` — Input / Output

| Function | Signature | Description |
|----------|-----------|-------------|
| `print` | `(s: str) -> void` | Print without newline |
| `println` | `(s: str) -> void` | Print with trailing newline |
| `read` | `() -> str` | Read one line from stdin |

`print` and `println` are also available globally without any import.

```quant
from io import read

fn main() -> void {
    println("What is your name?");
    name: str = read();
    println(`Hello, {name}!`);
}
```

---

## `math` — Mathematics

| Function | Signature | Description |
|----------|-----------|-------------|
| `sqrt` | `(x: float) -> float` | Square root |
| `abs` | `(x: int) -> int` | Absolute value (integer) |
| `fabs` | `(x: float) -> float` | Absolute value (float) |
| `floor` | `(x: float) -> float` | Round towards −∞ |
| `ceil` | `(x: float) -> float` | Round towards +∞ |
| `pow` | `(base: float, exp: float) -> float` | Exponentiation |
| `log` | `(x: float) -> float` | Natural logarithm |
| `sin` | `(x: float) -> float` | Sine (radians) |
| `cos` | `(x: float) -> float` | Cosine (radians) |
| `tan` | `(x: float) -> float` | Tangent (radians) |
| `min` | `(a: int, b: int) -> int` | Minimum of two ints |
| `max` | `(a: int, b: int) -> int` | Maximum of two ints |
| `pi` | `() -> float` | π ≈ 3.14159… |
| `e` | `() -> float` | Euler's number ≈ 2.71828… |

```quant
from math import sqrt, pow

fn main() -> void {
    hyp: float = sqrt(pow(3.0, 2.0) + pow(4.0, 2.0));
    println(`hypotenuse: {hyp}`);
}
```

---

## `string` — String Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `len` | `(s: str) -> int` | Character count |
| `concat` | `(a: str, b: str) -> str` | Concatenate two strings |
| `substring` | `(s: str, start: int, end: int) -> str` | Extract substring |
| `char_at` | `(s: str, i: int) -> str` | Single character at index |
| `split` | `(s: str, delim: str) -> [str]` | Split on delimiter |
| `join` | `(arr: [str], delim: str) -> str` | Join array with delimiter |
| `trim` | `(s: str) -> str` | Strip leading/trailing whitespace |
| `trim_left` | `(s: str) -> str` | Strip leading whitespace |
| `trim_right` | `(s: str) -> str` | Strip trailing whitespace |
| `to_upper` | `(s: str) -> str` | Uppercase |
| `to_lower` | `(s: str) -> str` | Lowercase |
| `contains` | `(s: str, sub: str) -> bool` | Substring check |
| `starts_with` | `(s: str, prefix: str) -> bool` | Prefix check |
| `ends_with` | `(s: str, suffix: str) -> bool` | Suffix check |
| `index_of` | `(s: str, sub: str) -> int` | First occurrence index (−1 if absent) |
| `replace` | `(s: str, old: str, new: str) -> str` | Replace all occurrences |
| `replace_first` | `(s: str, old: str, new: str) -> str` | Replace first occurrence |
| `repeat` | `(s: str, n: int) -> str` | Repeat string n times |
| `reverse` | `(s: str) -> str` | Reverse characters |
| `to_str` | `(x: any) -> str` | Convert any value to string |

```quant
from string import to_upper, split, join

fn main() -> void {
    words: [str] = split("hello world", " ");
    println(to_upper(join(words, "-")));   // HELLO-WORLD
}
```

---

## `array` — Array Utilities

| Function | Signature | Description |
|----------|-----------|-------------|
| `len` | `(arr: [int]) -> int` | Array length |
| `push` | `(arr: [int], v: int) -> void` | Append element |
| `pop` | `(arr: [int]) -> int` | Remove and return last element |
| `sum` | `(arr: [int]) -> int` | Sum of all elements |
| `min` | `(arr: [int]) -> int` | Minimum element |
| `max` | `(arr: [int]) -> int` | Maximum element |
| `reverse` | `(arr: [int]) -> [int]` | Reversed copy |
| `contains` | `(arr: [int], v: int) -> bool` | Membership test |
| `index_of` | `(arr: [int], v: int) -> int` | First index of value (−1 if absent) |
| `slice` | `(arr: [int], start: int, end: int) -> [int]` | Sub-array |
| `sort` | `(arr: [int]) -> [int]` | Sorted copy (ascending) |
| `join_str` | `(arr: [str], sep: str) -> str` | Join string array |

```quant
from array import sort, reverse

fn main() -> void {
    nums: [int] = [5, 2, 8, 1, 9];
    s: [int] = sort(nums);
    r: [int] = reverse(s);
    println(`sorted: {s[0]}, {s[1]}, {s[2]}`);
}
```

---

## `sys` — System Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `exit` | `(code: int) -> void` | Exit with status code |
| `args` | `() -> [str]` | Command-line arguments |
| `time` | `() -> int` | Unix timestamp (seconds) |
| `hostname` | `() -> str` | Machine hostname |
| `cpu_time` | `() -> float` | CPU time used (seconds) |
| `sleep` | `(ms: int) -> void` | Sleep for milliseconds |

```quant
from sys import time, exit

fn main() -> void {
    t: int = time();
    println(`Started at {t}`);
    exit(0);
}
```

---

## `varargs` — Variadic Utilities

Functions that accept a variable number of arguments via the `...T` syntax.

| Function | Signature | Description |
|----------|-----------|-------------|
| `sum` | `(args: ...int) -> int` | Sum of all arguments |
| `product` | `(args: ...int) -> int` | Product of all arguments |
| `min` | `(args: ...int) -> int` | Minimum of all arguments |
| `max` | `(args: ...int) -> int` | Maximum of all arguments |
| `join` | `(sep: str, parts: ...str) -> str` | Join strings with separator |
| `format` | `(template: str, args: ...str) -> str` | Replace `{}` holes in order |

```quant
from varargs import sum, join, format

fn main() -> void {
    println(`{sum(1, 2, 3, 4, 5)}`);               // 15
    println(join(", ", "one", "two", "three"));     // one, two, three
    println(format("{} + {} = {}", "1", "2", "3")); // 1 + 2 = 3
}
```

---

## Implementation Status

| Module | Status |
|--------|--------|
| `io` | Complete |
| `math` | Complete |
| `string` | Complete |
| `array` | Complete |
| `sys` | Complete |
| `varargs` | Complete |

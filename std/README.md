# Quant Standard Library

The Quant standard library provides modules for I/O, math, strings, arrays, system calls, and variadic utilities.

## Structure

```
std/
├── io.qa        # Console I/O
├── math.qa      # Mathematical functions
├── string.qa    # String manipulation
├── array.qa     # Array utilities
├── sys.qa       # System operations + raw fd I/O + open flags
├── file.qa      # File I/O (read, write, append, lines, …)
├── buf.qa       # Mutable string buffers (write, flush, to_str, …)
├── varargs.qa   # Variadic helpers (sum, join, format, …)
├── dict.qa      # Dict helpers
├── json.qa      # JSON parse/encode
├── socket.qa    # TCP sockets
├── regex.qa     # Regular expressions
├── path.qa      # Filesystem path manipulation (join, basename, ext, normalize, …)
├── datetime.qa  # UTC dates on Unix timestamps (ISO-8601, make, add, diff, …)
├── csv.qa       # RFC 4180 CSV parse/encode
├── yaml.qa      # Flat "key: value" YAML maps
├── crypto.qa    # SHA-256, MD5, HMAC-SHA-256, CRC-32 (pure Quant)
├── os.qa        # Subprocess exec/capture + environment
├── sqlite.qa    # SQLite binding via FFI (open/exec/query)
├── test.qa      # Property-based testing (seeded generator + shrinking)
└── README.md
```

## Importing

```quant
import math                         # all functions as math.sqrt, math.abs, …
from string import concat, to_upper # specific names, unqualified
from string import *                # wildcard -all names unqualified
```

---

## `io` -Input / Output

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

## `math` -Mathematics

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

## `string` -String Operations

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

## `array` -Array Utilities

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

## `sys` -System Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `exit` | `(code: int) -> void` | Exit with status code |
| `args` | `() -> [str]` | Command-line arguments |
| `time` | `() -> int` | Unix timestamp (seconds) |
| `hostname` | `() -> str` | Machine hostname |
| `sleep` | `(ms: int) -> void` | Sleep for milliseconds |
| `write` | `(fd: int, s: str) -> int` | Write string to fd; returns bytes written |
| `read` | `(fd: int, n: int) -> str` | Read up to n bytes from fd |
| `open` | `(path: str, flags: int) -> int` | Open fd (returns -1 on error) |
| `close` | `(fd: int) -> bool` | Close fd |
| `flush` | `(fd: int) -> void` | Flush fd |
| `isatty` | `(fd: int) -> bool` | True if fd is a terminal |
| `stdin_fd` | `() -> int` | Returns 0 |
| `stdout_fd` | `() -> int` | Returns 1 |
| `stderr_fd` | `() -> int` | Returns 2 |
| `o_rdonly` | `() -> int` | Open flag: read-only |
| `o_wronly` | `() -> int` | Open flag: write-only |
| `o_rdwr` | `() -> int` | Open flag: read+write |
| `o_creat` | `() -> int` | Open flag: create if absent (mode 0644) |
| `o_trunc` | `() -> int` | Open flag: truncate on open |
| `o_append` | `() -> int` | Open flag: writes go to end |

```quant
from sys import time, exit

fn main() -> void {
    t: int = time();
    println(`Started at {t}`);
    exit(0);
}
```

---

## `buf` -Mutable String Buffers

Accumulate string output efficiently and flush to any file descriptor.

| Function | Signature | Description |
|----------|-----------|-------------|
| `new` | `() -> [str]` | Create an empty buffer |
| `write` | `(b: [str], s: str) -> void` | Append string to buffer |
| `writeln` | `(b: [str], s: str) -> void` | Append string + newline |
| `to_str` | `(b: [str]) -> str` | Concatenate buffer without clearing |
| `len` | `(b: [str]) -> int` | Total character count |
| `clear` | `(b: [str]) -> void` | Clear buffer |
| `flush` | `(b: [str], fd: int) -> int` | Write to fd, clear, return bytes |

```quant
from buf import new, write, writeln, flush
from sys import stderr_fd

fn main() -> void {
    b: [str] = new();
    writeln(b, "error: something went wrong");
    writeln(b, "hint: check your input");
    flush(b, stderr_fd());
}
```

---

## `file` -File I/O

| Function | Signature | Description |
|----------|-----------|-------------|
| `read` | `(path: str) -> str` | Read entire file; `""` on error |
| `write` | `(path: str, content: str) -> bool` | Write/overwrite file |
| `append` | `(path: str, content: str) -> bool` | Append to file |
| `exists` | `(path: str) -> bool` | True if file exists |
| `delete` | `(path: str) -> bool` | Delete file |
| `rename` | `(old: str, new: str) -> bool` | Rename or move file |
| `size` | `(path: str) -> int` | Size in bytes; `-1` on error |
| `lines` | `(path: str) -> [str]` | Read file as array of lines |

```quant
from file import write, read, lines, delete

fn main() -> void {
    write("hello.txt", "line one\nline two\n");
    rows: [str] = lines("hello.txt");
    i: int = 0;
    while (i < len(rows)) {
        println(rows[i]);
        i = i + 1;
    };
    delete("hello.txt");
}
```

---

## `varargs` -Variadic Utilities

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
| `file` | Complete |
| `buf` | Complete |
| `varargs` | Complete |

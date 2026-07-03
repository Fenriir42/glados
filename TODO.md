# Stdlib TODO: Pure Quant Modules

All entries below can be implemented entirely in Quant using existing language
features and builtins. No new Haskell code or VM instructions required.

---

## High priority

### `std/test.qa`: lightweight test framework

For the API testing use case. Tracks pass/fail counts and exits non-zero on
first failure (or after a full run, configurable).

```quant
from test import assert_eq, assert_true, assert_false, fail, summary

fn main() -> void {
    assert_eq(1 + 1, 2, "addition");
    assert_true(string.contains("hello", "ell"), "substring");
    summary();   // prints "3 passed, 0 failed"
}
```

Functions: `assert_eq`, `assert_neq`, `assert_true`, `assert_false`,
`assert_str_eq`, `fail`, `summary`, `reset`.

State tracked in a module-level `dict(str, int)` keyed `"pass"` / `"fail"`.

---

### `std/set.qa`: unordered set backed by dict(str, bool)

```quant
from set import new, add, remove, has, size, to_array, union, intersection, difference

fn main() -> void {
    s: dict(str, bool) = new();
    add(s, "a");
    add(s, "b");
    println(has(s, "a"));    // True
    println(size(s));         // 2
}
```

Functions: `new`, `add`, `remove`, `has`, `size`, `to_array`,
`union`, `intersection`, `difference`, `is_subset`, `is_empty`.

---

### `std/iter.qa`: higher-order array utilities

Unblocked now that first-class functions land. Works on `[int]` via type
erasure; call with any element type.

```quant
from iter import map_arr, filter_arr, reduce_arr, for_each, any, all, find

fn double(x: int) -> int { return x * 2; }
fn is_even(x: int) -> bool { return x % 2 == 0; }

fn main() -> void {
    nums: [int] = [1, 2, 3, 4, 5];
    doubled: [int] = map_arr(nums, double);
    evens: [int] = filter_arr(nums, is_even);
    total: int = reduce_arr(nums, fn(acc: int, x: int) -> int { return acc + x; }, 0);
}
```

Functions: `map_arr`, `filter_arr`, `reduce_arr`, `for_each`,
`any`, `all`, `find`, `find_index`, `flat_map`, `zip`.

---

### `std/option.qa`: helpers for option(T)

```quant
from option import is_some, is_none, unwrap, unwrap_or, map_opt

fn safe_div(a: int, b: int) -> option(int) {
    if (b == 0) { return None; };
    return Some(a / b);
}

fn main() -> void {
    r: option(int) = safe_div(10, 2);
    println(is_some(r));            // True
    println(unwrap_or(r, -1));      // 5
}
```

Functions: `is_some`, `is_none`, `unwrap`, `unwrap_or`,
`map_opt`, `or_else`, `filter_opt`, `to_array`.

---

## Medium priority

### `std/csv.qa`: CSV parsing and encoding

Pure string operations over `string.split` / `string.join`.

```quant
from csv import parse_line, encode_line, parse, encode

fn main() -> void {
    row: [str] = parse_line("alice,30,admin");
    println(row[0]);   // alice
    println(encode_line(row));   // alice,30,admin
}
```

Functions: `parse_line`, `encode_line`, `parse` (multi-line → `[[str]]`),
`encode` (`[[str]]` → str), `parse_header` (returns `dict(str, str)` keyed by column name).

Handles quoted fields with commas. Custom delimiter configurable.

---

### `std/url.qa`: URL percent-encoding

Needed for building query strings in HTTP requests.

```quant
from url import encode, decode, encode_query, build_query

fn main() -> void {
    println(encode("hello world"));       // hello%20world
    println(decode("hello%20world"));     // hello world
    params: dict(str, str) = {"q": "hello world", "page": "1"};
    println(build_query(params));         // q=hello%20world&page=1
}
```

Functions: `encode`, `decode`, `encode_query`, `decode_query`,
`build_query` (dict → query string), `parse_query` (query string → dict).

Implemented with `string.char_at`, `string.substring`, and hex lookup tables.

---

### `std/queue.qa`: FIFO queue backed by array

```quant
from queue import new, enqueue, dequeue, front, is_empty, size

fn main() -> void {
    q: [int] = new();
    enqueue(q, 1);
    enqueue(q, 2);
    println(front(q));     // 1
    dequeue(q);
    println(front(q));     // 2
}
```

Uses a head-index trick to avoid O(n) dequeue: stores `[head_idx, ...items]`
as a flat int array with item data offset by 1.

Functions: `new`, `enqueue`, `dequeue`, `front`, `is_empty`, `size`, `clear`.

---

### `std/deque.qa`: double-ended queue

Backed by two arrays (front-stack and back-stack). Amortized O(1) operations
at both ends.

Functions: `new`, `push_front`, `push_back`, `pop_front`, `pop_back`,
`peek_front`, `peek_back`, `is_empty`, `size`.

---

## Nice to have

### `std/base64.qa`: Base64 encoding/decoding

Useful for `Authorization: Basic ...` headers in HTTP requests. Pure
character-level implementation using `string.char_at` and a lookup table.

```quant
from base64 import encode, decode

fn main() -> void {
    println(encode("user:pass"));   // dXNlcjpwYXNz
    println(decode("dXNlcjpwYXNz"));   // user:pass
}
```

---

### `std/math` extras

Functions not yet in `math.qa`:

| Function | Description |
|----------|-------------|
| `is_prime(n)` | Primality test (trial division) |
| `fibonacci(n)` | n-th Fibonacci number |
| `combination(n, k)` | Binomial coefficient C(n, k) |
| `permutation(n, k)` | P(n, k) = n! / (n-k)! |
| `digits(n)` | `[int]` of decimal digits |
| `from_digits(arr)` | Reconstruct int from digit array |

---

### `std/string` extras

Functions not yet in `string.qa`:

| Function | Description |
|----------|-------------|
| `to_chars(s)` | Split string into `[str]` of single characters |
| `from_chars(arr)` | Join `[str]` of chars back into a string |
| `count_occurrences(s, sub)` | Count non-overlapping occurrences of `sub` in `s` |
| `lines(s)` | Split on `\n` → `[str]` |
| `wrap(s, width)` | Word-wrap to given column width |
| `lpad(s, n, ch)` | Left-pad to length `n` with character `ch` (already have `pad_left`) |
| `indent(s, n)` | Prepend `n` spaces to each line |

---

### `std/array` extras

| Function | Description |
|----------|-------------|
| `zip(a, b)` | Interleave two arrays into one |
| `flatten(arr_of_arr)` | Concatenate nested arrays |
| `unique(arr)` | Remove duplicates (uses set internally) |
| `chunk(arr, n)` | Split into sub-arrays of size `n` |
| `rotate(arr, n)` | Rotate left by `n` positions |
| `count_if(arr, f)` | Count elements satisfying predicate |
| `partition(arr, f)` | Split into `[matches, non_matches]` |

---

## Dependency order

```
option         standalone
test           needs println, sys.exit
set            needs dict.*
iter           needs array.*, first-class functions (done)
csv            needs string.split, string.join
url            needs string.char_at, string.substring
queue/deque    needs array.*
base64         needs string.char_at, string.repeat, array.*
math extras    standalone
string extras  needs string.*
array extras   needs array.*, set (for unique)
```

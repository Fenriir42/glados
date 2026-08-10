/* quant_runtime.h - C runtime for the Quant native backend (stage 1).
 *
 * Mirrors the value semantics of the bytecode VM (vm/src/VM/Interpreter.hs).
 * The VM remains the reference implementation: every observable behaviour
 * here (printing, equality, conversions) is written to match it so that
 * native binaries can be diffed against VM output.
 *
 * Memory: qt_alloc never frees (arena semantics). Compile with -DQUANT_GC
 * and link -lgc to route allocation through the Boehm collector instead.
 */

#ifndef QUANT_RUNTIME_H
#define QUANT_RUNTIME_H

#include <stddef.h>
#include <stdint.h>

/* ------------------------------------------------------------------ */
/* Values                                                              */

typedef enum {
    QT_UNIT = 0,
    QT_INT,
    QT_FLOAT,
    QT_BOOL,
    QT_STRING,
    QT_ARRAY,
    QT_DICT,
    QT_STRUCT,
    QT_ERROR,
    QT_FN,      /* named function reference (stage 7) */
    QT_CLOSURE, /* function + captured bindings (stage 7) */
    QT_PTR,     /* raw C pointer */
    QT_TASK     /* async task handle (stage 9) */
} QtTag;

typedef struct QtArray QtArray;
typedef struct QtDict QtDict;
typedef struct QtStruct QtStruct;
typedef struct QtError QtError;
typedef struct QtClosure QtClosure;

typedef struct {
    QtTag tag;
    union {
        int64_t i;        /* QT_INT, QT_BOOL (0/1), QT_TASK (id) */
        double f;         /* QT_FLOAT */
        const char *s;    /* QT_STRING: immutable, NUL-terminated */
        QtArray *arr;     /* QT_ARRAY */
        QtDict *dict;     /* QT_DICT */
        QtStruct *st;     /* QT_STRUCT */
        QtError *err;     /* QT_ERROR */
        const char *fn;   /* QT_FN: function name (symbolic, stage 7) */
        QtClosure *clo;   /* QT_CLOSURE */
        uint64_t ptr;     /* QT_PTR: raw address */
    } as;
} QtValue;

/* Arrays are dense, growable vectors. The VM models arrays as sparse maps;
 * setting past the end here fills the gap with unit values, which matches
 * every access pattern the compiler emits. */
struct QtArray {
    QtValue *items;
    size_t len;
    size_t cap;
};

/* Dicts are association vectors with linear lookup: correctness-first,
 * matching the VM's Map Value Value semantics for small dictionaries.
 * (VM key iteration is in Ord order; revisit when porting dict.keys.) */
struct QtDict {
    QtValue *keys;
    QtValue *vals;
    size_t len;
    size_t cap;
};

typedef struct {
    const char *name; /* interned by the code generator */
    QtValue value;
} QtField;

/* Structs carry their type name for dynamic method dispatch
 * (the vmStructTypes equivalent). */
struct QtStruct {
    const char *type_name;
    QtField *fields;
    size_t len;
    size_t cap;
};

/* Error values double as enum variants and option payloads, exactly like
 * the VM's VErrorVal. */
struct QtError {
    const char *name;
    QtField *fields;
    size_t len;
    size_t cap;
};

/* ------------------------------------------------------------------ */
/* Allocation                                                          */

void *qt_alloc(size_t size);
char *qt_strdup(const char *s);

/* ------------------------------------------------------------------ */
/* Constructors                                                        */

QtValue qt_unit(void);
QtValue qt_int(int64_t n);
QtValue qt_float(double f);
QtValue qt_bool(int b);
QtValue qt_string(const char *s); /* keeps the pointer; s must be immortal */
QtValue qt_ptr(uint64_t addr);

QtValue qt_array_new(void);
QtValue qt_dict_new(void);
QtValue qt_struct_new(const char *type_name);
QtValue qt_error_new(const char *name);

/* ------------------------------------------------------------------ */
/* Arrays                                                              */

int64_t qt_array_len(QtValue arr);
QtValue qt_array_get(QtValue arr, int64_t idx); /* panics out of bounds */
void qt_array_set(QtValue arr, int64_t idx, QtValue v); /* grows, unit-fills */
void qt_array_push(QtValue arr, QtValue v);
QtValue qt_array_pop(QtValue arr); /* unit on empty, like the VM */

/* ------------------------------------------------------------------ */
/* Dicts                                                               */

int64_t qt_dict_len(QtValue dict);
int qt_dict_has(QtValue dict, QtValue key);
QtValue qt_dict_get(QtValue dict, QtValue key); /* unit when missing */
void qt_dict_set(QtValue dict, QtValue key, QtValue v);
void qt_dict_delete(QtValue dict, QtValue key);

/* ------------------------------------------------------------------ */
/* Structs                                                             */

QtValue qt_struct_get(QtValue st, const char *field); /* panics if missing */
void qt_struct_set(QtValue st, const char *field, QtValue v);
const char *qt_struct_type(QtValue st);

/* ------------------------------------------------------------------ */
/* Errors / enum variants                                              */

void qt_error_set_field(QtValue err, const char *field, QtValue v);
QtValue qt_error_get_field(QtValue err, const char *field); /* panics if missing */
int qt_error_is(QtValue v, const char *name); /* IIsErr semantics */
int qt_is_error(QtValue v);                   /* IIsOk is the negation */

/* ------------------------------------------------------------------ */
/* Equality (VM Eq Value semantics: strings structural, refs by identity) */

int qt_value_eq(QtValue a, QtValue b);

/* ------------------------------------------------------------------ */
/* Rendering and printing                                              */

/* Shortest round-trip float format matching the VM's formatFloat:
 * integral doubles render as "<digits>.0" at any magnitude; non-integral
 * ones follow Haskell show (fixed in [0.1, 1e7), scientific outside).
 * A 400-byte buffer covers every double; smaller buffers truncate safely. */
void qt_format_float(char *buf, size_t bufsize, double f);

/* renderValue semantics (print/println): booleans are "True"/"False",
 * unit is empty, refs render as "<array#..>" style placeholders.
 * Returns a runtime-allocated string. */
const char *qt_render(QtValue v);

/* resolveStr semantics (string conversion / format holes): booleans are
 * "true"/"false", non-scalars render as "". */
const char *qt_to_string(QtValue v);

/* applyFormat semantics: printf-style holes. %d renders integrally
 * (floats round half-to-even), %s via qt_to_string, %f via formatFloat,
 * %% is a literal percent; unknown directives are copied verbatim and
 * keep their argument; once arguments run out the rest is verbatim. */
const char *qt_apply_format(const char *fmt, size_t nargs, const QtValue *args);

void qt_print(QtValue v);
void qt_println(QtValue v);
/* print/println with format arguments: print("x=% y=%", a, b) */
void qt_print_fmt(const char *fmt, size_t nargs, const QtValue *args);
void qt_println_fmt(const char *fmt, size_t nargs, const QtValue *args);

/* ------------------------------------------------------------------ */
/* Failure                                                             */

/* Prints "runtime error: <msg>" on stderr and exits 1, like the VM. */
void qt_panic(const char *msg) __attribute__((noreturn));

#endif /* QUANT_RUNTIME_H */

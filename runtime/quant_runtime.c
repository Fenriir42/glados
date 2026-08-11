/* quant_runtime.c - C runtime for the Quant native backend (stage 1). */

/* Feature-test macros must precede every system header: they enable the
 * POSIX I/O (open/read/write/isatty), setenv, gethostname, and usleep the
 * sys.* and file.* builtins rely on. */
#define _POSIX_C_SOURCE 200809L
#define _DEFAULT_SOURCE

#include "quant_runtime.h"

#include <arpa/inet.h>
#include <ctype.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <inttypes.h>
#include <math.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <time.h>
#include <ucontext.h>
#include <unistd.h>

#ifdef QUANT_GC
#include <gc.h>
#endif

/* ------------------------------------------------------------------ */
/* Allocation                                                          */

void *qt_alloc(size_t size) {
#ifdef QUANT_GC
    void *p = GC_MALLOC(size);
#else
    void *p = malloc(size);
#endif
    if (p == NULL) {
        qt_panic("out of memory");
    }
    return p;
}

char *qt_strdup(const char *s) {
    size_t n = strlen(s);
    char *p = qt_alloc(n + 1);
    memcpy(p, s, n + 1);
    return p;
}

/* ------------------------------------------------------------------ */
/* Constructors                                                        */

QtValue qt_unit(void) {
    QtValue v;
    v.tag = QT_UNIT;
    v.as.i = 0;
    return v;
}

QtValue qt_int(int64_t n) {
    QtValue v;
    v.tag = QT_INT;
    v.as.i = n;
    return v;
}

QtValue qt_float(double f) {
    QtValue v;
    v.tag = QT_FLOAT;
    v.as.f = f;
    return v;
}

QtValue qt_bool(int b) {
    QtValue v;
    v.tag = QT_BOOL;
    v.as.i = b ? 1 : 0;
    return v;
}

QtValue qt_string(const char *s) {
    QtValue v;
    v.tag = QT_STRING;
    v.as.s = s;
    return v;
}

QtValue qt_ptr(uint64_t addr) {
    QtValue v;
    v.tag = QT_PTR;
    v.as.ptr = addr;
    return v;
}

/* Closures carry the target function's name plus a copy of the captured
 * bindings (in IMakeClosure order).  The generated code reads them back at
 * function entry via qt_env. */
struct QtClosure {
    const char *name;
    QtValue *env;
    size_t env_len;
};

QtValue qt_fn(const char *name) {
    QtValue v;
    v.tag = QT_FN;
    v.as.fn = name;
    return v;
}

QtValue qt_task(int64_t id) {
    QtValue v;
    v.tag = QT_TASK;
    v.as.i = id;
    return v;
}

QtValue qt_closure(const char *name, size_t n, const QtValue *env) {
    QtClosure *c = qt_alloc(sizeof(QtClosure));
    c->name = name;
    c->env_len = n;
    c->env = NULL;
    if (n > 0) {
        c->env = qt_alloc(n * sizeof(QtValue));
        memcpy(c->env, env, n * sizeof(QtValue));
    }
    QtValue v;
    v.tag = QT_CLOSURE;
    v.as.clo = c;
    return v;
}

/* Environment of the closure currently being entered.  ICallIndirect sets
 * it via qt_callable_bind immediately before dispatch; the callee copies
 * what it needs into locals before making any further indirect call. */
static const QtValue *g_env = NULL;

QtValue qt_env(size_t i) {
    return g_env[i];
}

/* Resolve the symbolic name a callable dispatches to (QT_FN or QT_CLOSURE). */
const char *qt_callable_name(QtValue c) {
    if (c.tag == QT_FN) {
        return c.as.fn;
    }
    if (c.tag == QT_CLOSURE) {
        return c.as.clo->name;
    }
    qt_panic("call: value is not a function or closure");
}

/* Install a callable's captured environment for the upcoming dispatch. */
void qt_callable_bind(QtValue c) {
    g_env = (c.tag == QT_CLOSURE) ? c.as.clo->env : NULL;
}

QtValue qt_array_new(void) {
    QtArray *a = qt_alloc(sizeof(QtArray));
    a->items = NULL;
    a->len = 0;
    a->cap = 0;
    QtValue v;
    v.tag = QT_ARRAY;
    v.as.arr = a;
    return v;
}

QtValue qt_dict_new(void) {
    QtDict *d = qt_alloc(sizeof(QtDict));
    d->keys = NULL;
    d->vals = NULL;
    d->len = 0;
    d->cap = 0;
    QtValue v;
    v.tag = QT_DICT;
    v.as.dict = d;
    return v;
}

QtValue qt_struct_new(const char *type_name) {
    QtStruct *s = qt_alloc(sizeof(QtStruct));
    s->type_name = type_name;
    s->fields = NULL;
    s->len = 0;
    s->cap = 0;
    QtValue v;
    v.tag = QT_STRUCT;
    v.as.st = s;
    return v;
}

QtValue qt_error_new(const char *name) {
    QtError *e = qt_alloc(sizeof(QtError));
    e->name = name;
    e->fields = NULL;
    e->len = 0;
    e->cap = 0;
    QtValue v;
    v.tag = QT_ERROR;
    v.as.err = e;
    return v;
}

/* ------------------------------------------------------------------ */
/* Arrays                                                              */

static void array_reserve(QtArray *a, size_t need) {
    if (need <= a->cap) {
        return;
    }
    size_t cap = a->cap == 0 ? 8 : a->cap;
    while (cap < need) {
        cap *= 2;
    }
    QtValue *items = qt_alloc(cap * sizeof(QtValue));
    if (a->len > 0) {
        memcpy(items, a->items, a->len * sizeof(QtValue));
    }
    a->items = items;
    a->cap = cap;
}

static QtArray *as_array(QtValue v, const char *ctx) {
    if (v.tag != QT_ARRAY) {
        qt_panic(ctx);
    }
    return v.as.arr;
}

int64_t qt_array_len(QtValue arr) {
    return (int64_t)as_array(arr, "expected array in len")->len;
}

QtValue qt_array_get(QtValue arr, int64_t idx) {
    QtArray *a = as_array(arr, "expected array in index");
    if (idx < 0 || (size_t)idx >= a->len) {
        qt_panic("array index out of bounds");
    }
    return a->items[idx];
}

void qt_array_set(QtValue arr, int64_t idx, QtValue v) {
    QtArray *a = as_array(arr, "expected array in index assignment");
    if (idx < 0) {
        qt_panic("array index out of bounds");
    }
    if ((size_t)idx >= a->len) {
        array_reserve(a, (size_t)idx + 1);
        for (size_t i = a->len; i < (size_t)idx; i++) {
            a->items[i] = qt_unit();
        }
        a->len = (size_t)idx + 1;
    }
    a->items[idx] = v;
}

void qt_array_push(QtValue arr, QtValue v) {
    QtArray *a = as_array(arr, "expected array in push");
    array_reserve(a, a->len + 1);
    a->items[a->len] = v;
    a->len += 1;
}

QtValue qt_array_pop(QtValue arr) {
    QtArray *a = as_array(arr, "expected array in pop");
    if (a->len == 0) {
        return qt_unit();
    }
    a->len -= 1;
    return a->items[a->len];
}

/* ------------------------------------------------------------------ */
/* Dicts                                                               */

static QtDict *as_dict(QtValue v, const char *ctx) {
    if (v.tag != QT_DICT) {
        qt_panic(ctx);
    }
    return v.as.dict;
}

static long dict_find(const QtDict *d, QtValue key) {
    for (size_t i = 0; i < d->len; i++) {
        if (qt_value_eq(d->keys[i], key)) {
            return (long)i;
        }
    }
    return -1;
}

int64_t qt_dict_len(QtValue dict) {
    return (int64_t)as_dict(dict, "expected dict in len")->len;
}

int qt_dict_has(QtValue dict, QtValue key) {
    return dict_find(as_dict(dict, "expected dict in has"), key) >= 0;
}

QtValue qt_dict_get(QtValue dict, QtValue key) {
    QtDict *d = as_dict(dict, "expected dict in lookup");
    long i = dict_find(d, key);
    if (i < 0) {
        return qt_unit();
    }
    return d->vals[i];
}

void qt_dict_set(QtValue dict, QtValue key, QtValue v) {
    QtDict *d = as_dict(dict, "expected dict in assignment");
    long i = dict_find(d, key);
    if (i >= 0) {
        d->vals[i] = v;
        return;
    }
    if (d->len == d->cap) {
        size_t cap = d->cap == 0 ? 8 : d->cap * 2;
        QtValue *keys = qt_alloc(cap * sizeof(QtValue));
        QtValue *vals = qt_alloc(cap * sizeof(QtValue));
        if (d->len > 0) {
            memcpy(keys, d->keys, d->len * sizeof(QtValue));
            memcpy(vals, d->vals, d->len * sizeof(QtValue));
        }
        d->keys = keys;
        d->vals = vals;
        d->cap = cap;
    }
    d->keys[d->len] = key;
    d->vals[d->len] = v;
    d->len += 1;
}

void qt_dict_delete(QtValue dict, QtValue key) {
    QtDict *d = as_dict(dict, "expected dict in delete");
    long i = dict_find(d, key);
    if (i < 0) {
        return;
    }
    d->keys[i] = d->keys[d->len - 1];
    d->vals[i] = d->vals[d->len - 1];
    d->len -= 1;
}

/* ------------------------------------------------------------------ */
/* Field vectors (shared by structs and errors)                        */

static long fields_find(const QtField *fields, size_t len, const char *name) {
    for (size_t i = 0; i < len; i++) {
        if (strcmp(fields[i].name, name) == 0) {
            return (long)i;
        }
    }
    return -1;
}

static void fields_set(QtField **fields, size_t *len, size_t *cap,
                       const char *name, QtValue v) {
    long i = fields_find(*fields, *len, name);
    if (i >= 0) {
        (*fields)[i].value = v;
        return;
    }
    if (*len == *cap) {
        size_t ncap = *cap == 0 ? 4 : *cap * 2;
        QtField *nf = qt_alloc(ncap * sizeof(QtField));
        if (*len > 0) {
            memcpy(nf, *fields, *len * sizeof(QtField));
        }
        *fields = nf;
        *cap = ncap;
    }
    (*fields)[*len].name = name;
    (*fields)[*len].value = v;
    *len += 1;
}

/* ------------------------------------------------------------------ */
/* Structs                                                             */

static QtStruct *as_struct(QtValue v, const char *ctx) {
    if (v.tag != QT_STRUCT) {
        qt_panic(ctx);
    }
    return v.as.st;
}

QtValue qt_struct_get(QtValue st, const char *field) {
    QtStruct *s = as_struct(st, "expected struct in field access");
    long i = fields_find(s->fields, s->len, field);
    if (i < 0) {
        qt_panic("struct field not found");
    }
    return s->fields[i].value;
}

void qt_struct_set(QtValue st, const char *field, QtValue v) {
    QtStruct *s = as_struct(st, "expected struct in field assignment");
    fields_set(&s->fields, &s->len, &s->cap, field, v);
}

const char *qt_struct_type(QtValue st) {
    return as_struct(st, "expected struct in type lookup")->type_name;
}

/* ------------------------------------------------------------------ */
/* Errors / enum variants                                              */

void qt_error_set_field(QtValue err, const char *field, QtValue v) {
    if (err.tag != QT_ERROR) {
        qt_panic("expected error value in field assignment");
    }
    QtError *e = err.as.err;
    fields_set(&e->fields, &e->len, &e->cap, field, v);
}

QtValue qt_error_get_field(QtValue err, const char *field) {
    if (err.tag != QT_ERROR) {
        qt_panic("expected error value in field access");
    }
    QtError *e = err.as.err;
    long i = fields_find(e->fields, e->len, field);
    if (i < 0) {
        qt_panic("error field not found");
    }
    return e->fields[i].value;
}

int qt_error_is(QtValue v, const char *name) {
    return v.tag == QT_ERROR && strcmp(v.as.err->name, name) == 0;
}

int qt_is_error(QtValue v) {
    return v.tag == QT_ERROR;
}

/* ------------------------------------------------------------------ */
/* Equality                                                            */

int qt_value_eq(QtValue a, QtValue b) {
    if (a.tag != b.tag) {
        return 0;
    }
    switch (a.tag) {
        case QT_UNIT:
            return 1;
        case QT_INT:
        case QT_BOOL:
        case QT_TASK:
            return a.as.i == b.as.i;
        case QT_FLOAT:
            return a.as.f == b.as.f;
        case QT_STRING:
            return strcmp(a.as.s, b.as.s) == 0;
        case QT_ARRAY:
            return a.as.arr == b.as.arr;
        case QT_DICT:
            return a.as.dict == b.as.dict;
        case QT_STRUCT:
            return a.as.st == b.as.st;
        case QT_ERROR:
            /* VM Eq compares name and payload; name-only covers every
             * pattern the compiler emits (IIsErr). */
            return strcmp(a.as.err->name, b.as.err->name) == 0;
        case QT_FN:
            return strcmp(a.as.fn, b.as.fn) == 0;
        case QT_CLOSURE:
            return a.as.clo == b.as.clo;
        case QT_PTR:
            return a.as.ptr == b.as.ptr;
    }
    return 0;
}

/* ------------------------------------------------------------------ */
/* Rendering                                                           */

/* Shared float rendering.  With integral_fixed set this is the VM's
 * formatFloat (integral floats print as "<digits>.0" at any magnitude);
 * without it this is raw Haskell show (fixed notation only in
 * [0.1, 1e7), scientific outside, used by string casts). */
static void format_float_core(char *buf, size_t bufsize, double f,
                              int integral_fixed) {
    if (f != f) {
        snprintf(buf, bufsize, "NaN");
        return;
    }
    if (f > 1.7976931348623158e308) {
        snprintf(buf, bufsize, "Infinity");
        return;
    }
    if (f < -1.7976931348623158e308) {
        snprintf(buf, bufsize, "-Infinity");
        return;
    }
    const char *sign = "";
    double a = f;
    if (a < 0.0 || (a == 0.0 && 1.0 / a < 0.0)) {
        sign = "-";
        a = -a;
    }
    if (a == 0.0) {
        snprintf(buf, bufsize, "%s0.0", sign);
        return;
    }
    /* Shortest round-trip digit string, via scientific printf. */
    char tmp[48];
    for (int prec = 0; prec <= 16; prec++) {
        snprintf(tmp, sizeof(tmp), "%.*e", prec, a);
        if (strtod(tmp, NULL) == a) {
            break;
        }
    }
    char digits[32];
    int nd = 0;
    char *e = strchr(tmp, 'e');
    int exp10 = (int)strtol(e + 1, NULL, 10);
    for (const char *p = tmp; p < e && nd < 31; p++) {
        if (*p >= '0' && *p <= '9') {
            digits[nd++] = *p;
        }
    }
    while (nd > 1 && digits[nd - 1] == '0') {
        nd--;
    }
    digits[nd] = '\0';
    int intdigits = exp10 + 1;
    int integral = integral_fixed && intdigits >= nd;
    if (integral || (exp10 >= -1 && exp10 <= 6)) {
        size_t o = 0;
        if (*sign != '\0' && o + 1 < bufsize) {
            buf[o++] = '-';
        }
        if (intdigits <= 0) {
            if (o + 1 < bufsize) buf[o++] = '0';
            if (o + 1 < bufsize) buf[o++] = '.';
            for (int i = 0; i < -intdigits; i++) {
                if (o + 1 < bufsize) buf[o++] = '0';
            }
            for (int i = 0; i < nd; i++) {
                if (o + 1 < bufsize) buf[o++] = digits[i];
            }
        } else if (nd <= intdigits) {
            for (int i = 0; i < nd; i++) {
                if (o + 1 < bufsize) buf[o++] = digits[i];
            }
            for (int i = nd; i < intdigits; i++) {
                if (o + 1 < bufsize) buf[o++] = '0';
            }
            if (o + 1 < bufsize) buf[o++] = '.';
            if (o + 1 < bufsize) buf[o++] = '0';
        } else {
            for (int i = 0; i < intdigits; i++) {
                if (o + 1 < bufsize) buf[o++] = digits[i];
            }
            if (o + 1 < bufsize) buf[o++] = '.';
            for (int i = intdigits; i < nd; i++) {
                if (o + 1 < bufsize) buf[o++] = digits[i];
            }
        }
        buf[o < bufsize ? o : bufsize - 1] = '\0';
    } else {
        if (nd == 1) {
            snprintf(buf, bufsize, "%s%c.0e%d", sign, digits[0], exp10);
        } else {
            snprintf(buf, bufsize, "%s%c.%se%d", sign, digits[0], digits + 1,
                     exp10);
        }
    }
}

void qt_format_float(char *buf, size_t bufsize, double f) {
    format_float_core(buf, bufsize, f, 1);
}

void qt_format_float_show(char *buf, size_t bufsize, double f) {
    format_float_core(buf, bufsize, f, 0);
}

static const char *render_scalar(QtValue v, int for_print) {
    /* Large enough for integral doubles near 1e308 rendered as digits. */
    char buf[400];
    switch (v.tag) {
        case QT_STRING:
            return v.as.s;
        case QT_INT:
            snprintf(buf, sizeof(buf), "%" PRId64, v.as.i);
            return qt_strdup(buf);
        case QT_FLOAT:
            qt_format_float(buf, sizeof(buf), v.as.f);
            return qt_strdup(buf);
        case QT_BOOL:
            if (for_print) {
                return v.as.i ? "True" : "False";
            }
            return v.as.i ? "true" : "false";
        default:
            return NULL;
    }
}

const char *qt_render(QtValue v) {
    const char *scalar = render_scalar(v, 1);
    if (scalar != NULL) {
        return scalar;
    }
    char buf[128];
    switch (v.tag) {
        case QT_UNIT:
            return "";
        case QT_ARRAY:
            snprintf(buf, sizeof(buf), "<array#%p>", (void *)v.as.arr);
            break;
        case QT_DICT:
            snprintf(buf, sizeof(buf), "<dict#%p>", (void *)v.as.dict);
            break;
        case QT_STRUCT:
            snprintf(buf, sizeof(buf), "<struct#%p>", (void *)v.as.st);
            break;
        case QT_ERROR:
            snprintf(buf, sizeof(buf), "error %s", v.as.err->name);
            break;
        case QT_FN:
            snprintf(buf, sizeof(buf), "<fn:%s>", v.as.fn);
            break;
        case QT_CLOSURE:
            snprintf(buf, sizeof(buf), "<closure#%p>", (void *)v.as.clo);
            break;
        case QT_PTR:
            snprintf(buf, sizeof(buf), "0x%" PRIx64, v.as.ptr);
            break;
        case QT_TASK:
            snprintf(buf, sizeof(buf), "<task:%" PRId64 ">", v.as.i);
            break;
        default:
            buf[0] = '\0';
            break;
    }
    return qt_strdup(buf);
}

const char *qt_to_string(QtValue v) {
    const char *scalar = render_scalar(v, 0);
    return scalar != NULL ? scalar : "";
}

/* %d hole: ints as-is, floats rounded half-to-even like Haskell round,
 * bools as 1/0, strings as-is, anything else "?". */
static const char *render_as_int(QtValue v) {
    char buf[32];
    switch (v.tag) {
        case QT_INT:
            snprintf(buf, sizeof(buf), "%" PRId64, v.as.i);
            return qt_strdup(buf);
        case QT_FLOAT: {
            double r = v.as.f - (double)(int64_t)v.as.f;
            int64_t t = (int64_t)v.as.f;
            /* round half to even */
            if (r > 0.5 || (r == 0.5 && (t % 2 != 0))) {
                t += 1;
            } else if (r < -0.5 || (r == -0.5 && (t % 2 != 0))) {
                t -= 1;
            }
            snprintf(buf, sizeof(buf), "%" PRId64, t);
            return qt_strdup(buf);
        }
        case QT_BOOL:
            return v.as.i ? "1" : "0";
        case QT_STRING:
            return v.as.s;
        default:
            return "?";
    }
}

/* %f hole: floats via formatFloat, ints verbatim, anything else "?". */
static const char *render_as_flt(QtValue v) {
    char buf[400];
    switch (v.tag) {
        case QT_FLOAT:
            qt_format_float(buf, sizeof(buf), v.as.f);
            return qt_strdup(buf);
        case QT_INT:
            snprintf(buf, sizeof(buf), "%" PRId64, v.as.i);
            return qt_strdup(buf);
        default:
            return "?";
    }
}

typedef struct {
    char *data;
    size_t len;
    size_t cap;
} OutBuf;

static void out_append(OutBuf *out, const char *piece, size_t plen) {
    if (out->len + plen + 1 > out->cap) {
        size_t ncap = (out->cap + plen + 1) * 2;
        char *ndata = qt_alloc(ncap);
        memcpy(ndata, out->data, out->len);
        out->data = ndata;
        out->cap = ncap;
    }
    memcpy(out->data + out->len, piece, plen);
    out->len += plen;
    out->data[out->len] = '\0';
}

const char *qt_apply_format(const char *fmt, size_t nargs, const QtValue *args) {
    OutBuf out;
    out.cap = strlen(fmt) + 64;
    out.len = 0;
    out.data = qt_alloc(out.cap);
    out.data[0] = '\0';
    size_t ai = 0;
    const char *p = fmt;
    while (*p != '\0') {
        if (*p != '%' || ai >= nargs) {
            /* Once arguments run out the rest of the format is verbatim. */
            out_append(&out, p, 1);
            p++;
            continue;
        }
        char d = p[1];
        if (d == 'd') {
            const char *piece = render_as_int(args[ai++]);
            out_append(&out, piece, strlen(piece));
            p += 2;
        } else if (d == 's') {
            const char *piece = qt_to_string(args[ai++]);
            out_append(&out, piece, strlen(piece));
            p += 2;
        } else if (d == 'f') {
            const char *piece = render_as_flt(args[ai++]);
            out_append(&out, piece, strlen(piece));
            p += 2;
        } else if (d == '%') {
            /* literal percent; does not consume an argument */
            out_append(&out, "%", 1);
            p += 2;
        } else if (d == '\0') {
            out_append(&out, "%", 1);
            p += 1;
        } else {
            /* unknown directive: copied verbatim, argument kept */
            out_append(&out, p, 2);
            p += 2;
        }
    }
    return out.data;
}

void qt_print(QtValue v) {
    fputs(qt_render(v), stdout);
}

void qt_println(QtValue v) {
    fputs(qt_render(v), stdout);
    fputc('\n', stdout);
}

void qt_print_fmt(const char *fmt, size_t nargs, const QtValue *args) {
    if (nargs == 0) {
        fputs(fmt, stdout);
    } else {
        fputs(qt_apply_format(fmt, nargs, args), stdout);
    }
}

void qt_println_fmt(const char *fmt, size_t nargs, const QtValue *args) {
    qt_print_fmt(fmt, nargs, args);
    fputc('\n', stdout);
}

/* ------------------------------------------------------------------ */
/* Translator helpers                                                  */

int qt_truthy(QtValue v) {
    if (v.tag == QT_BOOL) {
        return v.as.i != 0;
    }
    if (v.tag == QT_INT) {
        return v.as.i != 0;
    }
    qt_panic("conditional jump: expected bool");
}

int64_t qt_want_int(QtValue v, const char *ctx) {
    if (v.tag != QT_INT) {
        qt_panic(ctx);
    }
    return v.as.i;
}

QtValue qt_array_get_or_new(QtValue arr, int64_t idx) {
    QtArray *a = as_array(arr, "expected array in index");
    if (idx >= 0 && (size_t)idx < a->len && a->items[idx].tag != QT_UNIT) {
        return a->items[idx];
    }
    QtValue fresh = qt_array_new();
    qt_array_set(arr, idx, fresh);
    return fresh;
}

/* IFieldGet works on both structs and error/enum values (the VM reads
 * fields from either). */
QtValue qt_field_get(QtValue ref, const char *field) {
    if (ref.tag == QT_ERROR) {
        return qt_error_get_field(ref, field);
    }
    return qt_struct_get(ref, field);
}

/* IArrayGet/IArraySet are polymorphic over arrays (int index) and dicts
 * (value key), matching the VM's single indexing instruction.  A missing
 * dict key is a runtime error, exactly as in the VM. */
QtValue qt_index_get(QtValue ref, QtValue idx) {
    if (ref.tag == QT_DICT) {
        if (!qt_dict_has(ref, idx)) {
            qt_panic("dict key not found");
        }
        return qt_dict_get(ref, idx);
    }
    return qt_array_get(ref, qt_want_int(idx, "array index"));
}

void qt_index_set(QtValue ref, QtValue idx, QtValue v) {
    if (ref.tag == QT_DICT) {
        qt_dict_set(ref, idx, v);
        return;
    }
    qt_array_set(ref, qt_want_int(idx, "array index"), v);
}

/* IMustOp: unwrapping an error value panics like the VM's `must`. */
void qt_must(QtValue v) {
    if (v.tag == QT_ERROR) {
        char msg[256];
        snprintf(msg, sizeof(msg), "must: unwrapped error `%s`", v.as.err->name);
        qt_panic(msg);
    }
}

/* ------------------------------------------------------------------ */
/* Operators                                                           */

/* valEq semantics for == and !=: scalars structural, errors by name,
 * references (arrays/dicts/structs/...) never equal. */
static int val_eq_op(QtValue a, QtValue b) {
    if (a.tag != b.tag) {
        return 0;
    }
    switch (a.tag) {
        case QT_INT:
        case QT_BOOL:
            return a.as.i == b.as.i;
        case QT_FLOAT:
            return a.as.f == b.as.f;
        case QT_STRING:
            return strcmp(a.as.s, b.as.s) == 0;
        case QT_ERROR:
            return strcmp(a.as.err->name, b.as.err->name) == 0;
        case QT_UNIT:
            return 1;
        default:
            return 0;
    }
}

static double num_as_double(QtValue v, const char *ctx) {
    if (v.tag == QT_INT) {
        return (double)v.as.i;
    }
    if (v.tag == QT_FLOAT) {
        return v.as.f;
    }
    qt_panic(ctx);
}

/* Haskell div/mod: floor division. */
static int64_t floor_div(int64_t a, int64_t b) {
    int64_t q = a / b;
    if ((a % b != 0) && ((a < 0) != (b < 0))) {
        q -= 1;
    }
    return q;
}

static int64_t floor_mod(int64_t a, int64_t b) {
    int64_t r = a % b;
    if (r != 0 && ((r < 0) != (b < 0))) {
        r += b;
    }
    return r;
}

static int both_int(QtValue a, QtValue b) {
    return a.tag == QT_INT && b.tag == QT_INT;
}

static int numeric_pair(QtValue a, QtValue b) {
    return (a.tag == QT_INT || a.tag == QT_FLOAT) &&
           (b.tag == QT_INT || b.tag == QT_FLOAT);
}

QtValue qt_binary(QtBinOp op, QtValue a, QtValue b) {
    switch (op) {
        case QT_BOP_ADD:
        case QT_BOP_SUB:
        case QT_BOP_MUL:
            if (both_int(a, b)) {
                int64_t x = a.as.i, y = b.as.i;
                return qt_int(op == QT_BOP_ADD   ? x + y
                              : op == QT_BOP_SUB ? x - y
                                                 : x * y);
            }
            if (numeric_pair(a, b)) {
                double x = num_as_double(a, ""), y = num_as_double(b, "");
                return qt_float(op == QT_BOP_ADD   ? x + y
                                : op == QT_BOP_SUB ? x - y
                                                   : x * y);
            }
            qt_panic("Binary arithmetic: bad types");
        case QT_BOP_DIV:
            if (both_int(a, b)) {
                if (b.as.i == 0) {
                    qt_panic("Division by zero");
                }
                return qt_int(floor_div(a.as.i, b.as.i));
            }
            if (numeric_pair(a, b)) {
                return qt_float(num_as_double(a, "") / num_as_double(b, ""));
            }
            qt_panic("Binary division: bad types");
        case QT_BOP_MOD:
            if (both_int(a, b)) {
                if (b.as.i == 0) {
                    qt_panic("Modulo by zero");
                }
                return qt_int(floor_mod(a.as.i, b.as.i));
            }
            qt_panic("Binary modulo: bad types");
        case QT_BOP_EQ:
            return qt_bool(val_eq_op(a, b));
        case QT_BOP_NEQ:
            return qt_bool(!val_eq_op(a, b));
        case QT_BOP_LT:
        case QT_BOP_LTE:
        case QT_BOP_GT:
        case QT_BOP_GTE: {
            /* cmpOp: both sides through double, exactly like the VM */
            double x = num_as_double(a, "Comparison: bad types");
            double y = num_as_double(b, "Comparison: bad types");
            int r = op == QT_BOP_LT    ? x < y
                    : op == QT_BOP_LTE ? x <= y
                    : op == QT_BOP_GT  ? x > y
                                       : x >= y;
            return qt_bool(r);
        }
        case QT_BOP_AND:
        case QT_BOP_OR:
            if (a.tag == QT_BOOL && b.tag == QT_BOOL) {
                return qt_bool(op == QT_BOP_AND ? (a.as.i && b.as.i)
                                                : (a.as.i || b.as.i));
            }
            qt_panic("Binary logic: bad types");
        case QT_BOP_BITAND:
        case QT_BOP_BITOR:
        case QT_BOP_BITXOR:
        case QT_BOP_SHL:
        case QT_BOP_SHR:
            if (both_int(a, b)) {
                int64_t x = a.as.i, y = b.as.i;
                switch (op) {
                    case QT_BOP_BITAND:
                        return qt_int(x & y);
                    case QT_BOP_BITOR:
                        return qt_int(x | y);
                    case QT_BOP_BITXOR:
                        return qt_int(x ^ y);
                    case QT_BOP_SHL:
                        return qt_int(x << y);
                    default:
                        return qt_int(x >> y);
                }
            }
            qt_panic("Binary bitwise: bad types");
        default:
            qt_panic("Binary: unknown operator");
    }
}

QtValue qt_unary(QtUnOp op, QtValue v) {
    switch (op) {
        case QT_UOP_NEG:
            if (v.tag == QT_INT) {
                return qt_int(-v.as.i);
            }
            if (v.tag == QT_FLOAT) {
                return qt_float(-v.as.f);
            }
            qt_panic("Unary negate: bad type");
        case QT_UOP_NOT:
            if (v.tag == QT_BOOL) {
                return qt_bool(!v.as.i);
            }
            qt_panic("Unary not: bad type");
        case QT_UOP_BITNOT:
            if (v.tag == QT_INT) {
                return qt_int(~v.as.i);
            }
            qt_panic("Unary bitnot: bad type");
        default:
            qt_panic("Unary: unknown operator");
    }
}

/* ------------------------------------------------------------------ */
/* Casts                                                               */

QtValue qt_cast_int(QtValue v) {
    switch (v.tag) {
        case QT_FLOAT:
            return qt_int((int64_t)v.as.f); /* truncate toward zero */
        case QT_INT:
            return v;
        case QT_BOOL:
            return qt_int(v.as.i);
        case QT_STRING: {
            char *end = NULL;
            int64_t n = strtoll(v.as.s, &end, 10);
            if (end == v.as.s || *end != '\0') {
                qt_panic("Cannot cast string to int");
            }
            return qt_int(n);
        }
        default:
            qt_panic("Cast to int: bad type");
    }
}

QtValue qt_cast_float(QtValue v) {
    switch (v.tag) {
        case QT_INT:
            return qt_float((double)v.as.i);
        case QT_FLOAT:
            return v;
        case QT_STRING: {
            char *end = NULL;
            double f = strtod(v.as.s, &end);
            if (end == v.as.s || *end != '\0') {
                qt_panic("Cannot cast string to float");
            }
            return qt_float(f);
        }
        default:
            qt_panic("Cast to float: bad type");
    }
}

QtValue qt_cast_bool(QtValue v) {
    switch (v.tag) {
        case QT_INT:
            return qt_bool(v.as.i != 0);
        case QT_BOOL:
            return v;
        default:
            qt_panic("Cast to bool: bad type");
    }
}

QtValue qt_cast_string(QtValue v) {
    char buf[400];
    switch (v.tag) {
        case QT_INT:
            snprintf(buf, sizeof(buf), "%" PRId64, v.as.i);
            return qt_string(qt_strdup(buf));
        case QT_FLOAT:
            /* evalCast uses raw Haskell show, not formatFloat */
            qt_format_float_show(buf, sizeof(buf), v.as.f);
            return qt_string(qt_strdup(buf));
        case QT_BOOL:
            return qt_string(v.as.i ? "true" : "false");
        case QT_STRING:
            return v;
        default:
            qt_panic("Cast to string: bad type");
    }
}

/* ------------------------------------------------------------------ */
/* Builtin dispatch                                                    */

static QtValue builtin_concat(size_t nargs, const QtValue *args) {
    size_t total = 1;
    for (size_t i = 0; i < nargs; i++) {
        total += strlen(qt_to_string(args[i]));
    }
    char *out = qt_alloc(total);
    out[0] = '\0';
    size_t len = 0;
    for (size_t i = 0; i < nargs; i++) {
        const char *piece = qt_to_string(args[i]);
        size_t plen = strlen(piece);
        memcpy(out + len, piece, plen);
        len += plen;
    }
    out[len] = '\0';
    return qt_string(out);
}

/* ------------------------------------------------------------------ */
/* string.* and math.* builtins (see ROADMAP: Native backend stage 4)  */

/* Bytes spanned by the UTF-8 codepoint starting at lead byte c. */
static size_t utf8_seq_len(unsigned char c) {
    if (c < 0x80) {
        return 1;
    }
    if ((c >> 5) == 0x6) {
        return 2;
    }
    if ((c >> 4) == 0xe) {
        return 3;
    }
    if ((c >> 3) == 0x1e) {
        return 4;
    }
    return 1; /* invalid lead byte: advance one to make progress */
}

/* Number of codepoints in a NUL-terminated UTF-8 string. */
static size_t utf8_count(const char *s) {
    size_t n = 0;
    while (*s) {
        s += utf8_seq_len((unsigned char)*s);
        n++;
    }
    return n;
}

/* Byte offset of codepoint index cp (clamped to the terminating NUL). */
static size_t utf8_offset(const char *s, int64_t cp) {
    size_t off = 0;
    while (cp > 0 && s[off]) {
        off += utf8_seq_len((unsigned char)s[off]);
        cp--;
    }
    return off;
}

/* Allocate an immortal copy of the byte range [start, end). */
static QtValue string_slice(const char *s, size_t start, size_t end) {
    size_t n = end - start;
    char *out = qt_alloc(n + 1);
    memcpy(out, s + start, n);
    out[n] = '\0';
    return qt_string(out);
}

/* Haskell `round`: round half to even. */
static int64_t haskell_round(double f) {
    double fl = floor(f);
    double diff = f - fl;
    int64_t lo = (int64_t)fl;
    if (diff < 0.5) {
        return lo;
    }
    if (diff > 0.5) {
        return lo + 1;
    }
    return (lo % 2 == 0) ? lo : lo + 1; /* exactly .5 -> nearest even */
}

/* Codepoint index of the first/last occurrence of needle, or -1. */
static int64_t string_index_of(const char *hay, const char *needle, int last) {
    if (needle[0] == '\0') {
        return 0;
    }
    int64_t found = -1;
    size_t cp = 0;
    for (const char *p = hay; *p; p += utf8_seq_len((unsigned char)*p), cp++) {
        if (strncmp(p, needle, strlen(needle)) == 0) {
            if (!last) {
                return (int64_t)cp;
            }
            found = (int64_t)cp;
        }
    }
    return found;
}

/* Replace occurrences of `from` with `to` (all, or just the first). */
static QtValue string_replace(const char *s, const char *from, const char *to,
                              int first_only) {
    size_t flen = strlen(from);
    if (flen == 0) {
        return qt_string(qt_strdup(s));
    }
    size_t tlen = strlen(to);
    /* Count matches to size the output buffer. */
    size_t matches = 0;
    for (const char *p = s; (p = strstr(p, from)) != NULL; p += flen) {
        matches++;
        if (first_only) {
            break;
        }
    }
    size_t slen = strlen(s);
    size_t outlen = slen + matches * (tlen >= flen ? tlen - flen : 0);
    char *out = qt_alloc(outlen + 1);
    char *w = out;
    const char *p = s;
    size_t done = 0;
    while (*p) {
        if ((!first_only || done == 0) && strncmp(p, from, flen) == 0) {
            memcpy(w, to, tlen);
            w += tlen;
            p += flen;
            done++;
        } else {
            *w++ = *p++;
        }
    }
    *w = '\0';
    return qt_string(out);
}

static QtValue builtin_string(const char *fn, size_t nargs, const QtValue *args) {
    if (strcmp(fn, "len") == 0 && nargs == 1) {
        return qt_int((int64_t)utf8_count(qt_to_string(args[0])));
    }
    if (strcmp(fn, "is_empty") == 0 && nargs == 1) {
        return qt_bool(qt_to_string(args[0])[0] == '\0');
    }
    if (strcmp(fn, "substring") == 0 && nargs == 3) {
        const char *s = qt_to_string(args[0]);
        int64_t i = qt_want_int(args[1], "string.substring");
        int64_t j = qt_want_int(args[2], "string.substring");
        size_t start = utf8_offset(s, i < 0 ? 0 : i);
        size_t end = utf8_offset(s, j < 0 ? 0 : j);
        if (end < start) {
            end = start;
        }
        return string_slice(s, start, end);
    }
    if (strcmp(fn, "char_at") == 0 && nargs == 2) {
        const char *s = qt_to_string(args[0]);
        int64_t i = qt_want_int(args[1], "string.char_at");
        size_t off = utf8_offset(s, i < 0 ? 0 : i);
        if (s[off] == '\0') {
            return qt_string("");
        }
        return string_slice(s, off, off + utf8_seq_len((unsigned char)s[off]));
    }
    if (strcmp(fn, "contains") == 0 && nargs == 2) {
        return qt_bool(strstr(qt_to_string(args[0]), qt_to_string(args[1])) != NULL);
    }
    if (strcmp(fn, "starts_with") == 0 && nargs == 2) {
        const char *s = qt_to_string(args[0]);
        const char *pre = qt_to_string(args[1]);
        return qt_bool(strncmp(s, pre, strlen(pre)) == 0);
    }
    if (strcmp(fn, "ends_with") == 0 && nargs == 2) {
        const char *s = qt_to_string(args[0]);
        const char *suf = qt_to_string(args[1]);
        size_t sl = strlen(s);
        size_t fl = strlen(suf);
        return qt_bool(fl <= sl && strcmp(s + sl - fl, suf) == 0);
    }
    if (strcmp(fn, "index_of") == 0 && nargs == 2) {
        return qt_int(string_index_of(qt_to_string(args[0]), qt_to_string(args[1]), 0));
    }
    if (strcmp(fn, "last_index_of") == 0 && nargs == 2) {
        return qt_int(string_index_of(qt_to_string(args[0]), qt_to_string(args[1]), 1));
    }
    if (strcmp(fn, "to_upper") == 0 && nargs == 1) {
        char *out = qt_strdup(qt_to_string(args[0]));
        for (char *p = out; *p; p++) {
            *p = (char)toupper((unsigned char)*p);
        }
        return qt_string(out);
    }
    if (strcmp(fn, "to_lower") == 0 && nargs == 1) {
        char *out = qt_strdup(qt_to_string(args[0]));
        for (char *p = out; *p; p++) {
            *p = (char)tolower((unsigned char)*p);
        }
        return qt_string(out);
    }
    if ((strcmp(fn, "trim") == 0 || strcmp(fn, "trim_left") == 0 ||
         strcmp(fn, "trim_right") == 0) &&
        nargs == 1) {
        const char *s = qt_to_string(args[0]);
        size_t start = 0;
        size_t end = strlen(s);
        int is_trim = strcmp(fn, "trim") == 0;
        int do_left = is_trim || strcmp(fn, "trim_left") == 0;
        int do_right = is_trim || strcmp(fn, "trim_right") == 0;
        if (do_left) {
            while (s[start] && isspace((unsigned char)s[start])) {
                start++;
            }
        }
        if (do_right) {
            while (end > start && isspace((unsigned char)s[end - 1])) {
                end--;
            }
        }
        return string_slice(s, start, end);
    }
    if (strcmp(fn, "reverse") == 0 && nargs == 1) {
        const char *s = qt_to_string(args[0]);
        size_t len = strlen(s);
        char *out = qt_alloc(len + 1);
        char *w = out + len;
        *w = '\0';
        for (const char *p = s; *p;) {
            size_t cl = utf8_seq_len((unsigned char)*p);
            w -= cl;
            memcpy(w, p, cl);
            p += cl;
        }
        return qt_string(out);
    }
    if (strcmp(fn, "replace") == 0 && nargs == 3) {
        return string_replace(qt_to_string(args[0]), qt_to_string(args[1]),
                              qt_to_string(args[2]), 0);
    }
    if (strcmp(fn, "replace_first") == 0 && nargs == 3) {
        return string_replace(qt_to_string(args[0]), qt_to_string(args[1]),
                              qt_to_string(args[2]), 1);
    }
    if (strcmp(fn, "repeat") == 0 && nargs == 2) {
        const char *s = qt_to_string(args[0]);
        int64_t n = qt_want_int(args[1], "string.repeat");
        if (n < 0) {
            n = 0;
        }
        size_t slen = strlen(s);
        char *out = qt_alloc(slen * (size_t)n + 1);
        char *w = out;
        for (int64_t k = 0; k < n; k++) {
            memcpy(w, s, slen);
            w += slen;
        }
        *w = '\0';
        return qt_string(out);
    }
    if (strcmp(fn, "to_int") == 0 && nargs == 1) {
        const char *s = qt_to_string(args[0]);
        char *end = NULL;
        long long v = strtoll(s, &end, 10);
        if (end == s || *end != '\0') {
            return qt_int(0); /* matches VM: reads must consume the whole string */
        }
        return qt_int((int64_t)v);
    }
    if (strcmp(fn, "to_float") == 0 && nargs == 1) {
        const char *s = qt_to_string(args[0]);
        char *end = NULL;
        double v = strtod(s, &end);
        if (end == s || *end != '\0') {
            return qt_float(0.0);
        }
        return qt_float(v);
    }
    if (strcmp(fn, "split") == 0 && nargs == 2) {
        const char *s = qt_to_string(args[0]);
        const char *sep = qt_to_string(args[1]);
        QtValue out = qt_array_new();
        size_t seplen = strlen(sep);
        if (seplen == 0) {
            qt_array_push(out, qt_string(qt_strdup(s)));
            return out;
        }
        const char *start = s;
        const char *p;
        while ((p = strstr(start, sep)) != NULL) {
            qt_array_push(out, string_slice(start, 0, (size_t)(p - start)));
            start = p + seplen;
        }
        qt_array_push(out, qt_string(qt_strdup(start)));
        return out;
    }
    if (strcmp(fn, "join") == 0 && nargs == 2 && args[0].tag == QT_ARRAY) {
        const char *sep = qt_to_string(args[1]);
        size_t seplen = strlen(sep);
        QtArray *a = args[0].as.arr;
        size_t total = (a->len > 0) ? seplen * (a->len - 1) : 0;
        for (size_t i = 0; i < a->len; i++) {
            total += strlen(qt_to_string(a->items[i]));
        }
        char *out = qt_alloc(total + 1);
        size_t w = 0;
        for (size_t i = 0; i < a->len; i++) {
            if (i > 0) {
                memcpy(out + w, sep, seplen);
                w += seplen;
            }
            const char *e = qt_to_string(a->items[i]);
            size_t el = strlen(e);
            memcpy(out + w, e, el);
            w += el;
        }
        out[w] = '\0';
        return qt_string(out);
    }
    if (strcmp(fn, "format") == 0 && nargs == 2 && args[1].tag == QT_ARRAY) {
        QtArray *a = args[1].as.arr;
        return qt_string(qt_apply_format(qt_to_string(args[0]), a->len, a->items));
    }
    char msg[128];
    snprintf(msg, sizeof(msg), "string.%s: bad arguments", fn);
    qt_panic(msg);
}

/* ------------------------------------------------------------------ */
/* sys.* / file.* / buf.* builtins (see ROADMAP: Native backend stage 4) */

/* Command-line arguments, captured by main() via qt_set_args. */
static int qt_argc = 0;
static char **qt_argv = NULL;

void qt_set_args(int argc, char **argv) {
    qt_argc = argc;
    qt_argv = argv;
}

/* Write a string's bytes to a raw fd, flushing stdio first so that output
 * ordering matches the VM.  Returns bytes written, or -1 on error. */
static int64_t fd_write_str(int64_t fd, const char *s) {
    if (fd == 1) {
        fflush(stdout);
    } else if (fd == 2) {
        fflush(stderr);
    }
    size_t len = strlen(s);
    ssize_t r = write((int)fd, s, len);
    return r < 0 ? -1 : (int64_t)r;
}

static const char *platform_name(void) {
#if defined(__APPLE__)
    return "macos";
#elif defined(_WIN32)
    return "windows";
#else
    return "linux";
#endif
}

static QtValue builtin_sys(const char *fn, size_t nargs, const QtValue *args) {
    /* Zero-argument fd numbers and POSIX open flags. */
    if (nargs == 0) {
        if (strcmp(fn, "stdin_fd") == 0) {
            return qt_int(0);
        }
        if (strcmp(fn, "stdout_fd") == 0) {
            return qt_int(1);
        }
        if (strcmp(fn, "stderr_fd") == 0) {
            return qt_int(2);
        }
        if (strcmp(fn, "o_rdonly") == 0) {
            return qt_int(0);
        }
        if (strcmp(fn, "o_wronly") == 0) {
            return qt_int(1);
        }
        if (strcmp(fn, "o_rdwr") == 0) {
            return qt_int(2);
        }
        if (strcmp(fn, "o_creat") == 0) {
            return qt_int(64);
        }
        if (strcmp(fn, "o_trunc") == 0) {
            return qt_int(512);
        }
        if (strcmp(fn, "o_append") == 0) {
            return qt_int(1024);
        }
        if (strcmp(fn, "time") == 0) {
            return qt_int((int64_t)time(NULL));
        }
        if (strcmp(fn, "time_millis") == 0) {
            return qt_int((int64_t)(clock() / (CLOCKS_PER_SEC / 1000)));
        }
        if (strcmp(fn, "argc") == 0) {
            return qt_int(qt_argc > 0 ? qt_argc - 1 : 0);
        }
        if (strcmp(fn, "args") == 0) {
            QtValue out = qt_array_new();
            for (int i = 1; i < qt_argc; i++) {
                qt_array_push(out, qt_string(qt_argv[i]));
            }
            return out;
        }
        if (strcmp(fn, "platform") == 0) {
            return qt_string(platform_name());
        }
        if (strcmp(fn, "hostname") == 0) {
            char buf[256];
            if (gethostname(buf, sizeof(buf)) != 0) {
                buf[0] = '\0';
            }
            buf[sizeof(buf) - 1] = '\0';
            return qt_string(qt_strdup(buf));
        }
        if (strcmp(fn, "getcwd") == 0) {
            char buf[4096];
            const char *r = getcwd(buf, sizeof(buf));
            return qt_string(qt_strdup(r ? r : ""));
        }
    }
    if (nargs == 1) {
        if (strcmp(fn, "sleep") == 0) {
            usleep((useconds_t)(qt_want_int(args[0], "sys.sleep") * 1000));
            return qt_unit();
        }
        if (strcmp(fn, "close") == 0) {
            return qt_bool(close((int)qt_want_int(args[0], "sys.close")) == 0);
        }
        if (strcmp(fn, "flush") == 0) {
            int64_t fd = qt_want_int(args[0], "sys.flush");
            if (fd == 1) {
                fflush(stdout);
            } else if (fd == 2) {
                fflush(stderr);
            }
            return qt_unit();
        }
        if (strcmp(fn, "isatty") == 0) {
            return qt_bool(isatty((int)qt_want_int(args[0], "sys.isatty")) == 1);
        }
        if (strcmp(fn, "env") == 0) {
            const char *v = getenv(qt_to_string(args[0]));
            return qt_string(v ? qt_strdup(v) : "");
        }
        if (strcmp(fn, "chdir") == 0) {
            return qt_bool(chdir(qt_to_string(args[0])) == 0);
        }
        if (strcmp(fn, "system") == 0) {
            int code = system(qt_to_string(args[0]));
            return qt_int(code == -1 ? -1 : (int64_t)((code >> 8) & 0xff));
        }
    }
    if (nargs == 2) {
        if (strcmp(fn, "write") == 0) {
            return qt_int(fd_write_str(qt_want_int(args[0], "sys.write"),
                                       qt_to_string(args[1])));
        }
        if (strcmp(fn, "read") == 0) {
            int64_t fd = qt_want_int(args[0], "sys.read");
            int64_t n = qt_want_int(args[1], "sys.read");
            if (n < 0) {
                n = 0;
            }
            char *buf = qt_alloc((size_t)n + 1);
            ssize_t r = read((int)fd, buf, (size_t)n);
            buf[r < 0 ? 0 : r] = '\0';
            return qt_string(buf);
        }
        if (strcmp(fn, "open") == 0) {
            const char *path = qt_to_string(args[0]);
            int64_t f = qt_want_int(args[1], "sys.open");
            int mode = (int)(f & 3);
            int oflags = (mode == 1) ? O_WRONLY : (mode == 2) ? O_RDWR : O_RDONLY;
            if (f & 64) {
                oflags |= O_CREAT;
            }
            if (f & 512) {
                oflags |= O_TRUNC;
            }
            if (f & 1024) {
                oflags |= O_APPEND;
            }
            int fd = open(path, oflags, 0644);
            return qt_int(fd);
        }
        if (strcmp(fn, "set_env") == 0) {
            setenv(qt_to_string(args[0]), qt_to_string(args[1]), 1);
            return qt_bool(1);
        }
    }
    if (strcmp(fn, "exit") == 0 && nargs == 1) {
        exit((int)qt_want_int(args[0], "sys.exit"));
    }
    char msg[128];
    snprintf(msg, sizeof(msg), "sys.%s: bad arguments", fn);
    qt_panic(msg);
}

static QtValue builtin_file(const char *fn, size_t nargs, const QtValue *args) {
    const char *path = qt_to_string(args[0]);
    if (strcmp(fn, "exists") == 0 && nargs == 1) {
        struct stat sb;
        return qt_bool(stat(path, &sb) == 0);
    }
    if (strcmp(fn, "delete") == 0 && nargs == 1) {
        return qt_bool(remove(path) == 0);
    }
    if (strcmp(fn, "size") == 0 && nargs == 1) {
        struct stat sb;
        return qt_int(stat(path, &sb) == 0 ? (int64_t)sb.st_size : -1);
    }
    if (strcmp(fn, "rename") == 0 && nargs == 2) {
        return qt_bool(rename(path, qt_to_string(args[1])) == 0);
    }
    if ((strcmp(fn, "read") == 0 || strcmp(fn, "lines") == 0) && nargs == 1) {
        FILE *fp = fopen(path, "rb");
        if (!fp) {
            return strcmp(fn, "read") == 0 ? qt_string("") : qt_array_new();
        }
        size_t cap = 4096;
        size_t len = 0;
        char *buf = qt_alloc(cap);
        size_t got;
        while ((got = fread(buf + len, 1, cap - len, fp)) > 0) {
            len += got;
            if (len == cap) {
                size_t ncap = cap * 2;
                char *nb = qt_alloc(ncap);
                memcpy(nb, buf, len);
                buf = nb;
                cap = ncap;
            }
        }
        fclose(fp);
        buf[len] = '\0';
        if (strcmp(fn, "read") == 0) {
            return qt_string(buf);
        }
        /* file.lines: split on '\n'; a trailing newline does not yield a
         * final empty element (matching Haskell's Data.Text.lines). */
        QtValue out = qt_array_new();
        size_t start = 0;
        for (size_t i = 0; i <= len; i++) {
            if (i == len || buf[i] == '\n') {
                if (i == len && start == len) {
                    break; /* no trailing empty line */
                }
                qt_array_push(out, string_slice(buf, start, i));
                start = i + 1;
            }
        }
        return out;
    }
    if ((strcmp(fn, "write") == 0 || strcmp(fn, "append") == 0) && nargs == 2) {
        FILE *fp = fopen(path, strcmp(fn, "append") == 0 ? "ab" : "wb");
        if (!fp) {
            return qt_bool(0);
        }
        const char *content = qt_to_string(args[1]);
        size_t clen = strlen(content);
        int ok = fwrite(content, 1, clen, fp) == clen;
        fclose(fp);
        return qt_bool(ok);
    }
    char msg[128];
    snprintf(msg, sizeof(msg), "file.%s: bad arguments", fn);
    qt_panic(msg);
}

static QtValue builtin_buf(const char *fn, size_t nargs, const QtValue *args) {
    if (strcmp(fn, "new") == 0 && nargs == 0) {
        return qt_array_new();
    }
    /* All remaining buf.* take the buffer (an array of strings) first. */
    QtArray *b = args[0].as.arr;
    if (strcmp(fn, "write") == 0 && nargs == 2) {
        qt_array_push(args[0], args[1]);
        return qt_unit();
    }
    if (strcmp(fn, "writeln") == 0 && nargs == 2) {
        qt_array_push(args[0], qt_string(qt_to_string(args[1])));
        qt_array_push(args[0], qt_string("\n"));
        return qt_unit();
    }
    if (strcmp(fn, "clear") == 0 && nargs == 1) {
        b->len = 0;
        return qt_unit();
    }
    if (strcmp(fn, "len") == 0 && nargs == 1) {
        int64_t total = 0;
        for (size_t i = 0; i < b->len; i++) {
            total += (int64_t)utf8_count(qt_to_string(b->items[i]));
        }
        return qt_int(total);
    }
    if ((strcmp(fn, "to_str") == 0 || strcmp(fn, "flush") == 0)) {
        size_t total = 0;
        for (size_t i = 0; i < b->len; i++) {
            total += strlen(qt_to_string(b->items[i]));
        }
        char *out = qt_alloc(total + 1);
        size_t w = 0;
        for (size_t i = 0; i < b->len; i++) {
            const char *e = qt_to_string(b->items[i]);
            size_t el = strlen(e);
            memcpy(out + w, e, el);
            w += el;
        }
        out[w] = '\0';
        if (strcmp(fn, "to_str") == 0 && nargs == 1) {
            return qt_string(out);
        }
        if (strcmp(fn, "flush") == 0 && nargs == 2) {
            int64_t r = fd_write_str(qt_want_int(args[1], "buf.flush"), out);
            b->len = 0;
            return qt_int(r);
        }
    }
    char msg[128];
    snprintf(msg, sizeof(msg), "buf.%s: bad arguments", fn);
    qt_panic(msg);
}

/* C11 does not guarantee M_PI; use the same value Haskell's `pi` yields. */
static const double QT_PI = 3.141592653589793;

static QtValue builtin_math(const char *fn, size_t nargs, const QtValue *args) {
    if (nargs == 0) {
        if (strcmp(fn, "pi") == 0) {
            return qt_float(QT_PI);
        }
        if (strcmp(fn, "tau") == 0) {
            return qt_float(2.0 * QT_PI);
        }
    }
    if (nargs == 1) {
        QtValue a = args[0];
        double f = (a.tag == QT_INT) ? (double)a.as.i : a.as.f;
        if (strcmp(fn, "sqrt") == 0) {
            return qt_float(sqrt(f));
        }
        if (strcmp(fn, "abs") == 0) {
            return a.tag == QT_INT ? qt_int(a.as.i < 0 ? -a.as.i : a.as.i)
                                   : qt_float(fabs(f));
        }
        if (strcmp(fn, "fabs") == 0) {
            return qt_float(fabs(f));
        }
        if (strcmp(fn, "floor") == 0) {
            return qt_int((int64_t)floor(f));
        }
        if (strcmp(fn, "ceil") == 0) {
            return qt_int((int64_t)ceil(f));
        }
        if (strcmp(fn, "round") == 0) {
            return qt_int(haskell_round(f));
        }
        if (strcmp(fn, "exp") == 0) {
            return qt_float(exp(f));
        }
        if (strcmp(fn, "log") == 0) {
            return qt_float(log(f));
        }
        if (strcmp(fn, "log2") == 0) {
            return qt_float(log(f) / log(2.0));
        }
        if (strcmp(fn, "log10") == 0) {
            return qt_float(log(f) / log(10.0));
        }
        if (strcmp(fn, "sin") == 0) {
            return qt_float(sin(f));
        }
        if (strcmp(fn, "cos") == 0) {
            return qt_float(cos(f));
        }
        if (strcmp(fn, "tan") == 0) {
            return qt_float(tan(f));
        }
        if (strcmp(fn, "asin") == 0) {
            return qt_float(asin(f));
        }
        if (strcmp(fn, "acos") == 0) {
            return qt_float(acos(f));
        }
        if (strcmp(fn, "atan") == 0) {
            return qt_float(atan(f));
        }
    }
    if (nargs == 2) {
        QtValue a = args[0];
        QtValue b = args[1];
        double fa = (a.tag == QT_INT) ? (double)a.as.i : a.as.f;
        double fb = (b.tag == QT_INT) ? (double)b.as.i : b.as.f;
        if (strcmp(fn, "pow") == 0) {
            return qt_float(pow(fa, fb));
        }
        if (strcmp(fn, "atan2") == 0) {
            return qt_float(atan2(fa, fb));
        }
        if (strcmp(fn, "min") == 0) {
            return qt_int(a.as.i < b.as.i ? a.as.i : b.as.i);
        }
        if (strcmp(fn, "max") == 0) {
            return qt_int(a.as.i > b.as.i ? a.as.i : b.as.i);
        }
        if (strcmp(fn, "fmin") == 0) {
            return qt_float(fa < fb ? fa : fb);
        }
        if (strcmp(fn, "fmax") == 0) {
            return qt_float(fa > fb ? fa : fb);
        }
    }
    char msg[128];
    snprintf(msg, sizeof(msg), "math.%s: bad arguments", fn);
    qt_panic(msg);
}

/* ------------------------------------------------------------------ */
/* ptr.* raw-memory builtins (see ROADMAP: Native backend stage 8)      */

static QtValue builtin_ptr(const char *fn, size_t nargs, const QtValue *args) {
    if (strcmp(fn, "null") == 0 && nargs == 0) {
        return qt_ptr(0);
    }
    if (strcmp(fn, "is_null") == 0 && nargs == 1) {
        return qt_bool(args[0].as.ptr == 0);
    }
    if (strcmp(fn, "to_int") == 0 && nargs == 1) {
        return qt_int((int64_t)args[0].as.ptr);
    }
    if (strcmp(fn, "from_int") == 0 && nargs == 1) {
        return qt_ptr((uint64_t)qt_want_int(args[0], "ptr.from_int"));
    }
    if (strcmp(fn, "add") == 0 && nargs == 2) {
        return qt_ptr(args[0].as.ptr + (uint64_t)qt_want_int(args[1], "ptr.add"));
    }
    void *p = (void *)(uintptr_t)args[0].as.ptr;
    if (strcmp(fn, "read_int32") == 0 && nargs == 1) {
        int32_t v;
        memcpy(&v, p, sizeof(v));
        return qt_int(v);
    }
    if (strcmp(fn, "write_int32") == 0 && nargs == 2) {
        int32_t v = (int32_t)qt_want_int(args[1], "ptr.write_int32");
        memcpy(p, &v, sizeof(v));
        return qt_unit();
    }
    if (strcmp(fn, "read_int64") == 0 && nargs == 1) {
        int64_t v;
        memcpy(&v, p, sizeof(v));
        return qt_int(v);
    }
    if (strcmp(fn, "write_int64") == 0 && nargs == 2) {
        int64_t v = qt_want_int(args[1], "ptr.write_int64");
        memcpy(p, &v, sizeof(v));
        return qt_unit();
    }
    if (strcmp(fn, "read_float64") == 0 && nargs == 1) {
        double v;
        memcpy(&v, p, sizeof(v));
        return qt_float(v);
    }
    if (strcmp(fn, "write_float64") == 0 && nargs == 2) {
        double v = args[1].as.f;
        memcpy(p, &v, sizeof(v));
        return qt_unit();
    }
    char msg[128];
    snprintf(msg, sizeof(msg), "ptr.%s: bad arguments", fn);
    qt_panic(msg);
}

/* ------------------------------------------------------------------ */
/* FFI: dlopen + hand-written trampolines, no libffi (stage 8).         */

/* Dispatch back into generated code (registered by main) for callbacks. */
static QtValue (*g_dispatch)(const char *, size_t, const QtValue *) = NULL;

void qt_set_dispatch(QtValue (*fn)(const char *, size_t, const QtValue *)) {
    g_dispatch = fn;
}

/* The Quant callable currently installed as a C callback. */
static QtValue g_ffi_cb;

/* Arity lookup by function name, registered by generated code. */
static int (*g_arity)(const char *) = NULL;

void qt_set_arity(int (*fn)(const char *)) {
    g_arity = fn;
}

/* Shared callback body: wrap up to four pointer args as QtValue pointers,
 * re-enter Quant through the registered dispatcher, and return an int-class
 * result (a callback declared void simply has its result ignored). */
static int cb_invoke(size_t n, const void *p0, const void *p1, const void *p2,
                     const void *p3) {
    const void *ps[4] = {p0, p1, p2, p3};
    QtValue cargs[4];
    for (size_t i = 0; i < n; i++) {
        cargs[i] = qt_ptr((uint64_t)(uintptr_t)ps[i]);
    }
    qt_callable_bind(g_ffi_cb);
    QtValue r = g_dispatch(qt_callable_name(g_ffi_cb), n, cargs);
    if (r.tag == QT_INT) {
        return (int)r.as.i;
    }
    if (r.tag == QT_BOOL) {
        return r.as.i ? 1 : 0;
    }
    return 0;
}

/* Fixed-arity trampolines with C-visible pointer signatures (arity 0-4). */
static int qt_ffi_cb0(void) { return cb_invoke(0, 0, 0, 0, 0); }
static int qt_ffi_cb1(const void *a) { return cb_invoke(1, a, 0, 0, 0); }
static int qt_ffi_cb2(const void *a, const void *b) {
    return cb_invoke(2, a, b, 0, 0);
}
static int qt_ffi_cb3(const void *a, const void *b, const void *c) {
    return cb_invoke(3, a, b, c, 0);
}
static int qt_ffi_cb4(const void *a, const void *b, const void *c,
                      const void *d) {
    return cb_invoke(4, a, b, c, d);
}

/* Select the trampoline whose arity matches the Quant callback. */
static void *cb_trampoline(int arity) {
    switch (arity) {
        case 0: return (void *)&qt_ffi_cb0;
        case 1: return (void *)&qt_ffi_cb1;
        case 3: return (void *)&qt_ffi_cb3;
        case 4: return (void *)&qt_ffi_cb4;
        default: return (void *)&qt_ffi_cb2;
    }
}

/* One marshalled argument: either an integer/pointer slot or a double. */
typedef struct {
    uint64_t u;
    double d;
} FfiSlot;

QtValue qt_ffi_call(const char *lib, const char *sym, int ret, size_t argc,
                    const QtValue *args) {
    void *h = dlopen(lib, RTLD_LAZY);
    if (!h) {
        qt_panic("ffi: cannot open library");
    }
    void *fp = dlsym(h, sym);
    if (!fp) {
        qt_panic("ffi: symbol not found");
    }
    if (argc > 6) {
        qt_panic("ffi: too many arguments");
    }
    FfiSlot s[6];
    unsigned m = 0; /* bit i set => argument i is a double */
    for (size_t i = 0; i < argc; i++) {
        QtValue v = args[i];
        s[i].u = 0;
        s[i].d = 0.0;
        switch (v.tag) {
            case QT_FLOAT:
                s[i].d = v.as.f;
                m |= (1u << i);
                break;
            case QT_BOOL:
                s[i].u = v.as.i ? 1 : 0;
                break;
            case QT_STRING:
                s[i].u = (uint64_t)(uintptr_t)v.as.s;
                break;
            case QT_PTR:
                s[i].u = v.as.ptr;
                break;
            case QT_FN:
            case QT_CLOSURE: {
                g_ffi_cb = v;
                int ar = g_arity ? g_arity(qt_callable_name(v)) : 2;
                s[i].u = (uint64_t)(uintptr_t)cb_trampoline(ar);
                break;
            }
            default:
                s[i].u = (uint64_t)v.as.i;
                break;
        }
    }
    int rf = (ret == 2); /* CRetFloat */
    uint64_t ir = 0;
    double fr = 0.0;
#define U(i) (s[i].u)
#define D(i) (s[i].d)
#define CALL(PROTO, ...)                                                       \
    do {                                                                       \
        if (rf)                                                                \
            fr = ((double(*) PROTO)fp)(__VA_ARGS__);                           \
        else                                                                   \
            ir = ((uint64_t(*) PROTO)fp)(__VA_ARGS__);                         \
    } while (0)
    switch (argc) {
        case 0:
            CALL((void));
            break;
        case 1:
            if (m == 1) {
                CALL((double), D(0));
            } else {
                CALL((uint64_t), U(0));
            }
            break;
        case 2:
            switch (m) {
                case 0: CALL((uint64_t, uint64_t), U(0), U(1)); break;
                case 1: CALL((double, uint64_t), D(0), U(1)); break;
                case 2: CALL((uint64_t, double), U(0), D(1)); break;
                case 3: CALL((double, double), D(0), D(1)); break;
            }
            break;
        case 3:
            switch (m) {
                case 0: CALL((uint64_t, uint64_t, uint64_t), U(0), U(1), U(2)); break;
                case 1: CALL((double, uint64_t, uint64_t), D(0), U(1), U(2)); break;
                case 2: CALL((uint64_t, double, uint64_t), U(0), D(1), U(2)); break;
                case 3: CALL((double, double, uint64_t), D(0), D(1), U(2)); break;
                case 4: CALL((uint64_t, uint64_t, double), U(0), U(1), D(2)); break;
                case 5: CALL((double, uint64_t, double), D(0), U(1), D(2)); break;
                case 6: CALL((uint64_t, double, double), U(0), D(1), D(2)); break;
                case 7: CALL((double, double, double), D(0), D(1), D(2)); break;
            }
            break;
        case 4:
            if (m != 0) {
                qt_panic("ffi: float arguments past position 3 unsupported");
            }
            CALL((uint64_t, uint64_t, uint64_t, uint64_t), U(0), U(1), U(2), U(3));
            break;
        case 5:
            if (m != 0) {
                qt_panic("ffi: float arguments past position 3 unsupported");
            }
            CALL((uint64_t, uint64_t, uint64_t, uint64_t, uint64_t), U(0), U(1),
                 U(2), U(3), U(4));
            break;
        case 6:
            if (m != 0) {
                qt_panic("ffi: float arguments past position 3 unsupported");
            }
            CALL((uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t),
                 U(0), U(1), U(2), U(3), U(4), U(5));
            break;
    }
#undef CALL
#undef U
#undef D
    switch (ret) {
        case 0: return qt_unit();             /* CRetVoid */
        case 1: return qt_int((int64_t)ir);   /* CRetInt */
        case 2: return qt_float(fr);          /* CRetFloat */
        case 3:                               /* CRetStr */
            return qt_string(ir ? qt_strdup((const char *)(uintptr_t)ir) : "");
        case 4: return qt_bool((uint32_t)ir != 0); /* CRetBool */
        case 5: return qt_ptr(ir);            /* CRetPtr */
    }
    return qt_unit();
}

/* ------------------------------------------------------------------ */
/* socket.* TCP builtins.  The socket id handed back to Quant is the raw   */
/* file descriptor (the VM uses a heap index instead, but programs only    */
/* pass the id back to socket.* calls, so the two are interchangeable).    */

static QtValue builtin_socket(const char *fn, size_t nargs, const QtValue *args) {
    if (strcmp(fn, "connect") == 0 && nargs == 2) {
        const char *host = qt_to_string(args[0]);
        char port[16];
        snprintf(port, sizeof(port), "%" PRId64,
                 qt_want_int(args[1], "socket.connect"));
        struct addrinfo hints;
        memset(&hints, 0, sizeof(hints));
        hints.ai_socktype = SOCK_STREAM;
        struct addrinfo *res = NULL;
        if (getaddrinfo(host, port, &hints, &res) != 0 || !res) {
            return qt_int(-1);
        }
        int fd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
        int ok = (fd >= 0) && (connect(fd, res->ai_addr, res->ai_addrlen) == 0);
        freeaddrinfo(res);
        if (!ok) {
            if (fd >= 0) {
                close(fd);
            }
            return qt_int(-1);
        }
        return qt_int(fd);
    }
    if (strcmp(fn, "listen") == 0 && nargs == 2) {
        char port[16];
        snprintf(port, sizeof(port), "%" PRId64,
                 qt_want_int(args[0], "socket.listen"));
        int backlog = (int)qt_want_int(args[1], "socket.listen");
        struct addrinfo hints;
        memset(&hints, 0, sizeof(hints));
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_flags = AI_PASSIVE;
        struct addrinfo *res = NULL;
        if (getaddrinfo(NULL, port, &hints, &res) != 0 || !res) {
            return qt_int(-1);
        }
        int fd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
        int one = 1;
        int ok = (fd >= 0) &&
                 (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one)) == 0) &&
                 (bind(fd, res->ai_addr, res->ai_addrlen) == 0) &&
                 (listen(fd, backlog) == 0);
        freeaddrinfo(res);
        if (!ok) {
            if (fd >= 0) {
                close(fd);
            }
            return qt_int(-1);
        }
        return qt_int(fd);
    }
    if (strcmp(fn, "accept") == 0 && nargs == 1) {
        int c = accept((int)qt_want_int(args[0], "socket.accept"), NULL, NULL);
        return qt_int(c);
    }
    if (strcmp(fn, "send") == 0 && nargs == 2) {
        int fd = (int)qt_want_int(args[0], "socket.send");
        const char *s = qt_to_string(args[1]);
        ssize_t n = send(fd, s, strlen(s), 0);
        return qt_int(n < 0 ? -1 : (int64_t)n);
    }
    if (strcmp(fn, "recv") == 0 && nargs == 2) {
        int fd = (int)qt_want_int(args[0], "socket.recv");
        int64_t n = qt_want_int(args[1], "socket.recv");
        if (n < 0) {
            n = 0;
        }
        char *buf = qt_alloc((size_t)n + 1);
        ssize_t r = recv(fd, buf, (size_t)n, 0);
        buf[r < 0 ? 0 : r] = '\0';
        return qt_string(buf);
    }
    if (strcmp(fn, "close") == 0 && nargs == 1) {
        return qt_bool(close((int)qt_want_int(args[0], "socket.close")) == 0);
    }
    if (strcmp(fn, "peer_addr") == 0 && nargs == 1) {
        int fd = (int)qt_want_int(args[0], "socket.peer_addr");
        struct sockaddr_storage ss;
        socklen_t len = sizeof(ss);
        if (getpeername(fd, (struct sockaddr *)&ss, &len) != 0) {
            return qt_string("");
        }
        char host[INET6_ADDRSTRLEN];
        char out[INET6_ADDRSTRLEN + 8];
        if (ss.ss_family == AF_INET) {
            struct sockaddr_in *a = (struct sockaddr_in *)&ss;
            inet_ntop(AF_INET, &a->sin_addr, host, sizeof(host));
            snprintf(out, sizeof(out), "%s:%d", host, ntohs(a->sin_port));
        } else {
            struct sockaddr_in6 *a = (struct sockaddr_in6 *)&ss;
            inet_ntop(AF_INET6, &a->sin6_addr, host, sizeof(host));
            snprintf(out, sizeof(out), "%s:%d", host, ntohs(a->sin6_port));
        }
        return qt_string(qt_strdup(out));
    }
    char msg[128];
    snprintf(msg, sizeof(msg), "socket.%s: bad arguments", fn);
    qt_panic(msg);
}

/* ------------------------------------------------------------------ */
/* Async / await: cooperative ucontext scheduler (stage 9).             */
/*                                                                      */
/* Each task runs on its own stack.  Spawning queues a task without     */
/* running it; awaiting an unfinished task yields to the scheduler,      */
/* which drains the ready queue (FIFO) until the awaited task is done    */
/* and re-queues waiters on completion -- the same advance-at-await,     */
/* run-ready-in-order semantics as the VM scheduler.                     */

#define QT_TASK_CAP 4096
#define QT_TASK_STACK (1u << 20) /* 1 MiB per task stack */

typedef struct {
    ucontext_t ctx;
    const char *fname;
    QtValue *args;
    size_t argc;
    QtValue result;
    int started;
    int done;
    long waiting_on; /* task id being awaited, or -1 */
} QtTask;

static QtTask *g_tasks[QT_TASK_CAP];
static long g_ntasks = 0;
static long g_ready[QT_TASK_CAP];
static long g_ready_head = 0;
static long g_ready_tail = 0;
static ucontext_t g_sched_ctx;
static long g_cur = -1;

static void ready_push(long id) {
    if (g_ready_tail - g_ready_head >= QT_TASK_CAP) {
        qt_panic("async: ready queue overflow");
    }
    g_ready[g_ready_tail++ % QT_TASK_CAP] = id;
}

static long ready_pop(void) {
    return g_ready[g_ready_head++ % QT_TASK_CAP];
}

/* Task body: dispatch into the generated function, store the result, then
 * wake every task that was awaiting this one and hand control back. */
static void qt_task_entry(void) {
    long id = g_cur;
    QtTask *t = g_tasks[id];
    t->result = g_dispatch(t->fname, t->argc, t->args);
    t->done = 1;
    for (long i = 0; i < g_ntasks; i++) {
        if (!g_tasks[i]->done && g_tasks[i]->waiting_on == id) {
            g_tasks[i]->waiting_on = -1;
            ready_push(i);
        }
    }
    swapcontext(&t->ctx, &g_sched_ctx); /* never resumes: task is done */
}

QtValue qt_spawn(const char *name, size_t argc, const QtValue *args) {
    if (g_ntasks >= QT_TASK_CAP) {
        qt_panic("async: too many tasks");
    }
    long id = g_ntasks++;
    QtTask *t = qt_alloc(sizeof(QtTask));
    t->fname = name;
    t->argc = argc;
    t->args = NULL;
    t->started = 0;
    t->done = 0;
    t->waiting_on = -1;
    if (argc > 0) {
        t->args = qt_alloc(argc * sizeof(QtValue));
        memcpy(t->args, args, argc * sizeof(QtValue));
    }
    g_tasks[id] = t;
    ready_push(id);
    return qt_task(id);
}

QtValue qt_await(QtValue tv) {
    long id = (long)tv.as.i;
    QtTask *t = g_tasks[id];
    if (!t->done) {
        QtTask *self = g_tasks[g_cur];
        self->waiting_on = id;
        swapcontext(&self->ctx, &g_sched_ctx); /* yield; resumed when t is done */
    }
    return t->result;
}

/* Spawn the entry function as task 0 and run the scheduler to completion. */
void qt_async_run(const char *main_name) {
    qt_spawn(main_name, 0, NULL);
    while (g_ready_head != g_ready_tail) {
        long id = ready_pop();
        g_cur = id;
        QtTask *t = g_tasks[id];
        if (!t->started) {
            t->started = 1;
            getcontext(&t->ctx);
            t->ctx.uc_stack.ss_sp = qt_alloc(QT_TASK_STACK);
            t->ctx.uc_stack.ss_size = QT_TASK_STACK;
            t->ctx.uc_link = &g_sched_ctx;
            makecontext(&t->ctx, qt_task_entry, 0);
        }
        swapcontext(&g_sched_ctx, &t->ctx);
    }
}

QtValue qt_call_builtin(const char *name, size_t nargs, const QtValue *args) {
    if (strcmp(name, "print") == 0 || strcmp(name, "io.print") == 0) {
        if (nargs > 0) {
            if (nargs == 1) {
                qt_print(args[0]);
            } else {
                qt_print_fmt(qt_render(args[0]), nargs - 1, args + 1);
            }
        }
        return qt_unit();
    }
    if (strcmp(name, "println") == 0 || strcmp(name, "io.println") == 0) {
        if (nargs == 0) {
            fputc('\n', stdout);
        } else if (nargs == 1) {
            qt_println(args[0]);
        } else {
            qt_println_fmt(qt_render(args[0]), nargs - 1, args + 1);
        }
        return qt_unit();
    }
    if (strcmp(name, "string.to_str") == 0 && nargs == 1) {
        return qt_string(qt_to_string(args[0]));
    }
    if (strcmp(name, "string.concat") == 0) {
        return builtin_concat(nargs, args);
    }
    if (strcmp(name, "string.from_int") == 0 && nargs == 1) {
        return qt_string(qt_to_string(args[0]));
    }
    if (strcmp(name, "string.from_float") == 0 && nargs == 1) {
        return qt_string(qt_to_string(args[0]));
    }
    if ((strcmp(name, "len") == 0 || strcmp(name, "array.len") == 0) &&
        nargs == 1) {
        if (args[0].tag == QT_STRING) {
            return qt_int((int64_t)strlen(args[0].as.s));
        }
        return qt_int(qt_array_len(args[0]));
    }
    if ((strcmp(name, "push") == 0 || strcmp(name, "array.push") == 0) &&
        nargs == 2) {
        qt_array_push(args[0], args[1]);
        return qt_unit();
    }
    if ((strcmp(name, "pop") == 0 || strcmp(name, "array.pop") == 0) &&
        nargs == 1) {
        return qt_array_pop(args[0]);
    }
    if (strncmp(name, "string.", 7) == 0) {
        return builtin_string(name + 7, nargs, args);
    }
    if (strncmp(name, "math.", 5) == 0) {
        return builtin_math(name + 5, nargs, args);
    }
    if (strncmp(name, "sys.", 4) == 0) {
        return builtin_sys(name + 4, nargs, args);
    }
    if (strncmp(name, "file.", 5) == 0) {
        return builtin_file(name + 5, nargs, args);
    }
    if (strncmp(name, "buf.", 4) == 0) {
        return builtin_buf(name + 4, nargs, args);
    }
    if (strcmp(name, "dict.has") == 0 && nargs == 2) {
        return qt_bool(qt_dict_has(args[0], args[1]));
    }
    if (strcmp(name, "dict.len") == 0 && nargs == 1) {
        return qt_int(qt_dict_len(args[0]));
    }
    if (strcmp(name, "dict.delete") == 0 && nargs == 2) {
        qt_dict_delete(args[0], args[1]);
        return qt_unit();
    }
    if (strncmp(name, "ptr.", 4) == 0) {
        return builtin_ptr(name + 4, nargs, args);
    }
    if (strncmp(name, "socket.", 7) == 0) {
        return builtin_socket(name + 7, nargs, args);
    }
    char msg[256];
    snprintf(msg, sizeof(msg), "native backend: unsupported builtin `%s`",
             name);
    qt_panic(msg);
}

/* ------------------------------------------------------------------ */
/* Failure                                                             */

void qt_panic(const char *msg) {
    fprintf(stderr, "runtime error: %s\n", msg);
    exit(1);
}

/* quant_runtime.c - C runtime for the Quant native backend (stage 1). */

#include "quant_runtime.h"

#include <ctype.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    char msg[128];
    snprintf(msg, sizeof(msg), "string.%s: bad arguments", fn);
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
    if (strcmp(name, "sys.exit") == 0 && nargs == 1) {
        exit((int)qt_want_int(args[0], "sys.exit"));
    }
    if (strncmp(name, "string.", 7) == 0) {
        return builtin_string(name + 7, nargs, args);
    }
    if (strncmp(name, "math.", 5) == 0) {
        return builtin_math(name + 5, nargs, args);
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

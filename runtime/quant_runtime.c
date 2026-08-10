/* quant_runtime.c - C runtime for the Quant native backend (stage 1). */

#include "quant_runtime.h"

#include <inttypes.h>
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

void qt_format_float(char *buf, size_t bufsize, double f) {
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
    /* VM formatFloat: any integral float prints as "<digits>.0" regardless
     * of magnitude; non-integral floats follow Haskell show, which uses
     * fixed notation for 0.1 <= x < 1e7 and scientific outside. */
    int intdigits = exp10 + 1;
    int integral = intdigits >= nd;
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
/* Failure                                                             */

void qt_panic(const char *msg) {
    fprintf(stderr, "runtime error: %s\n", msg);
    exit(1);
}

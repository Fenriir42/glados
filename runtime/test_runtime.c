/* test_runtime.c - unit tests for the Quant C runtime (stage 1).
 *
 * Build and run: make runtime-test
 */

#include "quant_runtime.h"

#include <stdio.h>
#include <string.h>

static int tests_run = 0;
static int tests_failed = 0;

#define CHECK(cond)                                                     \
    do {                                                                \
        tests_run++;                                                    \
        if (!(cond)) {                                                  \
            tests_failed++;                                             \
            fprintf(stderr, "FAIL %s:%d: %s\n", __FILE__, __LINE__, #cond); \
        }                                                               \
    } while (0)

#define CHECK_STR(actual, expected)                                     \
    do {                                                                \
        tests_run++;                                                    \
        const char *a_ = (actual);                                      \
        const char *e_ = (expected);                                    \
        if (strcmp(a_, e_) != 0) {                                      \
            tests_failed++;                                             \
            fprintf(stderr, "FAIL %s:%d: got \"%s\", want \"%s\"\n",    \
                    __FILE__, __LINE__, a_, e_);                        \
        }                                                               \
    } while (0)

static void test_scalars(void) {
    CHECK(qt_int(42).as.i == 42);
    CHECK(qt_bool(1).as.i == 1);
    CHECK(qt_bool(7).as.i == 1); /* normalised */
    CHECK(qt_float(3.14).as.f == 3.14);
    CHECK(qt_unit().tag == QT_UNIT);
    CHECK(qt_ptr(0xdead).as.ptr == 0xdead);
}

static void test_float_format(void) {
    char buf[32];
    qt_format_float(buf, sizeof(buf), 1.0);
    CHECK_STR(buf, "1.0");
    qt_format_float(buf, sizeof(buf), 0.0);
    CHECK_STR(buf, "0.0");
    qt_format_float(buf, sizeof(buf), -4.0);
    CHECK_STR(buf, "-4.0");
    qt_format_float(buf, sizeof(buf), 3.14);
    CHECK_STR(buf, "3.14");
    qt_format_float(buf, sizeof(buf), 78.53975);
    CHECK_STR(buf, "78.53975");
    qt_format_float(buf, sizeof(buf), 0.1);
    CHECK_STR(buf, "0.1");
    qt_format_float(buf, sizeof(buf), -12.5);
    CHECK_STR(buf, "-12.5");
    qt_format_float(buf, sizeof(buf), 0.5);
    CHECK_STR(buf, "0.5");
    /* integral floats stay fixed at any magnitude (VM formatFloat) */
    qt_format_float(buf, sizeof(buf), 25000000000.0);
    CHECK_STR(buf, "25000000000.0");
    qt_format_float(buf, sizeof(buf), 12345678.0);
    CHECK_STR(buf, "12345678.0");
    qt_format_float(buf, sizeof(buf), 9999999.0);
    CHECK_STR(buf, "9999999.0");
    /* non-integral floats follow Haskell show: scientific outside [0.1, 1e7) */
    qt_format_float(buf, sizeof(buf), 0.001);
    CHECK_STR(buf, "1.0e-3");
    qt_format_float(buf, sizeof(buf), -0.001);
    CHECK_STR(buf, "-1.0e-3");
    qt_format_float(buf, sizeof(buf), 125000000.5);
    CHECK_STR(buf, "1.250000005e8");
}

static void test_render(void) {
    CHECK_STR(qt_render(qt_int(-7)), "-7");
    CHECK_STR(qt_render(qt_string("hi")), "hi");
    CHECK_STR(qt_render(qt_bool(1)), "True");
    CHECK_STR(qt_render(qt_bool(0)), "False");
    CHECK_STR(qt_render(qt_unit()), "");
    CHECK_STR(qt_render(qt_float(2.0)), "2.0");
    CHECK_STR(qt_render(qt_error_new("DivisionError")), "error DivisionError");

    /* string conversion differs from print for booleans, like the VM */
    CHECK_STR(qt_to_string(qt_bool(1)), "true");
    CHECK_STR(qt_to_string(qt_bool(0)), "false");
    CHECK_STR(qt_to_string(qt_int(5)), "5");
    CHECK_STR(qt_to_string(qt_unit()), "");
    CHECK_STR(qt_to_string(qt_array_new()), "");
}

static void test_apply_format(void) {
    QtValue args1[2] = {qt_int(1), qt_int(2)};
    CHECK_STR(qt_apply_format("x=%d y=%d", 2, args1), "x=1 y=2");
    /* one arg for two holes: leftover directive is verbatim */
    CHECK_STR(qt_apply_format("x=%d y=%d", 1, args1), "x=1 y=%d");

    QtValue args2[1] = {qt_string("world")};
    CHECK_STR(qt_apply_format("hello %s", 1, args2), "hello world");

    QtValue args3[1] = {qt_float(1.5)};
    CHECK_STR(qt_apply_format("f=%f", 1, args3), "f=1.5");

    /* %d on a float rounds half-to-even like Haskell round */
    QtValue argsf[2] = {qt_float(2.5), qt_float(3.5)};
    CHECK_STR(qt_apply_format("%d %d", 2, argsf), "2 4");

    /* %% is literal and does not consume an argument (while args remain) */
    QtValue args4[2] = {qt_int(5), qt_int(6)};
    CHECK_STR(qt_apply_format("%%=%d", 1, args4), "%=5");
    /* ...but once args are exhausted the tail is verbatim, %% included */
    CHECK_STR(qt_apply_format("%d%%", 1, args4), "5%%");

    /* bare % is not a directive: copied verbatim, argument kept */
    CHECK_STR(qt_apply_format("a=% b=%d", 1, args4), "a=% b=5");

    /* %s renders scalars via string conversion */
    QtValue args5[2] = {qt_int(7), qt_bool(1)};
    CHECK_STR(qt_apply_format("%s %s", 2, args5), "7 true");

    CHECK_STR(qt_apply_format("no holes", 0, NULL), "no holes");
}

static void test_arrays(void) {
    QtValue a = qt_array_new();
    CHECK(qt_array_len(a) == 0);
    CHECK(qt_array_pop(a).tag == QT_UNIT); /* pop on empty: unit */

    qt_array_push(a, qt_int(10));
    qt_array_push(a, qt_int(20));
    CHECK(qt_array_len(a) == 2);
    CHECK(qt_array_get(a, 0).as.i == 10);
    CHECK(qt_array_get(a, 1).as.i == 20);

    qt_array_set(a, 1, qt_int(21));
    CHECK(qt_array_get(a, 1).as.i == 21);

    /* setting past the end grows and unit-fills the gap */
    qt_array_set(a, 5, qt_int(50));
    CHECK(qt_array_len(a) == 6);
    CHECK(qt_array_get(a, 3).tag == QT_UNIT);
    CHECK(qt_array_get(a, 5).as.i == 50);

    QtValue popped = qt_array_pop(a);
    CHECK(popped.as.i == 50);
    CHECK(qt_array_len(a) == 5);

    /* growth across reallocation keeps contents */
    QtValue b = qt_array_new();
    for (int i = 0; i < 100; i++) {
        qt_array_push(b, qt_int(i));
    }
    CHECK(qt_array_len(b) == 100);
    CHECK(qt_array_get(b, 0).as.i == 0);
    CHECK(qt_array_get(b, 99).as.i == 99);
}

static void test_dicts(void) {
    QtValue d = qt_dict_new();
    CHECK(qt_dict_len(d) == 0);
    CHECK(!qt_dict_has(d, qt_string("a")));
    CHECK(qt_dict_get(d, qt_string("a")).tag == QT_UNIT);

    qt_dict_set(d, qt_string("a"), qt_int(1));
    qt_dict_set(d, qt_string("b"), qt_int(2));
    CHECK(qt_dict_len(d) == 2);
    CHECK(qt_dict_has(d, qt_string("a")));
    CHECK(qt_dict_get(d, qt_string("b")).as.i == 2);

    /* string keys are compared structurally, not by pointer */
    char key[2] = {'a', '\0'};
    CHECK(qt_dict_has(d, qt_string(key)));

    /* update in place */
    qt_dict_set(d, qt_string("a"), qt_int(11));
    CHECK(qt_dict_len(d) == 2);
    CHECK(qt_dict_get(d, qt_string("a")).as.i == 11);

    /* int keys */
    qt_dict_set(d, qt_int(42), qt_string("answer"));
    CHECK_STR(qt_dict_get(d, qt_int(42)).as.s, "answer");

    qt_dict_delete(d, qt_string("a"));
    CHECK(qt_dict_len(d) == 2);
    CHECK(!qt_dict_has(d, qt_string("a")));
    qt_dict_delete(d, qt_string("missing")); /* no-op */
    CHECK(qt_dict_len(d) == 2);
}

static void test_structs(void) {
    QtValue p = qt_struct_new("Point");
    CHECK_STR(qt_struct_type(p), "Point");

    qt_struct_set(p, "x", qt_int(3));
    qt_struct_set(p, "y", qt_int(4));
    CHECK(qt_struct_get(p, "x").as.i == 3);
    CHECK(qt_struct_get(p, "y").as.i == 4);

    qt_struct_set(p, "x", qt_int(30));
    CHECK(qt_struct_get(p, "x").as.i == 30);

    /* struct values are references: mutation is visible through aliases */
    QtValue alias = p;
    qt_struct_set(alias, "y", qt_int(40));
    CHECK(qt_struct_get(p, "y").as.i == 40);
}

static void test_errors(void) {
    QtValue e = qt_error_new("ParseError");
    CHECK(qt_is_error(e));
    CHECK(qt_error_is(e, "ParseError"));
    CHECK(!qt_error_is(e, "DivisionError"));
    CHECK(!qt_error_is(qt_int(1), "ParseError"));
    CHECK(!qt_is_error(qt_int(1)));

    qt_error_set_field(e, "line", qt_int(12));
    CHECK(qt_error_get_field(e, "line").as.i == 12);

    /* enum variants use the same representation */
    QtValue north = qt_error_new("North");
    CHECK(qt_error_is(north, "North"));
    QtValue circle = qt_error_new("Circle");
    qt_error_set_field(circle, "radius", qt_float(5.0));
    CHECK(qt_error_get_field(circle, "radius").as.f == 5.0);
}

static void test_equality(void) {
    CHECK(qt_value_eq(qt_int(3), qt_int(3)));
    CHECK(!qt_value_eq(qt_int(3), qt_int(4)));
    CHECK(!qt_value_eq(qt_int(3), qt_float(3.0))); /* tags differ */
    CHECK(qt_value_eq(qt_bool(1), qt_bool(1)));
    CHECK(qt_value_eq(qt_unit(), qt_unit()));

    /* strings: structural */
    char s1[6] = "hello";
    CHECK(qt_value_eq(qt_string(s1), qt_string("hello")));
    CHECK(!qt_value_eq(qt_string("a"), qt_string("b")));

    /* arrays: reference identity */
    QtValue a1 = qt_array_new();
    QtValue a2 = qt_array_new();
    CHECK(qt_value_eq(a1, a1));
    CHECK(!qt_value_eq(a1, a2));

    /* errors: by name (IIsErr semantics) */
    CHECK(qt_value_eq(qt_error_new("E"), qt_error_new("E")));
    CHECK(!qt_value_eq(qt_error_new("E"), qt_error_new("F")));
}

int main(void) {
    test_scalars();
    test_float_format();
    test_render();
    test_apply_format();
    test_arrays();
    test_dicts();
    test_structs();
    test_errors();
    test_equality();

    if (tests_failed > 0) {
        fprintf(stderr, "%d/%d checks failed\n", tests_failed, tests_run);
        return 1;
    }
    printf("runtime: %d checks passed\n", tests_run);
    return 0;
}

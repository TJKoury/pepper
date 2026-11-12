/*
 * A test of PEPPER's C interface (include/pepper.h), without Python: run by
 * `meson test` (pixi run -e python test-capi).
 *
 * Instances and contexts, strings in and out, errors, results of any size,
 * a programmatic body from a C function, a prepared query.
 */
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "pepper.h"

static int failures = 0;

#define CHECK(cond, ...)                                    \
    do {                                                    \
        if (!(cond)) {                                      \
            printf("FAIL %s:%d: ", __FILE__, __LINE__);     \
            printf(__VA_ARGS__);                            \
            printf("\n");                                   \
            failures++;                                     \
        }                                                   \
    } while (0)

/* A leapseconds kernel (naif0012's values), as lines for lmpool. */
static const char *lsk[] = {
    "DELTET/DELTA_T_A = 32.184",
    "DELTET/K = 1.657D-3",
    "DELTET/EB = 1.671D-2",
    "DELTET/M = ( 6.239996D0 1.99096871D-7 )",
    "DELTET/DELTA_AT = ( 10, @1972-JAN-1 11, @1972-JUL-1 12, @1973-JAN-1",
    "13, @1974-JAN-1 14, @1975-JAN-1 15, @1976-JAN-1 16, @1977-JAN-1",
    "17, @1978-JAN-1 18, @1979-JAN-1 19, @1980-JAN-1 20, @1981-JUL-1",
    "21, @1982-JUL-1 22, @1983-JUL-1 23, @1985-JUL-1 24, @1988-JAN-1",
    "25, @1990-JAN-1 26, @1991-JAN-1 27, @1992-JUL-1 28, @1993-JUL-1",
    "29, @1994-JUL-1 30, @1996-JAN-1 31, @1997-JUL-1 32, @1999-JAN-1",
    "33, @2006-JAN-1 34, @2009-JAN-1 35, @2012-JUL-1 36, @2015-JUL-1",
    "37, @2017-JAN-1 )",
};

/* A body in linear motion relative to the Earth, in J2000. */
static int linear(void *ctx, void *user, int body, double et, double *state) {
    const double *v = (const double *)user;
    (void)ctx;
    (void)body;
    for (int i = 0; i < 3; i++) {
        state[i] = 7000.0 * (i == 0) + v[i] * et;
        state[i + 3] = v[i];
    }
    return 1;
}

int main(void) {
    char msg[1024];
    pepper_runtime_init();
    void *spice = pepper_spice_new();
    void *ctx = pepper_context_new(spice);

    /* the kernel pool from lines (fixed-width block) */
    enum { W = 80 };
    char block[sizeof(lsk) / sizeof(lsk[0])][W];
    int n = (int)(sizeof(lsk) / sizeof(lsk[0]));
    memset(block, 0, sizeof(block));
    for (int i = 0; i < n; i++) strncpy(block[i], lsk[i], W - 1);
    CHECK(pepper_spice_lmpool(spice, n, W, &block[0][0]) == 0, "lmpool");

    /* time conversion both ways */
    double et = 0.0;
    CHECK(pepper_str2et(ctx, "2026 NOV 8 12:00:00", &et) == 0, "str2et");
    char utc[64];
    CHECK(pepper_et2utc(ctx, et, "ISOC", 3, sizeof utc, utc) == 0, "et2utc");
    CHECK(strcmp(utc, "2026-11-08T12:00:00.000") == 0, "et2utc gave '%s'", utc);

    /* a result of any size: the values of a pool variable */
    const double *values = NULL;
    int nvalues = 0, found = 0;
    CHECK(pepper_pool_values(ctx, "DELTET/M", &values, &nvalues, &found) == 0, "pool_values");
    CHECK(found && nvalues == 2 && fabs(values[1] - 1.99096871e-7) < 1e-20, "pool_values: %d %d", found,
          nvalues);

    /* a JSON kernel (a file named *.json), loaded and unloaded */
    {
        const char *json = "test_capi_kernel.json";
        FILE *f = fopen(json, "w");
        fputs("// a JSON kernel\n{ \"CAPI_JSON\": [1.5, \"@2000-01-02T12:00\"], \"+CAPI_JSON\": 3, }\n", f);
        fclose(f);
        CHECK(pepper_spice_furnsh(spice, json, -1) == 0, "furnsh of a JSON kernel");
        CHECK(pepper_pool_values(ctx, "CAPI_JSON", &values, &nvalues, &found) == 0, "pool_values (JSON)");
        CHECK(found && nvalues == 3 && values[0] == 1.5 && values[1] == 86400.0 && values[2] == 3.0,
              "pool_values (JSON): %d %d", found, nvalues);
        CHECK(pepper_spice_unload(spice, json) == 0, "unload of a JSON kernel");
        CHECK(pepper_pool_values(ctx, "CAPI_JSON", &values, &nvalues, &found) == 0 && !found,
              "the JSON kernel's variable is gone");
        remove(json);
    }

    /* an error: a missing file, reported in the instance */
    CHECK(pepper_spice_furnsh(spice, "no-such-kernel.bsp", -1) == 1, "furnsh of a missing file");
    pepper_spice_getmsg(spice, "SHORT", sizeof msg, msg);
    CHECK(strcmp(msg, "SPICE(NOSUCHFILE)") == 0, "short message '%s'", msg);
    pepper_spice_reset(spice);
    CHECK(pepper_spice_failed(spice) == 0, "reset");

    /* an error in a context: unknown body */
    double state[6], lt;
    CHECK(pepper_spkezr(ctx, "NOBODY", et, "J2000", "NONE", "EARTH", state, &lt) == 1, "spkezr of an unknown body");
    pepper_getmsg(ctx, "SHORT", sizeof msg, msg);
    CHECK(strcmp(msg, "SPICE(IDCODENOTFOUND)") == 0, "short message '%s'", msg);
    pepper_reset(ctx);

    /* a programmatic body from a C function */
    double v[3] = {1.0, 2.0, 3.0};
    CHECK(pepper_spice_add_ephemeris(spice, -999, 399, "J2000", -1e10, 1e10, linear, v) == 0, "add_ephemeris");
    CHECK(pepper_spkezr(ctx, "-999", 100.0, "J2000", "NONE", "EARTH", state, &lt) == 0, "spkezr");
    CHECK(state[0] == 7100.0 && state[1] == 200.0 && state[2] == 300.0 && state[4] == 2.0, "state %g %g %g",
          state[0], state[1], state[2]);

    /* a second context: opens the files again on units of its own */
    void *ctx2 = pepper_context_new(spice);
    double state2[6];
    CHECK(pepper_spkezr(ctx2, "-999", 100.0, "J2000", "NONE", "EARTH", state2, &lt) == 0, "spkezr, context 2");
    CHECK(state2[0] == state[0], "context 2: %g", state2[0]);
    pepper_context_free(ctx2);

    /* a prepared query, two epochs */
    void *q = NULL;
    CHECK(pepper_prepare_state(ctx, "-999", "EARTH", "J2000", "NONE", &q) == 0 && q != NULL, "prepare_state");
    double ets[2] = {0.0, 10.0}, states[2][6], lts[2];
    CHECK(pepper_query_state(ctx, q, 2, ets, &states[0][0], lts) == 0, "query_state");
    CHECK(states[1][0] == 7010.0 && states[1][2] == 30.0, "query state %g %g", states[1][0], states[1][2]);
    pepper_state_query_free(q);

    /* a math routine without a context */
    double r[9];
    double axis[3] = {0.0, 0.0, 1.0};
    pepper_axisar(axis, M_PI / 2, r);   /* column-major */
    CHECK(fabs(r[1] - 1.0) < 1e-15 && fabs(r[3] + 1.0) < 1e-15, "axisar %g %g", r[1], r[3]);

    pepper_context_free(ctx);
    pepper_spice_free(spice);

    if (failures == 0) printf("test_capi: all tests passed\n");
    return failures != 0;
}

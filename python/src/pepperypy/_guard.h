/* PepperyPy: the in-use flag of a Context (plans/pepperypy_plan.md, "Contexts in
   Python"). A context may be entered again by the thread that is in it (a
   callback that makes queries), never by another thread at the same time. */
#ifndef PEPPERYPY_GUARD_H
#define PEPPERYPY_GUARD_H

#if defined(_MSC_VER)
#include <windows.h>
static inline unsigned long pp_load(volatile unsigned long *p) { return InterlockedCompareExchange((volatile LONG *)p, 0, 0); }
static inline int pp_cas(volatile unsigned long *p, unsigned long expected, unsigned long desired) {
    return InterlockedCompareExchange((volatile LONG *)p, (LONG)desired, (LONG)expected) == (LONG)expected;
}
static inline void pp_store(volatile unsigned long *p, unsigned long v) { InterlockedExchange((volatile LONG *)p, (LONG)v); }
#else
static inline unsigned long pp_load(volatile unsigned long *p) { return __atomic_load_n(p, __ATOMIC_ACQUIRE); }
static inline int pp_cas(volatile unsigned long *p, unsigned long expected, unsigned long desired) {
    return __atomic_compare_exchange_n(p, &expected, desired, 0, __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
}
static inline void pp_store(volatile unsigned long *p, unsigned long v) { __atomic_store_n(p, v, __ATOMIC_RELEASE); }
#endif

/* Enter: 0 on success, -1 if another thread is in the context. */
static inline int pp_enter(volatile unsigned long *owner, long *depth, unsigned long me) {
    if (pp_load(owner) == me) { (*depth)++; return 0; }
    if (pp_cas(owner, 0UL, me)) { *depth = 1; return 0; }
    return -1;
}

static inline void pp_leave(volatile unsigned long *owner, long *depth) {
    if (--(*depth) == 0) pp_store(owner, 0UL);
}

#endif

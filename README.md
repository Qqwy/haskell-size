# Size

## Flags

By default, checks for overflow/underflow will be inserted when doing addition/subtraction/multiplication (in the functions from the `Num` typeclass) when compiling _without optimizations enabled_.
When optimizations are enabled (to be precise: when rewrite rules are enabled), these checks are elided.

This way, you can get confidence your code works correctly during development and when running your test suite,
but do not pay for the performance overhead of the checks in production.

This can be configured to your liking using the `overflow-checks-always` resp. `overflow-checks-never` library flags.
For instance, you might want to compile your test suite with optimizations enabled but with the overflow checks too.

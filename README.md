# Size

An unsigned datatype which is well suited for sizes, lengths, offsets and indexes. 

This is a better type for these situations than `Int`, as it 'makes invalid states unrepresentable'.

The builtin `Int` and `Word` datatypes do wrapping arithmetic 'modulo 2^n'. This means that when overflow (or underflow) happens during addition, subtraction, multiplication, or conversion from another integral type, this happens implicitly and silently.
`Size`, on the other hand, does _checked_ arithmetic: on overflow (or underflow), the appropriate `ArithException` is raised.

## Basic Usage

It is recommended to import the `Size` type unqualified, and anything else qualified:
```
import Size (Size)
import qualified Size
```

`Size` implements most (if not all) typeclasses that other integral types like `Int`/`Word` also implement.
The most important ones of course being `Num`, `Eq` and `Ord`.

To convert `Size` into other integral types:
- Use `Size.toInt`/`Size.toWord`/`Size.toInteger`/`Size.toNatural` for total infallble conversions. One can also use, `fromIntegral` or `fromEnum` here but that is less explicit.
- For _fallible_ conversions, use `toEnum`/`fromInteger`/`fromIntegral` (part of the `Enum`, `Num` and `Integral` typeclasses) to convert other integral types to `Size`. An `Overflow :: ArithException` (or `Underflow :: ArithException`) will be thrown if the conversion cannot be done.
- For _safe_ conversions, use `Size.fromInt`/ `Size.fromWord`/ `Size.safeFromInteger`/ `Size.safeFromNatural`. These functions return a `Maybe Size`; `Nothing` will be returned if the conversion cannot be done.

## Flags

By default, checks for overflow/underflow will be inserted when doing addition/subtraction/multiplication (in the functions from the `Num` typeclass), and bitwise operations (in the functions from the `Bits` typeclass) when compiling _without optimizations enabled_.
When optimizations are enabled (to be precise: when rewrite rules are enabled), these checks are elided.

Besides arithmetic, the underflow check when converting an `Int` to a `Size` using `toEnum` will also elided. This is done to provide a straightforward way to extend the 'checked or not based on optimization level' to your own code that does other arithmetic-y things.

All other conversion functions are unaffected by optimizations or the explicit flags. They will _always_ check.

This way, you can get confidence your code works correctly during development and when running your test suite,
but do not pay for the performance overhead of the checks in production.

This can be configured to your liking using the `overflow-checks-always` resp. `overflow-checks-never` library flags.
For instance, you might want to compile your test suite with optimizations enabled but with the overflow checks too.

## FAQ (Frequently Asked Questions)

### Why use `Size` instead of `Int`?

Because in many situations, negative indexes make little to no sense. There are many functions in widely-used modules (such as `Data.List`, `Data.Bits` or `Data.Vector`) 
that take an `Int` as argument but say 'it should never be negative'. In some situations, passing a negative int performs results in a runtime error. (Often a stringly-typed `error` exception)
In others, behaviour is alltogether ill-defined or even undefined when negative numbers are passed.

These runtime checks, if any, are delayed until the point where the `Int` is being used. 
That makes it difficult to figure out how the invalid value came to be.

With `Size`, on the other hand, the checks happen on conversion and during arithmetic, making negativity checks when calling an indexing function superfluous.

#### But what if I _do_ want negative indexes?

These cases are rare. Therefore it is better to be explicit about them when they arise. 
For instance, compute on `Int`, then convert to a `Size` in a way that makes the most sense in your domain, and then index using the safe `Size`-based API.
Depending on your domain, this conversion `Int` -> `Size` might vary greatly:
- You might want to return a Maybe and be Nothing if the index is still negative (or similar with Either)
- You might want to throw a domain-specific exception
- You might want to index from the right, and do modulo-based indexing

There is no 'one size fits all' conversion here (pun not intended), so being explicit is better.

### Why use `Size` instead of `Word`?

Most builtin indexing functions, as well as those that can be found in libraries other than `base`, unfortunately use `Int` for indexing.
If you were to use `Word` to track an index and at the very last moment converted this value back to an `Int` using `fromIntegral`,
you would end up again with a negative number.

This is because the domain of `Word` is twice as large as those of `Int`, `Int` happens to be 2's complement,
and the conversion between the two keeps 'representation' rather than 'sign'.

Therefore, it already is a problem if you go above `maxBound :: Int` (2^31-1 resp. 2^63-1). And thus, a type separate from `Word` is needed.

On top of this, `Word` also does wrapping arithmetic, so any time you accidentally end up 'below zero' you will suddenly hold a very large number.

`Size` on the other hand can always be safely converted back to the nonnegative range of `Int`, and will never be too larger or negative because of its checked arithmetic.

### But some knowledgeable C/C++ people said that unsigned integral types where a mistake?

Correct. In those languages, implicit conversions between (number) types can happen almost anywhere, tacitly.
Most of those conversions also keep 'representation' rather than 'sign' (as well as truncating any bits that do not fit).
That makes any type in which data is more restricted more dangerous to use.

But Haskell is not C/C++. In Haskell, conversions have to be done explicitly.

Furthermore, in Haskell we have a strong value to 'make invalid states unrepresentable'.
That should also be the case for our numeric types.

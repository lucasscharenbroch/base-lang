# The "Base" Programming Language

This is a Haskell re-write of UW Madison's [CS536 (Compilers)](https://pages.cs.wisc.edu/~hasti/cs536/) semester-long project (Spring 2024).

The net line-count for the Haskell version is ~1000 LoC, which is less than **25%** of the Java version (~4,300 LoC).

See [the accompanying blog post](https://scharenbroch.dev/blog/rewriting-a-toy-compiler/) for more commentary on the low-level technical differences between the two.

## Differences From The Course Project

The re-write generally has the behavior as the Java version, but I made a few slight changes:

- Removed the second variant of single-line comments ("!!") (the [Parsec Token Library](https://hackage.haskell.org/package/parsec-3.1.17.0/docs/Text-Parsec-Token.html) makes doing this inconvenient)
- Added support for tuples in code generation
- String literals have slightly different rules
- Errors and error propigation has different behavior
    - The compiler gives up after a single error
    - Error messages and the places from which they are thrown are different

## Code Generation

The (MIPS) assembly generated by this compiler is aimed to run on [SPIM](https://en.wikipedia.org/wiki/SPIM), but it's very poorly tested and likely very buggy (to be honest, I didn't even try running it).
My primary goals in this re-write were to illustrate a different way to approach syntactically structuring a project likes this and to gain a deeper understanding of the core difficulties of writing a compiler, *not* to get a "working" or "efficient" final-product.
Of course, fewer bugs are better than more, so issues and PRs are greatly appreciated if you notice anything or want to get it running.

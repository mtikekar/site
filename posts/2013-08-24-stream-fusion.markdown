---
title: An example of fast numerical computation using Haskell
---

# Problem statement

Consider the sequence of natural numbers obtained from iterating 

---------- ---------------- ------------
a~n+1~ =   a~n~ / 2         a~n~ is even
           (3a~n~ + 1) / 2  else
---------- ---------------- ------------

starting with a given a~0~. This is a slightly optimized version of the
sequence in the [Collatz conjecture]. For example, with a~0~ = 12, the sequence
is [12, 6, 3, 5, 8, 4, 2, 1, 2, 1, ...]. The sequence is also called the
hailstone sequence and the mathematician Collatz conjectured that it reaches 1
for all values of a~0~. When it reaches 1, it is stuck in the cycle [1, 2, 1, 2
...], so we will truncate the sequence at 1.

In the current problem, we would like find that a~0~ (in some range of numbers)
which results in the longest hailstone sequence.

# C implementation

[collatz.c](src/collatz.c)

```{.c  include="src/collatz.c"}
```

On my machine (with gcc 4.7.3), it takes 0.4 seconds to find the
longest sequence for the first million numbers.

```
$ gcc -O2 collatz.c
$ time ./a.out 1000000
(329, 837799)

real	0m0.429s
user	0m0.424s
sys	0m0.004s
```

# Haskell implementation

The Haskell code is quite short and more importantly, it seems to express the
idea rather than give a particular implementation.

[collatz.hs](src/collatz.hs)

```{.haskell  include="src/collatz.hs"}
```

But, it takes an atrocious 6 seconds for the same computation (with ghc 7.6.2
and llvm 3.2)

```
$ ghc -O2 -fllvm collatz.hs
$ time ./collatz
(329,837799)

real	0m5.994s
user	0m5.944s
sys	0m0.040s
```

# Faster Haskell implementation

At this point, one should ideally run the Haskell program with profiling enabled
and see what's slowing it down. But for now, I'll venture a guess that it has
something to do with the multiple lists being created by `iterate` and
`takeWhile` which are finally reduced by `length`. To test this, let's write a
function `lenIterWhile` that combines the three into one without generating any
intermediate lists.

[collatz1.hs](src/collatz1.hs)

```{.haskell  include="src/collatz1.hs"}
```

This brings the time down to 0.54 seconds! This is quite close to the C speed
(0.43 s) but it comes at the cost of some code readability.

# Stream fusion

What we just did with `lenIterWhile` is called stream fusion and has been
implemented as a Haskell library that replaces the common list processing
functions in Prelude and Data.List. You can install the [stream fusion library]
with `cabal install stream-fusion`. Armed with this library, we can get rid of
the ugly `lenIterWhile` and write instead:

[collatz2.hs](src/collatz2.hs)

```{.haskell  include="src/collatz2.hs"}
```

Notice that we only had to modify our first Haskell code to use list functions
from Data.List.Stream in place of Prelude and Data.List. Also, at 0.51 seconds,
this is slightly faster than our previous code, possibly because the `map` and
`maximum` instances also got fused.

# Cython implementation

To compare with another high-level language:

[cycollatz.pyx](src/cycollatz.pyx)

```{.python  include="src/cycollatz.pyx"}
```

The code is more verbose and looks rather similar to the C code. But, at 0.47
seconds (with cython 0.17.4), it is faster than the Haskell code.

```
$ cython --embed cycollatz.pyx
$ gcc -O2 -I/usr/include/python2.7 cycollatz.c -lpython2.7 -o cycollatz
$ time ./cycollatz 1000000
(329, 837799)

real	0m0.470s
user	0m0.460s
sys	0m0.008s
```

# Conclusion

The different implementations are compared in the table below. 

Implementation                              Time    Readable
--------------------------------------     ------   --------
[C](src/collatz.c)                          0.43s      less
[Haskell (first pass)](src/collatz.hs)      5.99s      more
[Haskell (optimized)](src/collatz1.hs)      0.54s      less
[Haskell (stream fusion)](src/collatz2.hs)  0.51s      more
[Cython](src/cycollatz.pyx)                 0.47s      less

Readability of any piece of code is somewhat subjective, but I would say that
the fastest Haskell code is more readable than C or Cython. I would expect
Haskell to keep this advantage in larger projects because its lazy list
abstraction is useful and stream fusion makes the abstraction efficient.

There is a general conception that high-level languages are much slower than
low-level languages. This example shows that that isn't necessarily true. In
particular, when a language lets a programmer express their intent than give a
specific implementation, a good compiler should be able to optimize the code
very well. Thus, we can have the best of both worlds - small, maintainable code
that also runs fast.

In the current case, there is some more exploration to be done. Although the
Haskell code takes only 19% more time than C, it takes 5.7 times more memory and
causes 3.7 times more page faults (as reported by GNU time). Also, the
executable is 150 times bigger! It would be interesting to see what's causing
these and figure out if they can be reduced.

[Collatz conjecture]: http://en.wikipedia.org/wiki/Collatz_conjecture
[stream fusion library]: http://hackage.haskell.org/package/stream-fusion 

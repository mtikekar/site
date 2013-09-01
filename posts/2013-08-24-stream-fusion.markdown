---
title: An example of fast numerical computation using Haskell
---

Problem statement
=================

Consider the sequence of natural numbers obtained from iterating

$a_{n+1} = \begin{cases} a_n/2 & a_n \text{ is even} \\                          (3a_n + 1)/2 & \text{ else} \end{cases}$

starting with a given $a_0$. This is a slightly optimized version of the
sequence in the [Collatz
conjecture](http://en.wikipedia.org/wiki/Collatz_conjecture). For
example, with $a_0 =$ 12, the sequence is [12, 6, 3, 5, 8, 4, 2, 1, 2,
1, ...]. The sequence is also called the hailstone sequence and the
mathematician Collatz conjectured that it reaches 1 for all values of
$a_0$. When it reaches 1, it is stuck in the cycle [1, 2, 1, 2 ...], so
we will truncate the sequence at 1.

In the current problem, we would like find that $a_0$ (in some range of
numbers) which results in the longest hailstone sequence.

C implementation
================

[src](src/collatz.c)

~~~~ {.c .numberLines include="collatz.c"}
#include <stdio.h>

int main(int argc, char **argv) {
   int max_a0 = atoi(argv[1]); 
   int longest = 0, max_len = 0;
   int a0, len;
   unsigned long a;
   
   for (a0 = 1; a0 <= max_a0; a0++) {
      a = a0;
      len = 0;
      
      while (a != 1) {
         len++;
         a = ((a%2==0)? a : 3*a+1)/2;
      }
      
      if (len > max_len) {
         max_len = len;
         longest = a0;
      }
   }
   printf("(%d, %d)\n", max_len, longest);
   return 0;
}
~~~~

On my machine (with gcc 4.7.3), it takes 0.4 seconds to find the longest
sequence for the first million numbers.

~~~~ {.bash}
$ gcc -O2 collatz.c
$ time ./a.out 1000000
(329, 837799)

real	0m0.429s
user	0m0.424s
sys	0m0.004s
~~~~

Haskell implementation
======================

The Haskell code is quite short and more importantly, it seems to
express the idea rather than give a particular implementation.

[src](src/collatz.hs)

~~~~ {.haskell .numberLines include="collatz.hs"}
import Data.Word
import Data.List
import System.Environment

collatzNext :: Word32 -> Word32
collatzNext a = (if even a then a else 3*a+1) `div` 2

collatzLen :: Word32 -> Int
collatzLen a0 = length $ takeWhile (/= 1) $ iterate collatzNext a0

main = do
    [a0] <- getArgs
    let max_a0 = (read a0)::Word32
    print $ maximum $ map (\a0 -> (collatzLen a0, a0)) [1..max_a0]
~~~~

But, it takes an atrocious 6 seconds for the same computation (with ghc
7.6.2 and llvm 3.2)

~~~~ {.bash}
$ ghc -O2 -fllvm collatz.hs
$ time ./collatz
(329,837799)

real	0m5.994s
user	0m5.944s
sys	0m0.040s
~~~~

Faster Haskell implementation
=============================

At this point, one should ideally run the Haskell program with profiling
enabled and see what's slowing it down. But for now, I'll venture a
guess that it has something to do with the multiple lists being created
by `iterate` and `takeWhile` which are finally reduced by `length`. To
test this, let's write a function `lenIterWhile` that combines the three
into one without generating any intermediate lists.

[src](src/collatz1.hs)

~~~~ {.haskell .numberLines include="collatz1.hs"}
import Data.Word
import Data.List
import System.Environment

collatzNext :: Word32 -> Word32
collatzNext a = (if even a then a else 3*a+1) `div` 2

-- new code
collatzLen :: Word32 -> Int
collatzLen a0 = lenIterWhile collatzNext (/= 1) a0

lenIterWhile :: (a -> a) -> (a -> Bool) -> a -> Int
lenIterWhile next notDone start = len start 0 where
    len n m = if notDone n
                then len (next n) (m+1)
                else m
-- End of new code

main = do
    [a0] <- getArgs
    let max_a0 = (read a0)::Word32
    print $ maximum $ map (\a0 -> (collatzLen a0, a0)) [1..max_a0]
~~~~

This brings the time down to 0.54 seconds! This is quite close to the C
speed (0.43 s) but it comes at the cost of some code readability.

Stream fusion
=============

What we just did with `lenIterWhile` is called stream fusion and has
been implemented as a Haskell library that replaces the common list
processing functions in Prelude and Data.List. You can install the
[stream fusion
library](http://hackage.haskell.org/package/stream-fusion) with
`cabal install stream-fusion`. Armed with this library, we can get rid
of the ugly `lenIterWhile` and write instead:

[src](src/collatz2.hs)

~~~~ {.haskell .numberLines include="collatz2.hs"}
import Data.Word
import qualified Data.List.Stream as S
import System.Environment

collatzNext :: Word32 -> Word32
collatzNext a = (if even a then a else 3*a+1) `div` 2

collatzLen :: Word32 -> Int
collatzLen a0 = S.length $ S.takeWhile (/= 1) $ S.iterate collatzNext a0

main = do
    [a0] <- getArgs
    let max_a0 = (read a0)::Word32
    print $ S.maximum $ S.map (\a0 -> (collatzLen a0, a0)) [1..max_a0]
~~~~

Notice that we only had to modify our first Haskell code to use list
functions from Data.List.Stream in place of Prelude and Data.List. Also,
at 0.51 seconds, this is slightly faster than our previous code,
possibly because the `map` and `maximum` instances also got fused.

Cython implementation
=====================

To compare with another high-level language:

[src](src/cycollatz.pyx)

~~~~ {.python .numberLines include="cycollatz.pyx"}
import sys

cdef int collatzLen(int a0):
    cdef unsigned long a = a0
    cdef int length = 0
    while a != 1:
        a = (a if a%2 == 0 else 3*a+1) / 2
        length += 1
    return length

def maxLen(max_a0):
    cdef int max_length = 0, longest = 0, a0, length
    for a0 in xrange(1, max_a0 + 1):
        length = collatzLen(a0)
        if length > max_length:
            max_length = length
            longest = a0
    return max_length, longest

if __name__ == '__main__':
    max_a0 = int(sys.argv[1])
    print maxLen(max_a0)
~~~~

The code is more verbose and looks rather similar to the C code. But, at
0.47 seconds (with cython 0.17.4), it is faster than the Haskell code.

~~~~ {.bash}
$ cython --embed cycollatz.pyx
$ gcc -O2 -I/usr/include/python2.7 cycollatz.c -lpython2.7 -o cycollatz
$ time ./cycollatz 1000000
(329, 837799)

real	0m0.470s
user	0m0.460s
sys	0m0.008s
~~~~

Conclusion
==========

The different implementations are compared in the table below.

  Implementation                            Time   Readable
  ---------------------------------------- ------- ----------
  [C](collatz.c)                            0.43s  less
  [Haskell (first pass)](collatz.hs)        5.99s  more
  [Haskell (optimized)](collatz1.hs)        0.54s  less
  [Haskell (stream fusion)](collatz2.hs)    0.51s  more
  [Cython](cycollatz.pyx)                   0.47s  less

Readability of any piece of code is somewhat subjective, but I would say
that the fastest Haskell code is more readable than C or Cython. I would
expect Haskell to keep this advantage in larger projects because its
lazy list abstraction is useful and stream fusion makes the abstraction
efficient.

There is a general conception that high-level languages are much slower
than low-level languages. This example shows that that isn't necessarily
true. In particular, when a language lets a programmer express their
intent than give a specific implementation, a good compiler should be
able to optimize the code very well. Thus, we can have the best of both
worlds - small, maintainable code that also runs fast.

In the current case, there is some more exploration to be done. Although
the Haskell code takes only 19% more time than C, it takes 5.7 times
more memory and causes 3.7 times more page faults (as reported by GNU
time). Also, the executable is 150 times bigger! It would be interesting
to see what's causing these and figure out if they can be reduced.

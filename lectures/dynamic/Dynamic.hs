{-# LANGUAGE FlexibleContexts #-}
{-
---
fulltitle: Lazy Dynamic Programming 
date: April 16, 2024
---
-}

{- Side Note:

On May 3rd, PLUM will host a research open house, offering projects
for 499s in the fall. More details will follow!

-} 

module Dynamic where

import Test.QuickCheck

-- We are going to use Data.Array for this one
import Data.Array 

-- We will talk about these in the end!
import Control.Monad.Memo

{-

Immutable arrays in Haskell
---------------------------

The standard library provides the following type of immutable,
non-strict arrays:

  data Array i e

Since they are immutable, we care about two operations:
- reading from an array in constant time
- constructing arrays

To read from an array, we can use the following operator:

    (!) :: Ix i => Array i e -> i -> e

Here, Ix is a typeclass for indices, which maps a contiguous subrange
of values in a type onto integers.

   class (Ord a) => Ix a where
       -- | The list of values in the subrange defined by a bounding pair.
       range               :: (a,a) -> [a]
       -- | The position of a subscript in the subrange.
       index               :: (a,a) -> a -> Int
       -- | Returns 'True' if the given subscript lies in the range defined
       -- the bounding pair.
       inRange             :: (a,a) -> a -> Bool

To construct an array, it is common to use the `listArray`
combinator:

  listArray :: Ix i => (i, i) -> [e] -> Array i e

This combinator constructs an array from a pair of bounds and a list
of values in index order.

So why is this useful if we can't do mutation? Read on!

-}

{-

Playing with Array
------------------


Before we get to the interesting part, let's get more familiar with
these combinators, by rewriting `zip`. In your homework, you had to fix a
really ugly version of `zip` to follow the style guide. That version
was using inefficient list indexing. Let's do that with Arrays
instead:

-}

zip' xs ys = g 0 xs ys where
   g n xs ys = if n == length xs || n == length ys then [] else
           (xs !! n, ys !! n) : g (n + 1) xs ys























--zip' :: [a] -> [b] -> [(a,b)]
--zip' x y = go 0
--  where -- Convert x and y into arrays
--        xs = listArray (0,lx-1) x
--        ys = listArray (0,ly-1) y
-- 
--        -- Access the ith element of each array
--        go i | i < lx && i < ly = (xs ! i, ys ! i) : go (i+1)
--             | otherwise = []
-- 
--        (lx,ly) = (length x, length y)

{-

Not the prettiest of zips, but it works!

-}


{-

Dynamic Programming in Haskell
------------------------------

Dynamic programming is a powerful algorithmic technique that is
fundamentally based on the concept of _recursion_: the solution
to a problem is built from solutions to (potentially overlapping)
subproblems. Yet, most presentations of dynamic programming
use a highly imperative style of manipulating arrays. 

Dynamic programming in Haskell doesn't need to follow that style:
in this lecture we will build up to expressing such algorithms
in Haskell in an (arguably) much more natural style. Mutation
will still happen, but "under the hood", in a perfect example
of what laziness can bring to the table.

-}

{-

String Edit Distance
--------------------

The problem we are going to tackle is string edit distance.  If you're
not familiar with this problem, you can refresh your memory by looking
at the [wikipedia entry](https://en.wikipedia.org/wiki/Edit_distance).

Given two strings _s1_ and _s2_, find the minimum number of operations
that transform one into another, where the operations are:

- Inserting a character into a string
- Deleting a character from a string
- Substituting a character for another

So for example, to go from "kitten" to "sitting", we can do the following:
- kitten → sitten (substitute "s" for "k")
- sitten → sittin (substitute "i" for "e")
- sittin → sitting (insert "g" at the end)

The main algorithm is called the Wagner-Fischer Algorithm and it is
surprisingly simple: apply all three operations, and recursively
calculate the distance between the results of the operations and the
target string. Naturally, since some of the subproblems overlap, we
want to avoid recomputing them, or we will suffer from an exponential
running time. Still, the naive implementation is very simple to write
down, and can serve as a specification of correctness, so let's see
that!

-}

{-

A naive recursive solution:
---------------------------

-}

naive :: String -> String -> Int
naive _ _ = undefined

{-

This function, while straightforward and intuitive is quite slow as it
solves the same subproblems problems multiple times.

   naive "aaaaaaaaaa" "bbbbbbbbbb"

For example, if you `:set +s` in ghci and try two simple
inputs, they might take a while:

λ> naive "aaaaaaaaaa" "bbbbbbbbbb"
10
(4.82 secs, 4,081,352,312 bytes)

-}

{-

Recursion over indices
----------------------

To take a traditional dynamic programming approach, we make the
recursion through indices explicit. Let s1 and s2 be our two strings,
and let d i j be the distance between the suffixes of s1 and s2
starting of length i and j respectively.  Then the edit distance
between s1 and s2 is d (length s1) (length s2).

-}

naive' :: String -> String -> Int
naive' s1 s2 = d m n 
  where m = length s1
        n = length s2
        
        d :: Int -> Int -> Int
        -- Base Cases:
        d i 0 = i 
        d 0 j = j
        -- Recursion:
        d i j
          -- If the characters match, no operation
          | s1 !! (i-1) == s2 !! (j-1) = d (i-1) (j-1)
          -- Otherwise, choose the best of the three:
          | otherwise = minimum [ d (i-1) j + 1
                                , d i (j-1) + 1
                                , d (i-1) (j-1) + 1 ]


{-

This solution is even more naive:
- First, it uses indexing in lists, which is slow.
- Second, it's still solving the same problems multiple times!

For example, if you `:set +s` in ghci and try two simple
inputs, they might take a while:

λ> naive "aaaaaaaaaa" "bbbbbbbbbb"
10
(7.36 secs, 5,312,156,240 bytes)

Hurray, we made it even slower _and_ uglier!

In an imperative setting, this is the time where we would
transform `d` into a mutable 2D array, and we would need
to be careful to
- Initialize the array correctly.
- Always update the values for the solutions we compute
- Make sure we never read from parts of the array we
  haven't written to.

Instead, we're going to rely on Haskell's laziness to do all of that
bookkeeping for us! To do that, we're going to _define an array that
depends on itself_. The trick is to use indirection: every recursive
call in the function will index into the array and every array cell
will call function. That is, we are describing _what_ each value of
the array should be, but not how to compute it! 

-}

dynamic :: String -> String -> Int 
dynamic s1 s2 = d m n
  where m = length s1
        n = length s2

        d :: Int -> Int -> Int 
        d i 0 = i
        d 0 j = j
        d i j
          | s1 !! (i-1) ==  s2 !! (j-1) = ds ! (i-1, j-1)
          | otherwise = minimum [ ds ! (i-1, j) + 1
                                , ds ! (i, j-1) + 1
                                , ds ! (i-1, j-1) + 1
                                ]

        ds = listArray bounds [d i j | (i, j) <- range bounds]
        bounds = ((0, 0), (m, n))

{-

What did we do, concretely?

- Every `d x y` call in the definition of d becomes an index
  into the ds array.
- We're defining that the value of every element in the
  array is given by d.

...and we're done!

Wait, what?

-}

{-

Indeed, try it out!

λ> dynamic "aaaaaaaaaa" "bbbbbbbbbb"
10
(0.00 secs, 444,144 bytes)        

So what is going on?
- We ask for the value of `d (length s1) (length s2)`
- That looks up the value in the array. When the array
  is defined, the computation of that element is forced.
- That (potentially) triggers recursive calls, which
  further force values in the array.
- Every value that is computed is then stored for future accesses.

This way, we compute subsolutions at most once, exactly when we need
(some are not ever computed!); we access the array as if it was
completely filled out; and we never have to worry about what or when
we access it! 

-}

{-

Testing
-------

Is it correct? That's what QuickCheck is for!

Since we have two implementations, let's make sure they
are equivalent!

-}

prop_naive_dynamic' :: String -> String -> Bool
prop_naive_dynamic' s1 s2 = naive s1 s2 == dynamic s1 s2

{-

...and try quickChecking it. What's wrong? The naive algorithm
is _too_ naive to run on large inputs! So we need to generate
"small" strings. We are going to write a newtype for that
using the `vectorOf` combinator of quickChick, for producing
lists of a given length.

-}

newtype SmallString = SS {getString :: String}
  deriving (Eq, Show)

instance Arbitrary SmallString where
  arbitrary = do
    size <- choose (0,4)
    SS <$> vectorOf size arbitrary
  shrink (SS s) = [SS s' | s' <- shrink s]

prop_naive_dynamic :: SmallString -> SmallString -> Bool
prop_naive_dynamic (SS s1) (SS s2) = naive s1 s2 == dynamic s1 s2

{-

Hurray! It seems correct. Now, there are two questions remaining:
- Can we make it pretty again?
- Can we recover the "edit script"?

-}


{-

In class exercise:
------------------

Try to write the above using recursion strings
instead of indices. What goes wrong?

-}

{-

A more general way: Memoization Monad
-------------------------------------

The library monad-memo defines combinators that help with memoizing
recursive calls to functions. In particular it provides
the `memo` combinator with a strange type that
appears to do nothing:

    memo :: MonadMemo k v m => (k -> m v) -> k -> m v

It takes a monadic function from keys k to values v and
returns... something of the same type. The difference is that it
remembers mappings from keys to values for particular inputs. 

To see it in practice let's consider a more familiar example,
Fibonacci numbers:

-}

fib :: (Eq n, Num n) => n -> n
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-

Just like with the naive distance algorithm, this is straightforward,
but extremely slow.

λ> fib 20
6765
(0.04 secs, 7,677,192 bytes)
λ> fib 25
75025
(0.21 secs, 82,394,568 bytes)
λ> fib 30
832040
(1.80 secs, 911,022,016 bytes)

To use Memo monad we need to convert it into monadic form:

-}

fibm :: (Eq n, Num n, Monad m) => n -> m n
fibm 0 = return 0
fibm 1 = return 1
fibm n = do
  n1 <- fibm (n-1)
  n2 <- fibm (n-2)
  return (n1+n2)

{-

What is this? Well, we're only describing how to sequence the
different computations that need to happen. Unlike in
`fib` where the subcomputations for n-1 and n-2 can
be executed in parallel, we're explicitly ordering the
two computations.

But can we run this? Yes! Enter the Identity monad.

-}

{-

Identity Monad
--------------

The standard library defines the following newtype in
Data.Functor.Identity. It's a trivial type but will prove very useful
later on when we talk about Monad transformers. For now, let's follow
the types and give it Functor and Monad instances!

-}

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Monad Identity where
  return = Identity
  (Identity a) >>= k = k a

{-

So what does this do? It allows us to run `fibm`!

λ> runIdentity $ fibm 20
6765
(0.04 secs, 10,216,008 bytes)
λ> runIdentity $ fibm 25
75025
(0.27 secs, 110,557,608 bytes)
λ> runIdentity $ fibm 30
832040
(2.44 secs, 1,223,356,288 bytes)

Slightly slower than before since we're serializing
computations, but still very similar.

-}

{-

Using memo
----------

Now we can specify the computations we want to memoize using
`memo`. The type `Memo Int Int Int` says that:
- The function we're memoizing has Int as its argument
- The function we're memoizing has Int as its output
- The output of the computation is an Int

-}

fibM :: Int -> Memo Int Int Int 
fibM 0 = return 0
fibM 1 = return 1
fibM n = do
  n1 <- memo fibM (n-1)
  n2 <- memo fibM (n-2)
  return (n1+n2)

{-

All we did was add `memo` calls before the two recursive calls! And we can now run this with `startEvalMemo`.

λ> startEvalMemo $ fibM 30
832040
(0.01 secs, 356,360 bytes)
λ> startEvalMemo $ fibM 100
3736710778780434371
(0.00 secs, 570,104 bytes)

-}

{-

Back to distance
----------------

Now try to memoize the monadic distance function!

-}

memoized :: String -> String ->
  Memo (String, String) Int  Int
memoized [] s2 = return (length s2)
memoized s1 [] = return (length s1)
memoized (h1:t1) (h2:t2)
  | h1 == h2  = for2 memo memoized t1 t2
  | otherwise = do
    del <- for2 memo memoized t1 (h2:t2)
    ins <- for2 memo memoized (h1:t1) t2
    mod <- for2 memo memoized t1 t2
    return $ minimum [del, ins, mod] + 1

{-

Here, for2 is a useful combinator to avoid uncurrying multi-argument
functions!

λ> startEvalMemo (memoized "aaaaaaaaaa" "bbbbbbbbbb") 10 (0.01 secs,
772,568 bytes)

And finally, we can write a property to test this -
notice that we're using regular strings here!

-}

prop_dynamic_memoized :: String -> String -> Bool
prop_dynamic_memoized s1 s2 =
  dynamic s1 s2 == startEvalMemo (memoized s1 s2)

{-

Further practice:
-----------------

- Add functionality to calculate the edit script (the actions that
  need to be taken to transform a string to the other).
- Practice on other dynamic programming problems!

-}

{-

This is needed to satisfy a typeclass constraint that we'll talk about
in the near future. Ignore it for now!

-}

instance Applicative Identity where
  pure = return
  (<*>) = ap



{-

Material in this lecture is based on Tikhon Jelvis' [blog](https://jelv.is/blog/Lazy-Dynamic-Programming/)

-}

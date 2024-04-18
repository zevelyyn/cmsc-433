{-
---
fulltitle: "Type-directed Property Testing"
date: April 9, 2024
---
-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuickCheck where

{-
In this lecture, we will look at [QuickCheck][1], a technique that
cleverly exploits typeclasses and monads to deliver a powerful
automatic testing methodology.

Quickcheck was developed by [Koen Claessen][0] and [John Hughes][11]
more than ten years ago, and has since been ported to other languages
and is currently used, among other things to find subtle [concurrency
bugs][3] in [telecommunications code][4]. In 2010, it received the
[most influential paper award](http://www.sigplan.org/award-icfp.htm)
for the ICFP 2000 conference.

The key idea on which QuickCheck is founded is *property-based
testing*.  That is, instead of writing individual test cases (eg unit
tests corresponding to input-output pairs for particular functions)
one should write *properties* that are desired of the functions, and
then *automatically* generate *random* tests which can be run to
verify (or rather, falsify) the property.

By emphasizing the importance of specifications, QuickCheck yields
several benefits:

1. The developer is forced to think about what the code *should do*,

2. The tool finds corner-cases where the specification is violated,
   which leads to either the code or the specification getting fixed,

3. The specifications live on as rich, machine-checkable documentation
   about how the code should behave.

-}

import Control.Monad (liftM2, liftM3)
import qualified Data.List as List
import Test.HUnit ((~?=))
import qualified Test.HUnit
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    OrderedList (..),
    Property,
    Testable (..),
    choose,
    classify,
    elements,
    forAll,
    frequency,
    label,
    oneof,
    quickCheck,
    sample,
    sized,
    withMaxSuccess,
    (==>),
    collect
  )


{-

Properties
==========

A long time ago we created the horrifying truth tables for the monoid
instances of boolean conjunction and disjunction:

-}

newtype And = And {getAnd :: Bool} deriving (Eq, Show)

instance Semigroup And where
--  (<>) :: And -> And -> And
  x <> y = And $ getAnd x && getAnd y

instance Monoid And where
--  mempty :: And
  mempty = And True

monoidAnd :: Test.HUnit.Test
monoidAnd =
  Test.HUnit.TestList
    [ And False <> (And False <> And False) ~?= (And False <> And False) <> And False,
      And False <> (And False <> And True) ~?= (And False <> And False) <> And True,
      And False <> (And True <> And False) ~?= (And False <> And True) <> And False,
      And False <> (And True <> And True) ~?= (And False <> And True) <> And True,
      And True <> (And False <> And False) ~?= (And True <> And False) <> And False,
      And True <> (And False <> And True) ~?= (And True <> And False) <> And True,
      And True <> (And True <> And False) ~?= (And True <> And True) <> And False,
      And True <> (And True <> And True) ~?= (And True <> And True) <> And True,
      And True <> mempty ~?= And True,
      And False <> mempty ~?= And False,
      mempty <> And True ~?= And True,
      mempty <> And False ~?= And False
    ]

{-
But most those tests are instances of a more general property: associativity.

A QuickCheck property is essentially a function whose output is a
boolean. 
-}

prop_and_assoc :: Bool -> Bool -> Bool -> Bool
prop_and_assoc b1 b2 b3 =
  And b1 <> (And b2 <> And b3) == (And b1 <> And b2) <> And b3

{-
That is, a property looks a bit like a mathematical theorem that the
programmer believes is true. A QC convention is to use the prefix `"prop_"`
for QC properties. QC uses the types to generate random
inputs.

To *check* a property, we simply invoke the `quickCheck` action with the
property. Note that only certain types of properties can be tested, these
properties are all in the 'Testable' type class.

~~~~~{.haskell}
quickCheck :: (Testable prop) => prop -> IO ()
  	-- Defined in Test.QuickCheck.Test
~~~~~

`Bool -> Bool -> Bool -> Bool` is a Testable property, so
let's try quickCheck on our example property above. Note that because
`quickCheck` runs in the `IO` monad, you need to use `ghci` to see the
examples in this module if you're using VSCode. You can start ghci with the command:

          stack ghci QuickCheck.hs

Once you have done that, you should see a prompt that you can use to evaluate
 definitions in the `QuickCheck` module. Try running quickcheck with the
 property above.

~~~~~{.haskell}
*QuickCheck> quickCheck prop_and_assoc
~~~~~

you should see

      +++ OK, passed 100 tests.

That is, Haskell generated 100 test inputs and for all of those, the
property held. You can up the stakes a bit by changing the number of tests
you want to run
-}

quickCheckN :: (Testable prop) => Int -> prop -> IO ()
quickCheckN n = quickCheck . withMaxSuccess n

{-
and then ask quickcheck to run more tests.

~~~~~{.haskell}
*QuickCheck> quickCheckN 1000 prop_and_assoc
~~~~~

QuickCheck QuickSort
--------------------

Let's look at a slightly more interesting example. Here is the canonical
implementation of *quicksort* in Haskell. 
-}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = filter (< x) xs
    rhs = filter (> x) xs    

{-
Really doesn't need much explanation! Let's run it "by hand" on a few inputs

~~~~~{.haskell}
*QuickCheck> [10,9..1]

*QuickCheck> qsort [10,9..1]

*QuickCheck> [2,4..20] ++ [1,3..11]

*QuickCheck> qsort $ [2,4..20] ++ [1,3..11]

~~~~~

Looks good -- let's try to test that the output is in
fact sorted. We need a function that checks that a
list is ordered
-}

isOrdered :: Ord a => [a] -> Bool
isOrdered (x : y : zs) = x <= y && isOrdered (y : zs)
isOrdered [_] = True
isOrdered [] = True

{-
and then we can use the above to write a property saying that the
result of qsort is an ordered list.
-}

prop_qsort_isOrdered :: [Int] -> Bool
prop_qsort_isOrdered xs = isOrdered (qsort xs)

{-
Let's test it!

~~~~~{.haskell}
*QuickCheck> quickCheckN 1000 prop_qsort_isOrdered
~~~~~

Conditional Properties
----------------------

Here are several other properties that we
might want. First, repeated `qsorting` should not
change the list. That is,
-}

prop_qsort_idemp :: [Int] -> Bool
prop_qsort_idemp xs = qsort (qsort xs) == qsort xs

{-
Second, the head of the result is the minimum element
of the input
-}

prop_qsort_min :: [Int] -> Bool
prop_qsort_min xs = head (qsort xs) == minimum xs

{-
~~~~~{.haskell}
*QuickCheck> quickCheck prop_qsort_min
~~~~~

However, when we run this, we run into a glitch.

But of course! The earlier properties held *for all inputs*
while this property makes no sense if the input list is empty!
This is why thinking about specifications and properties has the
benefit of clarifying the *preconditions* under which a given
piece of code is supposed to work.

In this case we want a *conditional properties* where we only want
the output to satisfy to satisfy the spec *if* the input meets the
precondition that it is non-empty.
-}

prop_qsort_nn_min :: [Int] -> Property
prop_qsort_nn_min xs =
  not (null xs) ==> head (qsort xs) == minimum xs

{-
We can write a similar property for the maximum element too.
-}

prop_qsort_nn_max :: [Int] -> Property
prop_qsort_nn_max xs =
  undefined

{-
~~~~~{.haskell}
*QuickCheck> quickCheckN 100 prop_qsort_nn_min

*QuickCheck> quickCheckN 100 prop_qsort_nn_max
~~~~~

This time around, both the properties hold.

Note that now, instead of just being a `Bool` the output
of the function is a `Property` a special type built into
the QC library. Similarly the *implies* combinator `==>`
is one of many QC combinators that allow the construction
of rich properties.

Testing Against a Model Implementation
--------------------------------------

We could keep writing different properties that capture
various aspects of the desired functionality of `qsort`.
Another approach for validation is to test that our `qsort`
is *behaviorally* identical to a trusted *reference
implementation* which itself may be too inefficient or
otherwise unsuitable for deployment. In this case, let's
use the standard library's `sort` function
-}

prop_qsort_sort :: [Int] -> Bool
prop_qsort_sort xs = qsort xs == List.sort xs

{-
which we can put to the test

~~~~~{.haskell}
*QuickCheck> quickCheckN 1000 prop_qsort_sort
~~~~~

Say, what?!

~~~~~{.haskell}
*QuickCheck> qsort [-1,-1]
~~~~~

Ugh! So close, and yet ... Can you spot the bug in our code?

~~~~~{.haskell}
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = filter (< x) xs
    ghs = filter (> x) xs
~~~~~

We're assuming that the *only* occurrence of (the value) `x`
is itself! That is, if there are any *copies* of `x` in the
tail, they will not appear in either `lhs` or `rhs` and hence
they get thrown out of the output.

Is this a bug in the code? What *is* a bug anyway? Perhaps the
fact that all duplicates are eliminated is a *feature*! At any
rate there is an inconsistency between our mental model of how
the code *should* behave as articulated in `prop_qsort_sort`
and the actual behavior of the code itself.

We can rectify matters by stipulating that the `qsort` produces
lists of distinct elements
-}

isDistinct :: Eq a => [a] -> Bool
isDistinct l = List.nub l == l 

prop_qsort_distinct :: [Int] -> Bool
prop_qsort_distinct = isDistinct . qsort

{-
and then, weakening the equivalence to only hold on inputs that
are duplicate-free
-}

prop_qsort_distinct_sort :: [Int] -> Property
prop_qsort_distinct_sort xs =
  isDistinct xs ==> qsort xs == List.sort xs

{-
QuickCheck happily checks the modified properties

~~~~~{.haskell}
*QuickCheck> quickCheck prop_qsort_distinct

*QuickCheck> quickCheck prop_qsort_distinct_sort

~~~~~

The Perils of Conditional Testing
---------------------------------

Well, we managed to *fix* the `qsort` property, but beware! Adding
preconditions leads one down a slippery slope. In fact, if we paid
closer attention to the above runs, we would notice something

~~~~~{.haskell}
*QuickCheck> quickCheckN 10000 prop_qsort_distinct_sort
...
(5012 tests; 248 discarded)
...
+++ OK, passed 10000 tests.
~~~~~

The bit about some tests being *discarded* is ominous. In effect,
when the property is constructed with the `==>` combinator, QC
discards the randomly generated tests on which the precondition
is false. In the above case QC grinds away on the remainder until
it can meet its target of `10000` valid tests. This is because
the probability of a randomly generated list meeting the precondition
(having distinct elements) is high enough. This may not always be the case.

To see why, let's look at another sorting function.

The following code is (a simplified version of) the `insert` function from the
standard library

-}

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

{-
Given an element `x` and a list `xs`, the function walks along `xs`
till it finds the first element greater than `x` and it places `x`
to the left of that element. Thus

~~~~~{.haskell}
*QuickCheck> insert 8 ([1..3] ++ [10..13])
~~~~~

Indeed, the following is the well known [insertion-sort][5] algorithm
-}

isort :: Ord a => [a] -> [a]
isort = foldr List.insert []

{-
We could write our own tests, but why do something a machine can do better?!
-}

prop_isort_sort :: [Int] -> Bool
prop_isort_sort xs = isort xs == List.sort xs

{-
~~~~~{.haskell}
*QuickCheck> quickCheckN 1000 prop_isort_sort
~~~~~

Now, the reason that the above works is that the `insert`
routine *preserves* sorted-ness. That is, while of course
the property
-}

prop_insert_ordered' :: Int -> [Int] -> Bool
prop_insert_ordered' x xs = isOrdered (insert x xs)

{-
is bogus,

~~~~~{.haskell}
*QuickCheck> quickCheckN 1000 prop_insert_ordered'

*QuickCheck> insert 0 [0, -1]
~~~~~

the output *is* ordered if the input was ordered to begin with
-}

prop_insert_ordered :: Int -> [Int] -> Property
prop_insert_ordered x xs =
  isOrdered xs ==> isOrdered (insert x xs)

{-
Notice that now, the precondition is more *complex* -- the property
requires that the input list be ordered. If we QC the property

~~~~~{.haskell}
*QuickCheck> quickCheck prop_insert_ordered

~~~~~

*Aside* the above example also illustrates the benefit of
writing the property as `p ==> q` instead of using the boolean
operator `||` to write `not p || q`. In the latter case, there is
a flat predicate, and QC doesn't know what the precondition is,
so a property may hold *vacuously*. For example consider the
variant
-}

prop_insert_ordered_vacuous :: Int -> [Int] -> Bool
prop_insert_ordered_vacuous x xs =
  not (isOrdered xs) || isOrdered (insert x xs)

{-
QC will happily check it for us

~~~~~{.haskell}
*QuickCheck> quickCheckN 1000 prop_insert_ordered_vacuous
~~~~~

Unfortunately, in the above, the tests passed *vacuously*
only because their inputs were *not* ordered, and one
should use `==>` to avoid the false sense of security
delivered by vacuity.

QC provides us with some combinators for guarding against
vacuity by allowing us to investigate the *distribution*
of test cases

~~~~~{.haskell}
label    :: String -> Property -> Property
classify :: Bool -> String -> Property -> Property
~~~~~

We may use these to write a property that looks like
-}

prop_insert_ordered_vacuous' :: Int -> [Int] -> Property
prop_insert_ordered_vacuous' x xs =
  label lbl $
    not (isOrdered xs) || isOrdered (insert x xs)
  where
    lbl =
      (if isOrdered xs then "Ordered, " else "Not Ordered, ")
        ++ show (length xs)

{-
When we run this, we get a detailed breakdown of the 100 passing tests:

~~~~~{.haskell}
*QuickCheck> quickCheck prop_insert_ordered_vacuous'
~~~~~

where in the first four lines, `P% COND, N` means that `P` percent of the
ordered inputs had length `N`, and satisfied the predicate denoted by the
string `COND`.

What percentage of lists were ordered? How long were they?  

Generating Data
===============

Before we start discussing how QC generates data (and how we can help it
generate data meeting some pre-conditions), we must ask ourselves a basic
question: how does QC behave *randomly* in the first place?!

~~~~~{.haskell}
*QuickCheck> quickCheck prop_insert_ordered'

*QuickCheck> quickCheck prop_insert_ordered'

~~~~~

Eh? This seems most *impure* -- same inputs yielding two totally different
outputs! How does that happen?

The QC library defines a type

   Gen a

of "generators for values of type a".

The impurity of random generation is bottled up inside the 'Gen' type. The
**monad** structure of this type let's us work with this impurity in a
controlled way, but we will get to that. For now, note that we don't get a
value of type 'a', we will do our work with these generators. If you have
a generator, you can see what it produces with the `sample` operation:

~~~~~{.haskell}
sample :: Show a => Gen a -> IO ()
~~~~~

This operation generates some example values and prints them to stdout.

Generator Combinators
---------------------

QC comes loaded with a set of combinators that allow us to create
generators for various data structures.

The first of these combinators is `choose`

~~~~~{.haskell}
choose :: (System.Random.Random a) => (a, a) -> Gen a
~~~~~

which takes an *interval* and returns an random element from that interval.
(The typeclass `System.Random.Random` describes types which can be
*sampled*. For example, the following is a randomly chosen set of numbers
between `0` and `3`.

~~~~~{.haskell}
*QuickCheck> sample $ choose (0, 3)
~~~~~

A second useful combinator is `elements`

~~~~~{.haskell}
elements :: [a] -> Gen a
~~~~~

which returns a generator that produces values drawn from the input list

~~~~~{.haskell}
*QuickCheck> sample $ elements [10, 20..100]
~~~~~

A third combinator is `oneof`

~~~~~{.haskell}
oneof :: [Gen a] -> Gen a
~~~~~

which allows us to randomly choose between multiple generators

~~~~~{.haskell}
*QuickCheck> sample $ oneof [elements [10,20,30], choose (0,3)]
~~~~~

and finally, the above is generalized into the `frequency` combinator

~~~~~{.haskell}
frequency :: [(Int, Gen a)] -> Gen a
~~~~~

which allows us to build weighted combinations of individual generators.

~~~~~{.haskell}
*QuickCheck> sample $ frequency [(1, elements [10,20]), (5, elements [11,21])]
~~~~~

The Generator Monad
-------------------

The parameterized type 'Gen' is an instance of the monad type class. 

~~~~~~~{.haskell}
-- from the class Monad
--
return :: a -> Gen a
(>>=)  :: Gen a -> (a -> Gen b) -> Gen b
~~~~~~~


We will cover what it exactly means for `Gen` to be a monad in a future
lecture. However, as we will see, these operations let us put generators
together compositionally.
-}

genThree :: Gen Int -- a generator that always generates the value '3'
genThree = undefined

-- A generator for pairs
genPair :: Gen a -> Gen b -> Gen (a, b)
genPair g1 g2 = undefined

{- More generator combinators:

The next three are from the library [Control.Monad](http://hackage.haskell.org/package/base-4.13.0.0/docs/Control-Monad.html).
These are defined in terms of return and (>>=) above, so they
are available for any type constructor that is an instance of
the Monad class, including Gen.

~~~~~~~{.haskell}
liftM  :: (a -> b) -> Gen a -> Gen b
liftM2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
liftM3 :: (a -> b -> c -> d) -> Gen a -> Gen b -> Gen c -> Gen d
~~~~~~~

The `lift` in these names comes from an analogy: we are taking normal functions and "lifting" them to work with generators. For example, `liftM` takes any regular function of type `a -> b` and converts it
to be a function of type `Gen a -> Gen b`.

Note, `liftM` above has another name---`fmap`.
And the infix operator `<$>` is yet another name for `fmap`,
and you'll probably see this one the most frequently.

-}

-- Implement using liftM variants:
genPair' :: Gen a -> Gen b -> Gen (a,b)
genPair' = undefined 

{-
Generator Practice
------------------

Use the operators above to define generators. Make sure that you test them out
with `sample` to make sure that they are what you want.
-}

genBool :: Gen Bool
genBool = undefined

genTriple :: Gen a -> Gen b -> Gen c -> Gen (a, b, c)
genTriple = undefined

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe ga = undefined                    

{-
The Arbitrary Typeclass
-----------------------

To keep track of all these generators, QC defines a typeclass containing types
for which random values can be generated!

~~~~~{.haskell}
class Arbitrary a where
  arbitrary :: Gen a
~~~~~

Thus, to have QC work with (ie generate random tests for) values of type
`a` we need only make `a` an instance of `Arbitrary` by defining an
appropriate `arbitrary` function for it. QC defines instances for base
types like `Int` , `Float`, etc

~~~~~{.haskell}
*QuickCheck> sample (arbitrary :: Gen Int)
~~~~~

and lifts them to compound types.

~~~~~{.haskell}
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a,b,c) where
  arbitrary = liftM3 (,,) arbitrary arbitrary arbitrary
~~~~~

~~~~~{.haskell}
*QuickCheck> sample (arbitrary :: Gen (Int,Float,Bool))
~~~~~

~~~~~{.haskell}
*QuickCheck> sample (arbitrary :: Gen [Int])
~~~~~

However, you'll need to make your own instances of `Arbitrary` for user
 defined datatypes. As we'll discuss below, there are two many options in
 generation for GHC to make this class automatically derivable. Below, we will
 walk through constructing a good generator for the list type as an example of
 constructing a good generator for an arbitrary datatype. You might challenge
 yourself to write a generator for a `Tree` type.

Generating Ordered Lists
------------------------

We can use the above combinators to write generators for lists
-}

genList1 :: (Arbitrary a) => Gen [a]
genList1 = liftM2 (:) arbitrary genList1

{-
~~~~~{.haskell}
*QuickCheck> sample (genList1 :: Gen [Int])
~~~~~

Can you spot a problem in the above?

<FILL IN HERE>

Let's try again,
-- oneOf :: [Gen a] -> Gen a
-}

genList2 :: (Arbitrary a) => Gen [a]
genList2 = undefined


{-
~~~~~{.haskell}
*QuickCheck> sample (genList2 :: Gen [Int])
~~~~~

This is not bad, but there is still something undesirable.
What is wrong with this output?

<FILL IN HERE>

This version fixes the problem. We only choose `[]` one eighth of the time.
-}

genList3 :: (Arbitrary a) => Gen [a]
genList3 =
  frequency
    [ (1, return []),
      (7, liftM2 (:) arbitrary genList3)
    ]

{-
~~~~~{.haskell}
*QuickCheck> sample (genList3 :: Gen [Int])
~~~~~

However, `genList3` has the opposite problem --- it generates a lot of long
lists (longer than length 2 or 3) but not so many short ones. But finding bugs
with shorter lists is a lot faster than finding bugs with long lists.

So, two last tweaks. We let quickcheck determine what frequency to use, and we
decrease the frequency of cons with each recursive call.  For the former, we
rely on the following function from QC library.

         sized :: (Int -> Gen a) -> Gen a

This function is higher-order; it takes a generator with a size parameter
 (i.e. the Int) and uses it to develop a new generator by progressively
 increasing this size.

For the latter, when we define this "size-aware" function, we cut the size in
half for each recursive call.
-}

genList4 :: forall a. (Arbitrary a) => Gen [a]
genList4 = sized gen
  where
    gen :: Int -> Gen [a]
    gen n =
      frequency
        [ (1, return []),
          (n, liftM2 (:) arbitrary (gen (n `div` 2)))
        ]

{-
(Above: the `ScopedTypeVariables` extension lets us mention the type variable
 `a` in the type of the local definition `gen`. We can't write this type
 without this extension and without introducing `a` with the keyword `forall`.)

Now look at that distribution! Not too small, not too big, not too many nulls.

~~~~~{.haskell}
*QuickCheck> sample (genList4 :: Gen [Int])
~~~~~

I encourage you to look at the implementation of `genList4` closely. This use
of `frequency` and `sized` is particularly important to controlling the
generation of tree-structured data.

For practice, see if you can generate arbitrary trees using the pattern shown
above in `genList4`.

-}

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = undefined

{-:
~~~~~{.haskell}
*QuickCheck> sample (arbitrary :: Gen (Tree Int))
~~~~~

Generating data that satisfies properties
-----------------------------------------

We can use the above to build a custom generator that always returns *ordered
lists* by mapping the `sort` function over the generated list.
-}

genOrdList :: (Arbitrary a, Ord a) => Gen [a]
genOrdList = List.sort <$> genList3

{-
~~~~~{.haskell}
*QuickCheck> sample (genOrdList :: Gen [Int])
~~~~~

NOTE: Above, just saying `sort genList3` doesn't work. We have that `genList3`
is a generator for lists, not a list itself. Because `Gen` is a functor, the
right way to compose generation with a transformation is to use `fmap`.

To *check* the output of a custom generator we can use the `forAll` combinator

~~~~~{.haskell}
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
~~~~~

For example, we can check that in fact, the combinator only produces
ordered lists

~~~~~
*QuickCheck> quickCheck $ forAll genOrdList isOrdered
~~~~~

and now, we can properly test the `insert` property
-}

prop_insert :: Int -> Property
prop_insert x = forAll genOrdList $ \xs ->
  isOrdered xs && isOrdered (insert x xs)

{-
~~~~~
*QuickCheck> quickCheck prop_insert
~~~~~

Using `newtype` for smarter test-case generation
------------------------------------------------

This works very well, but we might not want to write `forAll genOrdList`
everywhere we want to test a property on ordered lists only.  In order to get
around that, we can define a new type that *wraps* lists, but has a different
`Arbitrary` instance:
-}

newtype OrdList a = OrdList [a] deriving (Eq, Ord, Show, Read)

instance (Ord a, Arbitrary a) => Arbitrary (OrdList a) where
  arbitrary = fmap OrdList genOrdList

{-
This says that to generate an arbitrary `OrdList`, we use the `genOrdList`
generator we just defined, and package that up.

~~~~~{.haskell}
*QuickCheck> sample (arbitrary :: Gen (OrdList Int))
~~~~~

Now, we can rewrite our `prop_insert` function more simply:
-}

prop_insert' :: Int -> OrdList Int -> Bool
prop_insert' x (OrdList xs) = isOrdered $ insert x xs

{-
And in fact, QuickCheck already has this type built in:
-}

prop_insert'' :: Int -> OrderedList Int -> Bool
prop_insert'' x (Ordered xs) = isOrdered $ insert x xs

{-
This technique of using `newtype`s for special-purpose instances is very
common, both in QuickCheck and in other Haskell libraries.

QuickCheck outside of Haskell
-----------------------------

As a testing tool, QuickCheck has been ported to many languages, some of which are
listed on its wikipedia page [13]. Haskell's type classes (and monads) mean that
the implementation of QuickCheck in Haskell is surprisingly simple.

As an aside:
------------

The majority of my research is in property-based testing! Lately I've been thinking a lot
about properties with preconditions like `prop_insert_ordered`---it's great that
QuickCheck gives us the tools to build generators that satisfy preconditions,
but property-based testing would be much easier if QuickCheck could do some of
that work automatically.

If anyone wants to hear more about that, stop by my office hours or set up a
meeting!

-------------------------------------------------------------------------

Credit: This lecture based on [12].

[0]: http://www.cse.chalmers.se/~koen/
[1]: http://www.cse.chalmers.se/~rjmh/QuickCheck/
[2]: http://www.cs.york.ac.uk/fp/smallcheck/
[3]: http://video.google.com/videoplay?docid=4655369445141008672#
[4]: http://www.erlang-factory.com/upload/presentations/55/TestingErlangProgrammesforMulticore.pdf
[5]: http://en.wikipedia.org/wiki/Insertion_sort
[6]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/src/Test-QuickCheck-Gen.html#Gen
[7]: http://book.realworldhaskell.org/read/monads.html
[8]: http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
[11]: http://www.cse.chalmers.se/~rjmh
[12]: http://cseweb.ucsd.edu/classes/wi11/cse230/lectures/quickcheck.lhs
[13]: https://en.wikipedia.org/wiki/QuickCheck
-}

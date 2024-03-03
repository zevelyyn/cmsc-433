{-
---
fulltitle: "In class exercise: Semigroup, Monoid and Foldable"
date: February 29, 2022
---
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module MonoidFoldable where

import qualified Data.List as List
import Test.HUnit
import Prelude hiding (all, and, any, or)

import Data.Semigroup  -- also re-exports much of Data.Monoid

{-

Semigroups and Monoids
----------------------

The Semigroup and Monoid typeclasses may be the first examples of
something you've seen which does not easily align to overloading
interfaces defined in your (previous) favorite programming
language. 

The Semigroup type class defines what it means to "combine structures
together". In particular, it includes the following binary operator,
which must be defined when making a type an instance of this class.

(<>) :: a -> a -> a 

This operation can be any binary operation on the type, with one
important caveat: it must be ASSOCIATIVE. In other words, it shouldn't
matter in your code if you type a <> (b <> c) or (a <> b) <> c. Either
expression should produce the same result.

You've probably seen some examples of types with associative
operations before. For example, list concatenation is associative, so
the standard library includes the following instance that says that
when we are using lists as semigroups, we should interpret (<>) to
mean (++).

instance Semigroup [a] where
   (<>) = (++)

Because this instance is in the standard library, you don't actually need to use the specialized (++) for lists; you can use the more general (<>) whenever you please (as long as it is scope, and Haskell can tell you are using lists).

The Monoid typeclass extends Semigroup class with a designated value,
called mempty. A type must first be declared to be an instance of the
Semigroup class before it can be declared to be a Monoid.

mempty  :: Monoid a => a

The intended meaning is that mempty is some value of the type such
that when <>ed to anything, it does nothing. In other words, for any
type that has an associative binary operation (<>) with an identity
element (mempty) is a Monoid.

Now, that's all very abstract! Let's look at some instances. We can
extend our list Semigroup by adding an identity element for list
concatenation.

instance Monoid [a] where
  mempty  = []

In fact, lists are the canonical example of a Monoid -- they can be
combined together with (++), and the empty list, when combined with
any other list via (++), gives that other list as a result.

Furthermore, the test case below demonstrates that lists satisfy the
required properties of monoids: the empty list is a left and right
identity for append, and concatenation is an associative operation.

-}

testListMonoid :: Test
testListMonoid =
  let t1 , t2 , t3 :: [Int]
      t1 = [1,2] 
      t2 = [3,4] 
      t3 = [1,2,3,4] in
  TestList [ mempty <> t1     ~?= t1,              -- left identity
             t1 <> mempty     ~?= t1,              -- right identity
             (t1 <> t2) <> t3 ~?= t1 <> (t2 <> t3) -- associativity
            ]

{-

What else? Another example that may jump to mind is numbers. Any
integer can be added to any other, with zero being the identity
element. So you might expect that the standard library would have an
instance like this:

instance Semigroup Integer where
  (<>) = (+)

instance Monoid Integer where
  mempty  = 0

But it does not. After all, you could just as well realize that
integers can be combined by multiplying them, with one being the
identity element! In that case, we'd write an instance like this:

instance Semigroup Integer where
  (<>) = (*)

instance Monoid Integer where
  mempty  = 1

Who's to say which monoidal interpretation of integers is "more
right"?

In cases like this, we usually use a newtype to differentiate between
which interpretation we want to use. That is, we can instead say:

newtype Sum     a = Sum     { getSum     :: a }
newtype Product a = Product { getProduct :: a }

instance Num a => Semigroup (Sum a) where
  x <> y = Sum $ getSum x + getSum y
instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  
instance Num a => Semigroup (Product a) where
  x <> y = Product $ getProduct x * getProduct y
instance Num a => Monoid (Product a) where
  mempty = Product 1
  

Notice that in the above, these instances require a Num instance for
the types they're wrapping. Num, as you have seen in class, is a
typeclass which provides overloading for numeric operations and
literals -- so using it as a superclass of our instance allows us to
be generic over what kind of numbers we're manipulating, rather than
fixing a particular type of number.

For example, we can calculate the sum and product of a list of
integers by coercing the elements to type Sum Int and Product Int
respectively.

-}

foldList :: Monoid b => [b] -> b
foldList = List.foldr (<>) mempty

-- (<>) :: Semigroup a => a -> a -> a
-- mempty :: Monoid a => A

ten :: Int
-- ten = getSum (foldList (map Sum [1,2,3,4]))
ten = getSum (foldList (Sum <$> [1,2,3,4]))

-- sum = foldr (+) 0
-- product = foldr (*) 1

twentyfour :: Int
twentyfour = getProduct (foldList (map Product [1,2,3,4]))

{-

Or, for example, because the `String` type is an instance of this class
(using `++` for `mappend`) we can `foldList` a list of `String`s to
a single string.

-}

tm0 :: Test
tm0 = foldList ["C", "M", "S", "C", "4", "8", "8"] ~?= "CMSC488"

{-
The assignment shows you that numbers can instantiate this class in multiple
ways.  Like numbers, `Booleans` can be made an instance of the `Monoid` class
in two different ways.
-}

newtype And = And {getAnd :: Bool} deriving (Eq, Show)

newtype Or = Or {getOr :: Bool} deriving (Eq, Show)

{-
Make sure that you understand these type definitions. We are defining a type
`And` with single data constructor (also called `And`). The argument of this
data constructor is a record with a single field, called `getAnd`. What this
means is that `And` and `getAnd` allow us to convert `Bool`s to `And` and
back.

      λ> :t And
      And :: Bool -> And
      λ> :t getAnd
      getAnd :: And -> Bool

Above, `newtype` is like data, but is restricted to a single variant. It is
typically used to create a new name for an existing type. This new name allows
us to have multiple instances for the same type (as below) or to provide type
abstraction (like `SortedList` in the HW).

Your job is to complete these instances that can tell us whether any of the
booleans in a list are true, or whether all of the booleans in a list are
true. (See two test cases below for an example of the behavior.)
-}

anyT1 :: Test
anyT1 = getOr (foldList (fmap Or [True, False, True])) ~?= True

allT2 :: Test
allT2 = getAnd (foldList (fmap And [True, False, True])) ~?= False

instance Semigroup And where
  (<>) = undefined

instance Monoid And where
  mempty = undefined

instance Semigroup Or where
  (<>) = undefined

instance Monoid Or where
  mempty = undefined

{-
Because `And` and `Or` wrap boolean values, we can be sure that our instances
have the right properties by testing the truth tables.  (There are more
concise to write these tests, but we haven't covered them yet.)
-}

monoidAnd :: Test
monoidAnd =
  TestList
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

monoidOr :: Test
monoidOr =
  TestList
    [ Or False <> (Or False <> Or False) ~?= (Or False <> Or False) <> Or False,
      Or False <> (Or False <> Or True) ~?= (Or False <> Or False) <> Or True,
      Or False <> (Or True <> Or False) ~?= (Or False <> Or True) <> Or False,
      Or False <> (Or True <> Or True) ~?= (Or False <> Or True) <> Or True,
      Or True <> (Or False <> Or False) ~?= (Or True <> Or False) <> Or False,
      Or True <> (Or False <> Or True) ~?= (Or True <> Or False) <> Or True,
      Or True <> (Or True <> Or False) ~?= (Or True <> Or True) <> Or False,
      Or True <> (Or True <> Or True) ~?= (Or True <> Or True) <> Or True,
      Or True <> mempty ~?= Or True,
      Or False <> mempty ~?= Or False,
      mempty <> Or True ~?= Or True,
      mempty <> Or False ~?= Or False
    ]

{-

Foldable
--------

The Foldable Typeclass

At this point, I'd like to point something out: foldMapList can itself
be even further generalized. We already know that lists are not the
only data structures which support folding -- we've seen folds for
trees of various kinds and for other data structures as well. As a
result, it makes sense to allow some kind of foldMap operation for
those structures also. In the standard library, we therefore have:

foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

That is to say, foldMap is a method of yet another type class,
Foldable, of which the type constructor [] is an
instance. Implementing this interface roughly corresponds to saying,
"this data structure contains some elements, and I know how to do a
fold across them." To implement the Foldable class for some type, we
just need to implement foldMap.

Implement the Functor and Foldable instances for the Crispy
datatype. Remember to keep in mind a guiding principle: when you are
confused, don't think about what things are supposed to mean; just
follow the types and see where they take you.

-}

data Crispy a = Snap a [a] a
               | Crackle [[Crispy a]]
               | Pop Integer deriving (Eq,Show)

instance Functor Crispy where
  fmap = undefined

instance Foldable Crispy where
  foldMap = undefined

testCrispy :: Test
testCrispy =
  let c1, c2, c3, c5 :: Crispy Integer
      c1 = fmap (+1) (Snap 0 [1,2,3] 4)
      c2 = Snap 700 [] 600
      c3 = Pop 1234567890
      c5 = fmap (subtract 1) (Crackle [[c1, c2], [c1, c3]]) in
  TestList [ 15 ~?= getSum (foldMap Sum c1)
           , 1 ~?= getProduct (foldMap Product c3)
           , "0123469959901234" ~?= foldMap show c5]

-------------------------------------------------------------

{-

We can use your Monoid instances for `Or` and `And` to generalize
operations to any data structure.

For example, we can generalize the `and` operation to any Foldable data
structure using `foldMap`.
-}

and :: Foldable t => t Bool -> Bool
and = getAnd . foldMap And

{-
Define these three related operations:
-}

or :: Foldable t => t Bool -> Bool
or = undefined

all :: Foldable t => (a -> Bool) -> t a -> Bool
all f = undefined

any :: Foldable t => (a -> Bool) -> t a -> Bool
any f = undefined

{-
so that the following tests pass
-}

tf0 :: Test
tf0 = or [True, False, False, True] ~?= True

tf1 :: Test
tf1 = all (> 0) [1 :: Int, 2, 4, 18] ~?= True

tf2 :: Test
tf2 = all (> 0) [1 :: Int, -2, 4, 18] ~?= False

tf3 :: Test
tf3 = any (> 0) [1 :: Int, 2, 4, 18] ~?= True

tf4 :: Test
tf4 = any (> 0) [-1 :: Int, -2, -4, -18] ~?= False


{-

Application
-----------

Recall our familiar `Tree` type. Haskell can derive the `Functor` instance for this type so we ask it to do so.
-}

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Functor)

{-
And here is an example `Tree`.
-}

t1 :: Tree String
t1 = Branch "d" (Branch "b" (l "a") (l "c")) (Branch "f" (l "e") (l "g"))
  where
    l x = Branch x Empty Empty

{-
We *could* make this type an instance of `Foldable` using the definition of
`foldrTree` from the TreeFolds module.

But, for practice, complete the instance using `foldMap`.
-}

instance Foldable Tree where
  foldMap = undefined

{-
With this instance, we can for example, verify that all of the sample strings
above have length 1.
-}

tt1 :: Test
tt1 = all ((== 1) . length) t1 ~?= True

{-
Oblig-main
----------
-}

main :: IO ()
main = do
  _ <- runTestTT $ TestList [tm0, anyT1, allT2, monoidAnd, monoidOr, tf0, tf1, tf2, tf3, tf4, tt1]
  return ()

{-
---
fulltitle: "In class exercise: foldr"
date: February 26, 2024
---

In HigherOrder.hs we saw a few functions that you could write using the
general purpose 'foldr' function. This function captures the general pattern of
list recursion and is also good practice for working with higher-order functions.

This set of exercises is intended to give you more practice with using 'foldr'
with lists. You should start with one at the right level for you and your
partner (they are ordered in terms of difficulty).
-}

module Foldr where

import Test.HUnit
import Prelude hiding (all, filter, foldl, foldl1, last, length, map, reverse)

{-
Length Example
--------------

This function counts the number of elements stored in a list. The recursive
definition of the length function is
-}

-- >>> length1 "abc"
-- 3
length1 :: [a] -> Int
length1 [] = 0
length1 (_ : xs) = 1 + length1 xs

{-
Now we can rewrite it in terms of foldr.
-}

-- >>> length "abc"
-- 3
-- >>> length ""
-- 0
length :: [a] -> Int
-- length = foldr (\_ n -> 1 + n) 0
length = foldr (const (+1)) 0

{-
and test it on some inputs
-}

testLength :: Test
testLength =
  "length"
    ~: TestList
      [ length "abcd" ~?= 4,
        length "" ~?= 0
      ]

{-
Once we have completed the foldr version, we can trace through its evaluation using the
argument `['a','b','c']`.

The evaluation starts out as:

      foldr (\_ n -> 1 + n) 0 ['a', 'b', 'c']
         = (_ n -> 1 + n) 'a' (foldr (\_ n -> 1 + n) 0 ['b', 'c'])
         = 1 + (foldr (\_ n -> 1 + n) 0 ['b', 'c'])

By unfolding the definition of foldr one step as above we can kind of see what
it's doing. It's just adding 1 each time we encounter an element. The first
argument in the anonymous function is just ignored, because you don't care
what the elements of the list are when you're just counting them. The n is the
accumulator, representing the rest of the fold (which in this case is the
length of the tail). 1 + length of tail gives you the length of your list.

Unfolding this further we can see how the whole thing would evaluate:

         foldr (\_ n -> 1 + n) 0 ['a', 'b', 'c']
          = 1 + (foldr (\_ n -> 1 + n) 0 ['b', 'c'])
          = 1 + (1 + (foldr (\_ n -> 1 + n) 0 ['c']))
                 -- Skipping to when the anonymous function is applied :)
          = 1 + (1 + (1 + (foldr (\_ n -> 1 + n) 0 [])))
          = 1 + (1 + (1 + 0))
          = 1 + (1 + 1)
          = 1 + 2
          = 3

Note, this evaluation can also be generated by the [online tool](http://bm380.user.srcf.net/cgi-bin/stepeval.cgi?expr=foldr+%28%5C_+n+-%3E+1+%2B+n%29+0+%5B%27a%27%2C+%27b%27%2C+%27c%27%5D).

Feel free to use this tool below, but understand that it is fairly
simplistic. It won't be able to handle all of the examples that you throw at
it.

All
---

Calculate whether *all* elements of a list satisfy a given predicate. The recursive definition is
-}

all1 :: (a -> Bool) -> [a] -> Bool
all1 _ [] = True
all1 p (x : xs) = p x && all1 p xs

{-
Now implement using foldr

-}

-- >>> all (>10) ([1 .. 20] :: [Int])
-- False
-- >>> all (>0) ([1 .. 20] :: [Int])
-- True
all :: (a -> Bool) -> [a] -> Bool
-- all p = foldr (\a b -> p a && b) True
all p = foldr (\a -> (p a &&)) True
-- all p = foldr ((&&) . p) True

testAll :: Test
testAll =
  "all"
    ~: TestList
      [ all (> 10) ([1 .. 20] :: [Int]) ~?= False,
        all (> 0) ([1 .. 20] :: [Int]) ~?= True
      ]

{-

Last
----

Find and return the last element of the lists (if the list is nonempty).

The recursive definition is
-}

last1 :: [a] -> Maybe a
last1 [] = Nothing
last1 (x : xs) = case xs of
  [] -> Just x
  _ -> last1 xs

{-
Now implement using foldr
-}

-- >>> last "abcd"
-- Just 'd'
-- >>> last ""
-- Nothing
last :: [a] -> Maybe a
last = foldr (\a b -> case b of Nothing -> Just a | Just x -> Just x) Nothing

{-
>
-}

testLast :: Test
testLast =
  "last"
    ~: TestList
      [ last "abcd" ~?= Just 'd',
        last "" ~?= Nothing
      ]

{-
and trace through the evaluation `last [1,2]`

Filter
-----

The filter function selects items from a list that satisfy a given
predicate. The output list should contain only the elements
of the first list for which the input function returns `True`.
-}

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\a b -> if p a then a:b else b)

testFilter :: Test
testFilter =
  "filter"
    ~: TestList
      [ filter (> 10) [1 .. 20] ~?= ([11 .. 20] :: [Int]),
        filter (\l -> sum l <= 42) [[10, 20], [50, 50], [1 .. 5]] ~?= ([[10, 20], [1 .. 5]] :: [[Int]])
      ]

{-
Try this on `filter (>2) [2,3]`

Reverse
-------

Reverse the elements appearing in the list.

Consider this linear time version that used direct recursion.
-}

reverse1 :: [a] -> [a]
reverse1 l = aux l []
  where
    aux [] = id
    aux (x : xs) = \ys -> aux xs (x : ys)

{-
Now rewrite this function using 'foldr'
-}

reverse :: [a] -> [a]
reverse l = undefined

testReverse :: Test
testReverse =
  "reverse"
    ~: TestList
      [ reverse "abcd" ~?= "dcba",
        reverse "" ~?= ""
      ]

{-
And trace through its evaluation on the list `['a','b','c']`:

Intersperse
-----------

The intersperse function takes an element and a list
and `intersperses' that element between the elements of the list.
For example,
-}

-- >>> intersperse ',' "abcde"
-- "a,b,c,d,e"

{-
The recursive version looks like this:
-}

intersperse1 :: a -> [a] -> [a]
intersperse1 _ [] = []
intersperse1 a (x : xs) = case xs of
  [] -> [x]
  _ -> x : a : intersperse1 a xs

{-
Now rewrite using 'foldr'
-}

intersperse :: a -> [a] -> [a]
intersperse = undefined

testIntersperse :: Test
testIntersperse =
  "intersperse"
    ~: TestList
      [ "intersperse0" ~: intersperse ',' "abcde" ~=? "a,b,c,d,e",
        "intersperse1" ~: intersperse ',' "" ~=? ""
      ]

{-
and trace through an example of `intersperse ',' "ab"`

foldl
-----

Here is the usual recursive definition for "fold left".
-}

foldl1 :: (b -> a -> b) -> b -> [a] -> b
foldl1 _ z [] = z
foldl1 f z (x : xs) = foldl1 f (z `f` x) xs

{-
You can see that `foldl1` is different than `foldr` by comparing the results on various examples:

      Foldr*> foldl1 (flip (:)) [] [1,2,3]
      [3,2,1]
      Foldr*> foldr  (:) [] [1,2,3]
      [1,2,3]

      Foldr*> foldl1 (++) "x" ["1","2","3"]
      "x123"
      Foldr*> foldr (++) "x" ["1","2","3"]
      "123x"

But, you can also define `foldl` in terms of `foldr`. Give it a try.
-}

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z xs = undefined

testFoldl :: Test
testFoldl = foldl (++) "x" ["1", "2", "3"] ~=? "x123"

{-
And trace through the test case above.

Test runner
-----------
-}

runTests :: IO ()
runTests = do
  _ <-
    runTestTT $
      TestList
        [ testLength,
          testAll,
          testLast,
          testFilter,
          testReverse,
          testIntersperse,
          testFoldl
        ]
  return ()
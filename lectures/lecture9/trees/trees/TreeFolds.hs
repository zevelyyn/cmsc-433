{-
---
fulltitle: "Extra practice: Tree folds"
date: February 26, 2024
---
-}

module TreeFolds where

{-
>
-}

import qualified Data.DList as DL
import Test.HUnit

{-
This exercise is about efficiently iterating over tree-structured data.
Recall the basic type of binary trees.
-}

-- | a basic tree data structure
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

{-
And also the `infixOrder` function from the Datatypes module.
-}

infixOrder :: Tree a -> [a]
infixOrder Empty = []
infixOrder (Branch x l r) = infixOrder l ++ [x] ++ infixOrder r

{-
For example, using this tree

              5
            /   \
           2     9
          / \     \
         1   4     7
-}

exTree :: Tree Int
exTree =
  Branch
    5
    (Branch 2 (Branch 1 Empty Empty) (Branch 4 Empty Empty))
    (Branch 9 Empty (Branch 7 Empty Empty))

{-
the infix order traversal produces this result.
-}

testInfixOrder :: Test
testInfixOrder = "infixOrder" ~: infixOrder exTree ~?= [1, 2, 4, 5, 9, 7]

{-
However, if you did the DList exercise, the (++) in the definition of
 `infixOrder` should bother you. What if the tree is terribly right-skewed?

                 1
                /
               2
              /
             3
            /
           4
          /
         5
        /
      ...
-}

-- | A big "right-skewed" tree
bigRightTree :: Int -> Tree Int
bigRightTree m = go 0
  where
    go n = if n <= m then Branch n (go (n + 1)) Empty else Empty

-- | A big "left-skewed" tree
bigLeftTree :: Int -> Tree Int
bigLeftTree m = go 0
  where
    go n = if n <= m then Branch n Empty (go (n + 1)) else Empty

{-
If you turn on benchmarking, you can observe the difference between a left
 skewed and right skewed tree in ghci.  At this scale, the time taken to print
 these trees dominates the computation, but take a look at the difference in
 allocation!

        λ> sum (infixOrder (bigRightTree 10000))
        50005000
        (0.93 secs, 4,305,878,096 bytes)
        λ> sum (infixOrder (bigLeftTree 10000))
        50005000
        (0.02 secs, 6,625,936 bytes)

We can improve things by using DLists while traversing the tree. Try to
 complete this version so that the number of bytes used for traversing t1 and
 t2 is more similar to the version above...
-}

infixOrder1 :: Tree a -> [a]
infixOrder1 t = DL.toList (go t)
  where go :: Tree a -> DL.DList a
        go Empty = DL.empty
        go (Branch x l r) =
          DL.append (go l) $ DL.append (DL.singleton x) $ go r

tinfixOrder1 :: Test
tinfixOrder1 = "infixOrder1a" ~: infixOrder1 exTree ~?= [1, 2, 4, 5, 9, 7]

{-
       λ> sum (infixOrder1 (bigRightTree 10000))
       50005000
       (0.02 secs, 8,546,256 bytes)
       λ> sum (infixOrder1 (bigLeftTree 10000))
       50005000
       (0.02 secs, 8,546,256 bytes)

Now, let's inline the DList definitions and get rid of the uses of `(.)` and `id`.
-}

infixOrder2 :: Tree Int -> [Int]
infixOrder2 = undefined

{-
Foldable Trees
--------------

Does this idea generalize to forms of tree recursion? You betcha.

Let's generalize the "base case" and "inductive step" of the definition above, separating
the recursion from the specific operation of traversal. First, we identify these operators
inside the definition of infixOrder.
-}

infixOrder3 :: Tree Int -> [Int]
infixOrder3 = undefined

{-
Then we abstract them from the definition.
-}

foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree f b Empty = b
foldrTree f b (Branch x l r) =
  let br = foldrTree f b r in  
  foldrTree f (f x br) l

{-
>
-}

infixOrder4 :: Tree a -> [a]
infixOrder4 = foldrTree (:) []
           -- foldrTree (\a b -> [a] ++ b) []

sizeTree :: Tree Int -> Int
sizeTree = foldrTree (const (1 +)) 0
        -- foldrTree (\_ b -> 1 + b) 0

sumTree :: Tree Int -> Int
sumTree = foldrTree (+) 0

anyTree :: (a -> Bool) -> Tree a -> Bool
anyTree f = foldrTree (\x b -> f x || b) False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree f = foldrTree (\x b -> f x && b) True

{-
Now use `foldrTree` as an inspiration to define a `foldlTree` function, which
folds over the tree in the opposite order.
-}

foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree f b Empty = b
foldlTree f b (Branch x l r) =
  foldlTree f (f (foldlTree f b l) x) r

revOrder :: Tree a -> [a]
revOrder = foldlTree (flip (:)) []

trevOrder :: Test
trevOrder = "revOrder" ~: revOrder exTree ~?= [7, 9, 5, 4, 2, 1]

{-
Note: this tree fold is not the same as a more general `fold`-like operation
 for trees that captures the general principle of tree recursion.
-}

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ e Empty = e
foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)

{-
Challenge!

Define `foldrTree` and `foldlTree` in terms of `foldTree`.
(This is challenging!)

-}

foldrTree' :: (a -> b -> b) -> b -> Tree a -> b
-- foldrTree' f acc t = foldTree aux id t acc
foldrTree' f acc t = foldTree (\a bl br -> bl . f a . br) id t acc
-- acc :: b (acc is type b), id :: b -> b
  aux :: a -> (b -> b) -> (b -> b) -> (b -> b)
  aux a bl br = bl (f a (br b))

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)

tfoldrTree' :: Test
tfoldrTree' = "foldrTree'" ~: foldrTree' (+) 0 tree1 ~?= 6

foldlTree' :: (b -> a -> b) -> b -> Tree a -> b
foldlTree' = undefined

tfoldlTree' :: Test
tfoldlTree' = "foldlTree'" ~: foldlTree' (+) 0 tree1 ~?= 6

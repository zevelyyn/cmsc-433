module Printing where

import Data.List(intersperse)
import Text.PrettyPrint ( (<+>), Doc )
import qualified Text.PrettyPrint as PP
import qualified Data.Char as Char

{- | Pretty Printing |
   ===================

Recall our favorite datatype, binary trees:

-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Eq, Show)

ex :: Tree Int
ex = Branch 10 (Branch 5 (Branch 0 Empty Empty) (Branch 7 (Branch 6 Empty Empty) Empty)) (Branch 42 (Branch 17 Empty Empty) (Branch 100 Empty Empty))


{- |
Derived `Show` instances for datatypes can be pretty hard to
read, especially when the structures get long. Really, who wants to read this...

> ex
> Branch 10 (Branch 5 (Branch 0 Empty Empty) (Branch 7 (Branch 6 Empty Empty) Empty)) (Branch 42 (Branch 17 Empty Empty) (Branch 100 Empty Empty))

instead of this...

    ghci> putStrLn (pretty ex)
    10 - 5 - 0 - x
               - x
           - 7 - 6 - x
                   - x
               - x
       - 42 - 17 - x
                 - x
            - 100 - x
                  - x

A *pretty printer* is a function that formats an abstract syntax tree into a
readable representation of the concrete syntax. 

The `pretty` library, imported above as `PP`, provides the following to assist
in the development of pretty printers:

   * An abstract type `Doc` of "pretty documents" that know how to lay
     themselves out prettily. We can use this type to define a class of of types
     that support pretty printing---those that define a function mapping any
     value of that type to a suitable `Doc`.
-} 

class PP a where
  pp :: a -> Doc

{- |

   * Operations for rendering, or converting a `Doc` to text at the
     top level.  The rendering functions are parameterized over display
     options, such as the maximum line length, so that they can figure out
     how to best display the text. 
-}

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: PP a => a -> String
pretty = PP.render . pp

-- | Compact version. Displays its argument without newlines.
oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode=PP.OneLineMode}) . pp

{- |
   * Primitive documents and operations for constructing `Doc`s from primitive
     types, such as characters, ints and string.

     For example, we can use the `int` function to define the `Int` instance of the `PP`
     class and the `text` function to define the `String` one.
-}

instance PP Int where
  pp = PP.int

instance PP String where
  pp = PP.text


{- |
   * Combinators for combining `Doc`s in various ways, providing constraints on
     the textual layout. For example, some are listed below. (See the library
     documentation for *many* more.)

          -- An empty document, with no width and no height.
          empty :: Doc

          -- Beside. Combines two documents horizontally with no space between.
          (<>) :: Doc -> Doc -> Doc

          -- Beside, separated by space, unless one of the arguments is `empty`.
          (<+>) :: Doc -> Doc -> Doc

          -- Nest (or indent) a document by a given number of positions
          -- (which may also be negative).
          nest :: Int -> Doc -> Doc

          -- Above. Combines two documents vertically (with overlap if
          -- possible: if the last line of the first argument stops at
          -- least one position before the first line of the second begins,
          -- these two lines are overlapped).
          ($$) :: Doc -> Doc -> Doc

          -- List version of $$.
          vcat :: [Doc] -> Doc

          -- wrap document in (..)
          parens :: Doc -> Doc

          -- wrap document in [..]
          braces :: Doc -> Doc
-}

{- | Pretty-Printer implementation |
   ---------------------------------

So let's write a pretty printing instance for Trees! 

-}

instance (Show a, PP a) => PP (Tree a) where
  pp Empty = PP.char 'x'
  pp (Branch n l r) =
    pp n <+> PP.nest (length $ show n) (PP.vcat [ PP.char '-' <+> pp l
                                                , PP.char '-' <+> pp r ])

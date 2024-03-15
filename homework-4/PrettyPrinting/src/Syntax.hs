{- | Mini Dafny - Syntax |
   -----------------------

This module defines data structures to represent the syntax of the "miniDafny" programming language.
You should read this file carefully to understand the miniDafny language, but you do not need to
edit this file.

This module contains:

1. The definitions of the datatypes that represent the abstract syntax of miniDafny
2. Sample programs written in miniDafny
3. A *pretty-printer* that displays these datatypes as concise text
-}

module Syntax where

import Data.List(intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint ( (<+>), Doc )
import qualified Text.PrettyPrint as PP
import Test.QuickCheck(Arbitrary(..),Gen)
import qualified Test.QuickCheck as QC
import Control.Monad(mapM_)
import qualified Data.Char as Char

import Test.HUnit
import qualified Data.Bool as PP
import qualified Control.Exception as PP
import qualified Control.Exception as PP

{- |
What is a miniDafny Program?
=====================

The general idea is that miniDafny is a very, very cut down version of the Dafny
verification language. 

A program is a single Method with a name, a list of input variables,
a list of output variables, a sequence of requires/ensures/modifies
statements, followed by a main body.
-}

data Method = Method Name [Binding] [Binding] [Specification] Block
  deriving (Eq, Show)

-- | A Name is just a type synonym for a String:

type Name = String            -- either the name of a variable or the name of a method

-- | A Binding is a Name together with its Type:

type Binding = (Name, Type)

-- | For simplicity, types in miniDafny can be integers, booleans, or arrays of integers.

data Type = TInt | TBool | TArrayInt
  deriving (Eq, Show)

-- | Specifications are logical statements that describe program behaviors.
-- | They can be requires, ensures or modifies statements.

data Specification =
    Requires Predicate
  | Ensures  Predicate
  | Modifies Name
  deriving (Eq, Show)

-- | A Predicate is a forall-quantified boolean formula, potentially with a precondition:

data Predicate = Predicate [Binding] Expression
  deriving (Eq, Show)

-- | Programs are sequences of statements in a block:

newtype Block = Block [ Statement ]                 -- s1 ... sn 
  deriving (Eq, Show)

-- | Statements themselves have the following forms:

data Statement =
    Decl Binding Expression            -- var x : int := e;
  | New  Binding Expression            -- var x : array<int> := new int[e];> 
  | Assert Predicate                   -- (assert FOO)
  | Assign Var Expression              -- x := e
  | If Expression Block Block          -- if e { s1 } else { s2 }
  | While [Predicate] Expression Block -- while e [invariant P] { s }
  | Empty                              -- 
  deriving (Eq, Show)

-- | Expressions are variables, literal constants, or operators applied
-- | to sub-expressions:

data Expression =
    Var Var                            -- global variables x and table indexing
  | Val Value                          -- literal values
  | Op1 Uop Expression                 -- unary operators
  | Op2 Expression Bop Expression      -- binary operators
  deriving (Eq, Show)

{- | The literal values include ints, booleans, and a special value for
     arrays that should not appear directly in source programs, but is
     used by the interpreter.
-}

data Value =
    IntVal Int         -- 1
  | BoolVal Bool       -- false, true
  | ArrayVal [Int]
  deriving (Eq, Show, Ord)

-- | Unary operators are single argument functions: arithmetic negation, logical not, and a
-- | length operation for arrays.

data Uop =
    Neg   -- `-` :: Int -> Int
  | Not   -- `not` :: a -> Bool
  | Len   -- `.Length` :: Array -> Int
  deriving (Eq, Show, Enum, Bounded)

-- | Binary operators are two-argument functions: arithmetic operators that work with int values only, overloaded
-- | equality and ordering operations that work for anything, and string concatenation.

data Bop =
    Plus     -- `+`  :: Int -> Int -> Int
  | Minus    -- `-`  :: Int -> Int -> Int
  | Times    -- `*`  :: Int -> Int -> Int
  | Divide   -- `/`  :: Int -> Int -> Int   -- floor division
  | Modulo   -- `%`  :: Int -> Int -> Int   -- modulo
  | Eq       -- `==` :: a -> a -> Bool
  | Neq      -- `!=` :: a -> a -> Bool> 
  | Gt       -- `>`  :: a -> a -> Bool
  | Ge       -- `>=` :: a -> a -> Bool
  | Lt       -- `<`  :: a -> a -> Bool
  | Le       -- `<=` :: a -> a -> Bool
  | Conj     -- `&&` :: Bool -> Bool -> Bool
  | Disj     -- `||` :: Bool -> Bool -> Bool
  | Implies  -- `==>` :: Bool -> Bool -> Bool
  | Iff      -- `<->` :: Bool -> Bool -> Bool
  deriving (Eq, Show, Enum, Bounded)

{- | Variables and Arrays |
   ------------------------

Variables are places that store values. 

Arrays of integers are a primitive part of the language. They are data
structure that can be accessed by looking up the value associated
with an integer key, in a variable expression

    t[1]    -- any integer value can be used as a key

or modified by introducing a new value associated with a key, using an
assignment statement:

    t[1] = 3      -- any integer value can be stored in a table

We represent these globals and table fields, using the
following datatype definitions.

-}

data Var =
    Name Name                  -- x, global variable
  | Proj Expression Expression -- t[1], access table table using an integer
  deriving (Eq, Show)

{- | Test Programs |
   =================

Below are some test programs that you can use in this assignment. These programs can also be
found in the corresponding files in the `dafny` folder. Please take a look at these files to 
familiarize yourself with the concrete syntax of MiniDafny

-}

var :: String -> Expression
var = Var . Name

-- abs.dfy
wAbs = Method "Abs" [("r",TInt)] [("absR",TInt)] []
              (Block [If (Op2 (Var (Name "r")) Lt (Val (IntVal 0)))
                         (Block [Assign (Name "absR") (Op1 Neg (Var (Name "r"))),Empty])
                         (Block [Assign (Name "absR") (Var (Name "r")),Empty])])
-- findMinVal.dfy
wMinVal = Method "FindMinVal" [("a",TArrayInt)] [("min",TInt)] [Requires (Predicate [] (Op2 (Op1 Len (Var (Name "a"))) Gt (Val (IntVal 0)))),Ensures (Predicate [("i",TInt)] (Op2 (Op2 (Op2 (Op2 (Val (IntVal 0)) Le (Op2 (Var (Name "i")) Conj (Var (Name "i")))) Lt (Op1 Len (Var (Name "a")))) Implies (Var (Name "min"))) Le (Var (Proj (Var (Name "a")) (Var (Name "i"))))))] (Block [Assign (Name "min") (Var (Proj (Var (Name "a")) (Val (IntVal 0)))),Empty,Decl ("i",TInt) (Val (IntVal 1)),Empty,While [] (Op2 (Var (Name "i")) Lt (Op1 Len (Var (Name "a")))) (Block [If (Op2 (Var (Proj (Var (Name "a")) (Var (Name "i")))) Lt (Var (Name "min"))) (Block [Assign (Name "min") (Var (Proj (Var (Name "a")) (Var (Name "i")))),Empty]) (Block []),Assign (Name "i") (Op2 (Var (Name "i")) Plus (Val (IntVal 1))),Empty])])

-- findMinIndex.dfy
wMinIndex = Method "FindMinIndex" [("a",TArrayInt)] [("index",TInt)] [] (Block [Decl ("min",TInt) (Var (Proj (Var (Name "a")) (Val (IntVal 0)))),Empty,Decl ("i",TInt) (Val (IntVal 0)),Empty,Assign (Name "index") (Val (IntVal 0)),Empty,While [] (Op2 (Var (Name "i")) Lt (Op1 Len (Var (Name "a")))) (Block [If (Op2 (Var (Proj (Var (Name "a")) (Var (Name "i")))) Lt (Var (Name "min"))) (Block [Assign (Name "min") (Var (Proj (Var (Name "a")) (Var (Name "i")))),Empty,Assign (Name "index") (Val (IntVal 1)),Empty]) (Block []),Assign (Name "i") (Op2 (Var (Name "i")) Plus (Val (IntVal 1))),Empty])])

-- minMax.dfy
wMinMax =  Method "MinMax" [("x",TInt),("y",TInt)] [("min",TInt),("max",TInt)] [] (Block [If (Op2 (Var (Name "x")) Lt (Var (Name "y"))) (Block [Assign (Name "min") (Var (Name "x")),Empty,Assign (Name "max") (Var (Name "y")),Empty]) (Block [Assign (Name "max") (Var (Name "x")),Empty,Assign (Name "min") (Var (Name "y")),Empty])])

-- arraySpec.dfy
wArraySpec = Method "ArrayTest" [("a",TArrayInt)] [("x",TInt)] [Requires (Predicate [] (Op2 (Op1 Len (Var (Name "a"))) Gt (Val (IntVal 0)))),Requires (Predicate [("i",TInt)] (Op2 (Op2 (Op2 (Op2 (Val (IntVal 0)) Le (Op2 (Var (Name "i")) Conj (Var (Name "i")))) Lt (Op1 Len (Var (Name "a")))) Implies (Var (Proj (Var (Name "a")) (Var (Name "i"))))) Gt (Val (IntVal 0)))),Ensures (Predicate [] (Op2 (Var (Name "x")) Gt (Val (IntVal 0))))] (Block [Assign (Name "x") (Var (Proj (Var (Name "a")) (Val (IntVal 0)))),Empty])

tFib = Method 
  "ComputeFib" [("n", TInt)] [("b", TInt)] 
  [Requires (Predicate [] (Val (BoolVal True))), Ensures (Predicate [] (Op2 (Var (Name "b")) Eq (Val (IntVal 55))))]
  (Block [
    If (Op2 (Var (Name "n")) Eq (Val (IntVal 0)))
      (Block [Assign (Name "b") (Val (IntVal 0))])
      (Block [
        Decl ("i", TInt) (Val (IntVal 1)),
        Decl ("a", TInt) (Val (IntVal 0)),
        Assign (Name "b") (Val (IntVal 1)),
        While [
          Predicate [] (Op2 (Var (Name "i")) Le (Var (Name "n"))),
          Predicate [] (Op2 (Var (Name "b")) Eq (Val (IntVal 5))),
          Predicate [] (Op2 (Var (Name "a")) Eq (Val (IntVal 4)))
        ] (Op2 (Var (Name "i")) Lt (Var (Name "n"))) (Block [
          Assign (Name "b") (Op2 (Var (Name "a")) Plus (Var (Name "b"))),
          Assign (Name "a") (Var (Name "b")),
          Assign (Name "i") (Op2 (Var (Name "i")) Plus (Val (IntVal 1)))
        ])
      ])
  ])

{- | A Pretty Printer for Lu |
   ===========================

The derived `Show` instances for the datatypes above are pretty hard to
read, especially when programs get long. Really, who wants to read this...

>>> wAbs
> Method "Abs" [("r",TInt)] [("absR",TInt)] [] (Block [If (Op2 (Var (Name "r")) Lt (Val (IntVal 0))) (Block [Assign (Name "absR") (Op1 Neg (Var (Name "r"))),Empty]) (Block [Assign (Name "absR") (Var (Name "r")),Empty])])

instead of this...

    ghci> putStrLn (pretty wAbs)
    method Abs (r : int) returns (absR : int)
    {
        if r < 0 {
            absR := -r;
        }
        else {
            absR := r; 
        }
    }

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
     types, such as characters and string.

     For example, we can use the `text` function to define the `Uop` instance of the `PP`
     class.  This instance converts each unary operator into a document.
-}

instance PP Uop where
  pp Neg = PP.char '-'
  pp Not = PP.char '!'
  pp Len = PP.text ".Length"

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
          brackets :: Doc -> Doc

          -- wrap document in {..}
          braces :: Doc -> Doc

-}

{- | Pretty-Pretter implementation |
   ---------------------------------

Your job will be to complete the following functionality, ensuring that the output
of the pretty printer is valid Dafny---that is, you can parse it in Visual Studio
if you load it as a .dfy.

-}

-- | We've given you the `PP` instances for String and Int to get started.
instance PP String where
  pp = PP.text

instance PP Int where
  pp = PP.int

-- | TODO: Implement pretty printing for Booleans and lists of integers
instance PP Bool where
  pp True = PP.text "True"
  pp False = PP.text "False"

instance PP [Int] where
  pp xs = PP.brackets $ PP.hcat (PP.punctuate (pp ", ") (map pp xs))

-- | That should allow you to also pretty pring values if needed.
instance PP Value where
  pp (IntVal i)  = pp i
  pp (BoolVal b) = pp b
  pp (ArrayVal l)  = pp l

-- | TODO: Implement pretty printing for binary operators
instance PP Bop where
  pp Plus   = PP.char '+'
  pp Minus  = PP.char '-'
  pp Times  = PP.char '*'
  pp Divide = PP.char '/'
  pp Modulo = PP.char '%'
  pp Eq     = PP.char '='
  pp Neq    = PP.text "!="
  pp Gt     = PP.char '>'
  pp Ge     = PP.text ">="
  pp Lt     = PP.char '<'
  pp Le     = PP.text "<="
  pp Conj   = PP.text "&&"
  pp Disj   = PP.text "||"
  pp Implies= PP.text "==>"
  pp Iff    = PP.text "<->"
  pp _ = undefined

-- | Types and bindings can be pretty printed

instance PP Type where
  pp TInt  = PP.text "int"
  pp TBool = PP.text "bool"
  pp TArrayInt = PP.text "array<int>"

instance PP Binding where
  pp (x, t) = PP.text x <+> PP.text ":" <+> pp t


{- |
   Expressions are trickier if you want to avoid putting parentheses everywhere.

   The following code uses two heuristics to ensure minimal parentheses
   are placed:

   * When printing single-operand operations, we don't wrap
     "base" expressions in parentheses.
   * When printing binary operations, we take the precedence level
     of the operator into account.

   We've implemented this logic for you, but there are is one thing
   missing: the array ".Length" operation is now not handled.

TODO: Make sure you implement pretty printing for ".Length" by
editing the following code appropriately.

-}

instance PP Expression where
  pp (Var v) = pp v
  pp (Val v) = pp v
  pp (Op1 Len v) = (if isBase v then pp v else PP.parens (pp v)) <> pp Len
  pp (Op1 o v) = pp o <+> if isBase v then pp v else PP.parens (pp v)
  pp e@Op2{} = ppPrec 0 e  where
     ppPrec n (Op2 e1 bop e2) =
        ppParens (level bop < n) $
           ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
     ppPrec _ e' = pp e'
     ppParens b = if b then PP.parens else id

isBase :: Expression -> Bool
isBase Val{} = True
isBase Var{} = True
isBase Op1{} = True
isBase _ = False

level :: Bop -> Int
level Times  = 7
level Divide = 7
level Plus   = 5
level Minus  = 5
level Conj   = 3
level _      = 2    -- comparison operators

-- | TODO: Implement pretty printing for variables
instance PP Var where
  pp (Name x) = PP.text x
  pp (Proj table i) = pp table <> PP.brackets (pp i)

-- | TODO: Implement pretty printing for blocks
instance PP Block where
  pp (Block b) = PP.nest 2 (PP.vcat (map pp b))

-- | TODO: Implement the rest of pretty printing for statements.
instance PP Statement where
  pp Empty = PP.empty
  pp (Decl bind expr) = (pp "var" <+> pp bind <+> pp ":=" <+> pp expr) <> pp ";"
  pp (New bind expr) = (pp "var" <+> pp ":=" <+> pp expr) <> pp ";"
  pp (Assert pred) = PP.parens $ pp "assert" <+> pp pred
  pp (Assign var expr) = (pp var <+> pp ":=" <+> pp expr) <> pp ";"
  pp (If expr b1 (Block [])) = 
    pp "if" <+> pp expr <+> pp "{" 
    PP.$$ PP.nest 2 (pp b1)
    PP.$$ pp "}"
  pp (If expr b1 b2) = 
    pp "if" <+> pp expr <+> pp "{" 
    PP.$$ PP.nest 2 (pp b1)
    PP.$$ pp "} else {" 
    PP.$$ PP.nest 2 (pp b2)
    PP.$$ pp "}"
  pp (While pred expr b) = 
    pp "while" <+> PP.parens (pp expr)
    PP.$$ PP.nest 2 (PP.vcat (map pp pred))
    PP.$$ pp "{"
    PP.$$ PP.nest 2 (pp b)
    PP.$$ pp "}"

-- | TODO: Implement pretty printing for predicates
instance PP Predicate where
  pp (Predicate bind expr) = 
    case bind of
      [] -> pp expr
      _ -> pp "forall" <+> PP.hcat (PP.punctuate (pp ", ") (map pp bind)) <+> pp "::" <+> pp expr

instance PP Specification where
  pp (Requires pred)  = pp "requires" <+> pp pred
  pp (Ensures pred)   = pp "ensures" <+> pp pred
  pp (Modifies name)  = pp "modifies" <+> pp name

-- | TODO: Finally, implement pretty printing for MiniDafny methods
instance PP Method where
  pp (Method name bind1 bind2 spec block) = 
    pp "method" <+> pp name
    <+> PP.parens (PP.hcat (PP.punctuate (pp ", ") (map pp bind1)))
    <+> PP.text "returns"
    <+> PP.parens (PP.hcat (PP.punctuate (pp ", ") (map pp bind2)))
    PP.$$ PP.nest 2 (PP.vcat (map pp spec))
    PP.$$ pp "{" 
    PP.$$ PP.nest 2 (pp block)
    PP.$$ pp "}"


{- | Arbitrary Mini Dafny |
   ========================

As usual with QuickCheck, we need an `Arbitrary` instance for MiniDafny programs.
We've provided these instances for you If you'd like to see what sorts of
expressions and programs are being generated, you can try running the
following commands in the terminal.

> sampleVar :: IO ()
> sampleVar = QC.sample' (arbitrary :: Gen Var) >>= mapM_ (print . pp)

> sampleExp :: IO ()
> sampleExp = QC.sample' (arbitrary :: Gen Expression) >>= mapM_ (print . pp)

> sampleStat :: IO ()
> sampleStat = QC.sample' (arbitrary :: Gen Statement) >>= mapM_ (print . pp)

To test with a lot of tests while still making sure that our Lu programs
don't get too big, we can limit the size used by quickcheck.

> quickCheckN :: QC.Testable prop => Int -> prop -> IO ()
> quickCheckN n = QC.quickCheckWith $ QC.stdArgs { QC.maxSuccess = n , QC.maxSize = 100 }

-}

module Arbitrary where

import Syntax

import Test.QuickCheck(Arbitrary(..),Gen)
import qualified Test.QuickCheck as QC


-- | Generators 

-- | Generate a small set of names for generated tests. These names are guaranteed to not include
-- reserved words
genName :: Gen Name
genName = QC.elements [ "x", "X", "y", "x0",  "X0", "xy", "XY" ]

-- | Generate types
genType :: Gen Type
genType = QC.elements [ TInt, TBool, TArrayInt ]

-- | Generate a size-controlled global variable or table field
genVar :: Int -> Gen Var
genVar 0 = Name <$> genName
genVar n = QC.frequency [(1, Name <$> genName)
                        ,(n, Proj <$> genName <*> genExp n')]
   where n' = n `div` 2

-- | Generate a size-controlled expression
genExp :: Int -> Gen Expression
genExp 0 = QC.oneof [ Var <$> genVar 0, Val <$> arbitrary ]
genExp n = QC.frequency [ (1, Var <$> genVar n)
                        , (1, Val <$> arbitrary)
                        , (n, Op1  <$> arbitrary <*> genExp n') 
                        , (n, Op2  <$> genExp n' <*> arbitrary <*> genExp n') ]
    where n' = n `div` 2

-- | Generate a size-controlled statement
genStatement :: Int -> Gen Statement
genStatement n | n <= 1 = QC.oneof [Assign <$> genVar 0 <*> genExp 0, return Empty]
genStatement n = QC.frequency [ (1, Assign <$> genVar n' <*> genExp n')
                              , (1, Decl <$> ((,) <$> genName <*> genType) <*> genExp n')
                              , (1, return Empty)
                              , (n, If <$> genExp n' <*> genBlock n' <*> genBlock n')
                              -- generate loops half as frequently as if statements
                              , (n', While <$> (Predicate <$> genExp n') <*> genExp n' <*> genBlock n')
                           ]
    where n' = n `div` 2

genBlock :: Int -> Gen Block
genBlock n = Block <$> genStmts n where
    genStmts 0 = pure []
    genStmts n = QC.frequency [ (1, return []),
                                (n, (:) <$> genStatement n' <*> genStmts n')]
           where n' = n `div` 2

-- | Instances

instance Arbitrary Var where
  arbitrary :: Gen Var
  arbitrary = QC.sized genVar
  shrink :: Var -> [Var]
  shrink (Name n)  = []
  shrink (Proj e1 e2) = [Proj e1' e2 | e1' <- shrink e1] ++
                        [Proj e1 e2' | e2' <- shrink e2]

instance Arbitrary Type where
  arbitrary = genType
  shrink = const []

instance Arbitrary Statement where
  arbitrary :: Gen Statement
  arbitrary = QC.sized genStatement
  shrink :: Statement -> [Statement]
  shrink (Assign v e) = [ Assign v' e | v'  <- shrink v ] ++
                        [ Assign v e' | e'  <- shrink e ]
  shrink (If e b1 b2) = first b1 ++ first b2 ++
                        [ If e' b1 b2 | e'  <- shrink e ] ++
                        [ If e b1' b2 | b1' <- shrink b1 ] ++
                        [ If e b1 b2' | b2' <- shrink b2 ]
  shrink (While i e b) = first b ++
                        [ While i e' b   | e'  <- shrink e ] ++
                        [ While i e b'   | b'  <- shrink b ]
  shrink (Decl b e) =  [ Decl b' e | b'  <- shrink b ] ++
                       [ Decl b e' | e'  <- shrink e ]
  shrink (Assert p) = [Assert p' | p' <- shrink p]
  shrink Empty = []

instance Arbitrary Predicate where
  arbitrary = Predicate <$> arbitrary
  shrink (Predicate p) = [Predicate p' | p' <- shrink p]

-- | access the first statement in a block, if one exists
first :: Block -> [Statement]
first (Block []) = []
first (Block (x:_)) = [x]

instance Arbitrary Block where
  arbitrary :: Gen Block
  arbitrary = QC.sized genBlock
  shrink :: Block -> [Block]
  shrink (Block ss) = [ Block ss' | ss' <- shrink ss ]

instance Arbitrary Expression where
  arbitrary :: Gen Expression
  arbitrary = QC.sized genExp

  shrink :: Expression -> [Expression]
  shrink (Val v)          = Val <$> shrink v
  shrink (Var v)          = Var <$> shrink v
  shrink (Op1 o e)        = e : [Op1 o e' | e' <- shrink e]
  shrink (Op2 e1 o e2)    =
    [ Op2 e1' o e2 | e1' <- shrink e1 ] ++
    [ Op2 e1 o e2' | e2' <- shrink e2 ] ++
    [ e1, e2 ]

instance Arbitrary Uop where
  arbitrary :: Gen Uop
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Bop where
  arbitrary :: Gen Bop
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Value where
  arbitrary :: Gen Value
  arbitrary = QC.oneof [ IntVal <$> arbitrary
                       , BoolVal <$> arbitrary
                       -- note: do not generate array values
                       ]

  shrink :: Value -> [Value]
  shrink (IntVal n) = IntVal <$> shrink n
  shrink (BoolVal b) = BoolVal <$> shrink b
  shrink (ArrayVal l) = [] -- ignore

{- | Weakest Preconditions |
   =========================

In this file, you are going to calculate weakest preconditions for
the non-array part of Mini Dafny, using the Hoare Logic rules we
introduced in the beginning of the semester to propagate "ensures"
clauses backwards from the end of a block of statements, and use
those to calculate which verification conditions need to hold for
a program to be correct.

We are going to use the "Square.dfy" program that we have been using
throughout the class as our running example:

method Square (x : int) returns (z : int) 
  requires x > 0 
  ensures z == x * x
{
    var y : int := 0;
    z := 0;
    while y < x 
      invariant y <= x && z == y * x
    {
      z := z + x;
      y := y + 1;
    }
}

and it's full annotation in Hoare Logic:

{ x > 0} ->                               (1)
{ 0 <= x && 0 == 0 * x }
    var y : int := 0;
{ y <= x && 0 == y * x }
    z := 0;
{ y <= x && z == y * x }
    while y < x {
{ y <= x && z == y * x && y < x } ->      (2)
{ y + 1 <= x && z + x == (y + 1) * x }
      z := z + x;
{ y + 1 <= x && z == (y + 1) * x }
      y := y + 1;
{ y <= x && z == y * x }
    }
{ y <= x && z == y * x && !(y < x) } ->   (3)
{ z == x * x }

-}

module WP where

import Printer
import Syntax

import Test.HUnit

{- | Substitution |
   ================

Recall the Hoare Logic rule for assignment:

   ---------------------
   {Q[x := a] x := a; {Q}

We read this rule as "in order for predicate Q to hold
after executing an assignment, we need Q but with
x substituted for a to hold before".  Which means that
before we calculate weakest preconditions for MiniDafny
statements, we need to define substitution.


We begin by defining a typeclass that characterizes
substitution, given a variable Name and an Expression to
be substituted in.

-}

class Subst a where
  subst :: a -> Name -> Expression -> a

-- | We will write subst e x e' for e [x := e']. To perform this substitution
-- you need to do case analysis on e, propagating the substitution via
-- recursion until you reach the base case of a variable. At that point,
-- you can either perform the substitution or not. Remember, we're ignoring
-- arrays for this project, so we have already left this bit out.

instance Subst Expression where
  subst (Var (Proj _ _)) _ _ = error "Ignore arrays for this project"
  subst (Var (Name e)) x e' = if e == x then e' else Var (Name e)
  subst (Val v) _ _ = Val v
  subst (Op1 uop e) x e' = Op1 uop (subst e x e')
  subst (Op2 e1 bop e2) x e' = Op2 (subst e1 x e') bop (subst e2 x e')

-- | As an example, consider the loop invariant of Square:
--
--   y <= x && z == y * x
--
--   Or, in our AST representation:

wInv :: Expression
wInv =  Op2 (Op2 (Var (Name "y")) Le (Var (Name "x")))
            Conj
            (Op2 (Var (Name "z")) Eq (Op2 (Var (Name "y")) Times (Var (Name "x"))))

-- | When propagating the loop invariant backwards inside the loop body,
--   we substitute "y+1" for "y" and obtain:
--
--   y + 1 <= x && z == (y + 1) * x
--
--   That is:

wYPlus1 :: Expression
wYPlus1 = Op2 (Var (Name "y")) Plus (Val (IntVal 1))

wInvSubstYY1 :: Expression
wInvSubstYY1 = Op2 (Op2 (Op2 (Var (Name "y")) Plus (Val (IntVal 1))) Le (Var (Name "x")))
                   Conj
                   (Op2 (Var (Name "z")) Eq (Op2 (Op2 (Var (Name "y")) Plus (Val (IntVal 1))) Times (Var (Name "x"))))

test_substExp :: Test
test_substExp = TestList [ "exp-subst" ~: subst wInv "y" wYPlus1 ~?= wInvSubstYY1 ]

-- | You will also need to implement substitution for predicates, which is
--   simply a matter of invoking substitution for expressions.

instance Subst Predicate where
  subst (Predicate pred) x e = Predicate (subst pred x e)

test_substPred :: Test
test_substPred = TestList [ "pred-subst" ~: subst (Predicate wInv) "y" wYPlus1 ~?= Predicate wInvSubstYY1 ]


{- | Calculating Weakest Preconditions |
   -------------------------------------

Next up, we are going to actually calculate weakest preconditions.
We are also going to organize this in a typeclass, to account for
both statements and blocks of statements.

-}

class WP a where
  wp :: a -> Predicate -> Predicate

{- | The core of automated reasoning using Hoare Logic is the
     calculation of weakest preconditions for statements
     Ignoring asserts and arrays (for this project), we need to
     calculate, using Hoare rules, the weakest precondition
     that needs to hold before the statement is executed in order
     for the postcondition to hold.

   * For empty statements, we simply leave the postcondition unchanged:

                           --------------------
                            {Q}    { }     {Q}

   * For assignments and declarations we can use the assignment rule
     (ignoring Dafny's requirements that variables assigned to need
     to already be declared, and that variables cannot be redeclared):


                         ----------------------
                         {Q[x := a] x := a; {Q}

     That is, we substitute the expression being assigned to a variable,
     for that variable, in the postcondition predicate.

   * For if statements, let's take a look at the Hoare rule:

                                {P && b } s1 {Q}
                                {P && !b} s2 {Q} 
                        -------------------------------
                        {P} if b { s1 } else { s2 } {Q}

      What is the weakest precondition in this case? We can calculate
      the weakest precondtion for both the "then" and the "else"
      statement blocks. If P1 is the weakest precondition of s1, and P2
      is the weakest precondtion of s2, then the weakest precondition
      for the if statement is

                            b ==> P1 && !b ==> P2

    * For while statements, let's take a look at the Hoare rule:

                                 {P && b} s {P}
                         ---------------------------
                         {P} while b { s } {P && !b}

      While loops are not amenable to automatically computing preconditions,
      so we can simply use the loop invariant as that precondition, as you
      did on paper.

-}

instance WP Statement where
  wp (Assert _) p = error "Ignore assert for this project"
  wp (Assign (Proj _ _) _) p = error "Ignore arrays for this project"
  wp (Assign (Name x) e) p = subst p x e
  wp (Decl (x, _) e) p = subst p x e
  wp (If e b1 b2) p = wpIf e b1 b2 p
  wp (While inv _ _) p = inv  -- Loop invariant is the weakest precondition
  wp Empty p = p  -- No effect on postcondition

wpIf :: Expression -> Block -> Block -> Predicate -> Predicate
wpIf e b1 b2 p =
  let Predicate p1 = wp b1 p
      Predicate p2 = wp b2 p
  in Predicate (Op2 e Implies (Op2 (Op1 Not e) Implies (Op2 p1 Conj p2)))

-- | You will also need to implement weakest preconditions for blocks
--   of statements, by repeatedly getting the weakest precondition
--   starting from the end.
--   HINT: folds are your friend.
instance WP Block where
  wp (Block b) p = foldr wp p b

{- | Verification conditions |
   ---------------------------

   The next part of automating verification of programs is to
   use the weakest precondition calculation to compute which
   implications need to hold. For blocks of statements that don't
   contain a while loop, then the only verification condition
   is that the precondition implies the weakest precondition
   of the postcondition through the block---similar to (1) in
   the square example.

   However, each while loop in a program introduces two additional
   verification conditions:

   * The loop invariant plus the guard should imply the weakest
     precondition of the loop invariant through the loop body.
   * The loop invariant plus the negation of the guard should imply
     the weakest precondition of the continuation of the program (or
     the postcondition itself if the while loop is the last statement).

   That is, the following test should pass:
-}

-- | The while loop from Square:
-- while y < x
--   invariant y <= x && z == y * x
-- {
--   z := z + x;
--   y := y + 1;
-- }
wSquareWhile :: Statement
wSquareWhile = While (Predicate (Op2 (Op2 (Var (Name "y")) Le (Var (Name "x"))) Conj (Op2 (Var (Name "z")) Eq (Op2 (Var (Name "y")) Times (Var (Name "x")))))) (Op2 (Var (Name "y")) Lt (Var (Name "x"))) (Block [Assign (Name "z") (Op2 (Var (Name "z")) Plus (Var (Name "x"))),Empty,Assign (Name "y") (Op2 (Var (Name "y")) Plus (Val (IntVal 1))),Empty])

-- | The post condition of Square
-- z == x * x
wWhilePost :: Expression
wWhilePost = Op2 (Var (Name "z")) Eq (Op2 (Var (Name "x")) Times (Var (Name "x")))

-- | The two verification conditions it gives rise to --- (2) and (3) above.
-- y <= x && z == y * x && y < x ==> (y + 1 <= x && z + x == (y + 1) * x)
-- y <= x && z == y * x && ! (y < x) ==> z == x * x

vcsWhile :: [Predicate]
vcsWhile =
  [ Predicate (Op2 (Op2 (Op2 (Op2 (Var (Name "y")) Le (Var (Name "x"))) Conj (Op2 (Var (Name "z")) Eq (Op2 (Var (Name "y")) Times (Var (Name "x"))))) Conj (Op2 (Var (Name "y")) Lt (Var (Name "x")))) Implies (Op2 (Op2 (Op2 (Var (Name "y")) Plus (Val (IntVal 1))) Le (Var (Name "x"))) Conj (Op2 (Op2 (Var (Name "z")) Plus (Var (Name "x"))) Eq (Op2 (Op2 (Var (Name "y")) Plus (Val (IntVal 1))) Times (Var (Name "x"))))))
  ,Predicate (Op2 (Op2 (Op2 (Op2 (Var (Name "y")) Le (Var (Name "x"))) Conj (Op2 (Var (Name "z")) Eq (Op2 (Var (Name "y")) Times (Var (Name "x"))))) Conj (Op1 Not (Op2 (Var (Name "y")) Lt (Var (Name "x"))))) Implies (Op2 (Var (Name "z")) Eq (Op2 (Var (Name "x")) Times (Var (Name "x")))))]

test_vcStmt :: Test
test_vcStmt =
  TestList [ "vc - while" ~: vcStmt (Predicate wWhilePost) wSquareWhile ~?= vcsWhile ]

-- | To implement this, first, calculate the latter two for a single (While) statement:
vcStmt :: Predicate -> Statement -> [Predicate]
vcStmt (Predicate p) (While (Predicate inv) e (Block b)) =
  let Predicate wpBody = foldr wp (Predicate inv) b
      vc1 = Predicate (Op2 (Op2 inv Conj e) Implies wpBody)
      vc2 = Predicate (Op2 (Op2 inv Conj (Op1 Not e)) Implies p)
  in [vc1, vc2]
vcStmt _ _ = []

-- | Then, calculate the while loop verification conditions for blocks.
vcBlock :: Predicate -> Block -> [Predicate]
vcBlock (Predicate p) (Block b) = foldr process [] b
  where
    process :: Statement -> [Predicate] -> [Predicate]
    process s [] = vcStmt (Predicate p) s
    process s (c : cs) =
      let Predicate (Op2 left Implies _) = c
          Op2 inv Conj _ = left
      in
        vcStmt (Predicate inv) s ++ (c : cs)
-- vcBlock post (Block []) = []
-- vcBlock post (Block (x:xs)) =
-- vcBlock _ (Block []) = []
-- vcBlock p (Block (x:xs)) =
--   let vcStmts = case x of
--                   While inv e b -> vcStmt p (While inv e b)
--                   _ -> []
--   in vcStmts ++ vcBlock (wp (Block [x]) p) (Block xs)

{- | Lifting to Methods |
   ----------------------

Then, to put everything together, we need to use the specification of the methods
as the precondition and postcondition of the method body.

First, we provide you with code that joins requires and ensures specifications
of a method together into a single expression.
-}
requires :: [Specification] -> Expression
requires [] = Val (BoolVal True)
requires (Requires (Predicate e) : ps) = Op2 e Conj (requires ps)
requires (_ : ps) = requires ps

ensures :: [Specification] -> Expression
ensures [] = Val (BoolVal True)
ensures (Ensures (Predicate e) : ps) = Op2 e Conj (ensures ps)
ensures (_ : ps) = ensures ps

{- | Finally, given a method, use the requires and ensures functions
     defined above to calculate the verification conditions for the
     whole method:
     - The method precondition implies the weakest precondtion of the
       method postcondition through the method body.
     - Followed by the verification conditions that while loops in the
       method block give rise to.
-}
vc :: Method -> [Predicate]
vc (Method _ _ _ specs (Block b)) =
  let e = ensures specs
      r = requires specs
      Predicate wpBody = wp (Block b) (Predicate r) -- give Predicate
      vcBody = vcBlock (Predicate e) (Block b) -- gives [Predicate]
  in
  Predicate (Op2 r Implies wpBody) : vcBody

-- | As a complete end-to-end test, the verification conditions for the whole of
--   the Square method is the list of the following three expressions (in order):
-- Right (Op2 (Var (Name "x")) Gt (Op2 (Val (IntVal 0)) Conj (Val (BoolVal True))))
-- x > 0 && true ==> (0 <= x && 0 == 0 * x)
-- issue for implies .. Conj 0 == 0 * x:
-- I have: y * x, it should be: 0 * x

-- Right (Op2 (Op2 (Op2 (Var (Name "y")) Le (Op2 (Var (Name "x")) Conj (Var (Name "z")))) Eq (Op2 (Op2 (Var (Name "y")) Times (Var (Name "x"))) Conj (Var (Name "y")))) Lt (Var (Name "x")))
-- y <= x && z == y * x && y < x ==> (y + 1 <= x && z + x == (y + 1) * x)
-- Right (Op2 (Op2 (Var (Name "y")) Le (Op2 (Var (Name "x")) Conj (Var (Name "z")))) Eq (Op2 (Op2 (Var (Name "y")) Times (Var (Name "x"))) Conj (Op1 Not (Op2 (Var (Name "y")) Lt (Var (Name "x"))))))

-- y <= x && z == y * x && ! (y < x) ==> (z == x * x && true)
-- issue for implies z == x * x:
-- I have: x > 0, it should be z == x * x (ENSURES)

vcSquare :: [Predicate]
vcSquare = [ Predicate (Op2 (Op2 (Op2 (Var (Name "x")) Gt (Val (IntVal 0))) Conj (Val (BoolVal True))) Implies (Op2 (Op2 (Val (IntVal 0)) Le (Var (Name "x"))) Conj (Op2 (Val (IntVal 0)) Eq (Op2 (Val (IntVal 0)) Times (Var (Name "x"))))))
           , Predicate (Op2 (Op2 (Op2 (Op2 (Var (Name "y")) Le (Var (Name "x"))) Conj (Op2 (Var (Name "z")) Eq (Op2 (Var (Name "y")) Times (Var (Name "x"))))) Conj (Op2 (Var (Name "y")) Lt (Var (Name "x")))) Implies (Op2 (Op2 (Op2 (Var (Name "y")) Plus (Val (IntVal 1))) Le (Var (Name "x"))) Conj (Op2 (Op2 (Var (Name "z")) Plus (Var (Name "x"))) Eq (Op2 (Op2 (Var (Name "y")) Plus (Val (IntVal 1))) Times (Var (Name "x"))))))
           , Predicate (Op2 (Op2 (Op2 (Op2 (Var (Name "y")) Le (Var (Name "x"))) Conj (Op2 (Var (Name "z")) Eq (Op2 (Var (Name "y")) Times (Var (Name "x"))))) Conj (Op1 Not (Op2 (Var (Name "y")) Lt (Var (Name "x"))))) Implies (Op2 (Op2 (Var (Name "z")) Eq (Op2 (Var (Name "x")) Times (Var (Name "x")))) Conj (Val (BoolVal True))))]

test_vc_method :: Test
test_vc_method = TestList [ "vc square" ~: vc wSquare ~?= vcSquare ]


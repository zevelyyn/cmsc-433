datatype Peano = 
  | Zero
  | Succ (Peano) // successor

/** Quiz:

a.  Is Succ(Zero) equal to Succ(Zero)? 
      Yes

b.  Is Succ(Zero) equal to Zero? No

c.  Does there exist a Peano number n such that Succ(n) is equal to Zero? 
      No

d.  If Succ(zero) is equal to Succ(x), do we know something about x? 
      Yes, x is zero

e.  Let's define another datatype: 
    
    datatype Positive = 
    | One 
    | Inc(Positive)

    Is One an element of the Peano datatype?

f.  How do you prove things about Peano?
      Induction
      
  ghost = a mathematical object, not a programming object

  lemma Proposition()
    ensures forall m: int, n: int :: m > 0 && n > m ==> m + n > 0

  lemma Prop(x: int) 
    requires x % 2 == 0
    ensures (x / 2) * 2 == x 

  Types can have type characteristics
    00 says the type is nonempty
    Note that auto-init (0) implies nonempty (00)
    NaturalNumber stands for a fixed but arbitrary, nonempty type

    type NaturalNumber(00)

  Rather than:

  ghost function Divide(m: int, n: int): int

  lemma EssenceOfDivide(m: int, n: int)
    requires n != 0
    ensures Divide(m, n) == m / n

  You can define a partial function:

  ghost function Divide(m: int, n: int): int
    requires n != 0
  {
    m / n
  }

 */

function SumUpTo(counter: int, upTo: int): int
  // What decreases clause is needed here?
  decreases (upTo - counter)
  {
    if upTo <= counter then
      counter
    else
      counter + SumUpTo(counter + 1, upTo)
  }


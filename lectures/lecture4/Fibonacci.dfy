function fib(n:nat) : nat {
   match n
   case 0 => 0
   case 1 => 1
   case _ => fib(n-1) + fib(n - 2)
}

// nat = natural number (0, 1, 2, ...)
method ComputeFib(n: nat) returns (b: nat)
   requires true
   ensures b == fib(n)
{
   if n == 0 { return 0; }
   var i: int := 1;
   var a := 0;
       b := 1;
   while i < n
      invariant i <= n
      invariant b == fib(i)
      invariant a == fib(i - 1)
   {
      a, b := b, a + b;
      i := i + 1;
   }
   // b == fib(i)
   // a == fib(i - 1)
   //
   // conclude b == fib(n)
}
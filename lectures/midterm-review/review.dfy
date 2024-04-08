/*
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b [] = b
foldr f b (x:xs) = f x (foldr f b xs)

f :: [Int] -> Int
f = foldr (+) 0
*/

// method f(a:array<int) returns (out:int) {
//     var i : int := a.Length - 1;
//     out := 0;

//     while (i >= 0) {
//         out := out + a[i];
//         i := i - 1;
//     }
// }

method Square(x:int) returns (z:int) 
    requires x > 0
    ensures z == x * x
{
    var y := 0;
    z := 0;
    while y < x 
        invariant z == y * x
    {
        z := z + x;
        y := y + 1;
    }
}

/*
method Square(x:int) returns (z:int) 
    requires x > 0
    ensures z == x * x
{
    { m > 0 } ->>
    !! { 0 <= m && 0 == 0 * m }
    var y := 0;
    !! { y <= m && 0 == 0 * y }
    z := 0;
    !! { y <= m && z == y * m }
    while y < m {
        !! { y < x && z == y * m && y < m } ->>
        !! { z + x == (y + 1) * m && y + 1 <= m }
        z := z + x;
        !! { z == (y + 1) * m && y + 1 <= m }
        y := y + 1;
        !! { z == y * m && y <= m }
    }
    !! { !(y < m) && y <= m && z == y * m } ->>
    { y == m * m }

    INV => z == y * m
}
*/

/*
Convert the following Haskell function that operates on lists of integers 
to an equivalent Dafny method that operates on an array of integers:

foo :: [Int] -> Int
foo = foldr (+) 0
*/

method foo(a:array<int>) returns (b:int) {
    b := 0;
    var i : int := 0;
    while (i < a.Length) {
        b := b + a[i];
        i := i + 1;
    }
}

/* 
What is the type of the following Haskell expression? You can assume `foldr` operates on polymorphic lists and `+` on integers.

\x -> foldr (+) x

(a -> b -> b)... (+) is (num -> num -> num)
b is the accumulator type, meaning x is a num...??

foo :: [Int] -> Int
*/

/*
Consider the following Dafny program. Annotate the program using Hoare triples

method foo(x:int) returns (out:int)
  ensures out == x
{
  { x >= 0 ?? }
  var i := 0;
  { i == out }
  while (i < x) {
    { i < x && i == out } ->>
    { i + 1 == out + 1 && i+ 1 >= 0 && i + 1 <= x }
    out := out + 1;
    { i + 1 == out && i + 1 >= 0 && i + 1 <= x }
    i := i + 1;
    { i == out && i >= 0 && i <= x }
  }
  { !(i < x) && i == out && i >= 0 && i <= x } ->>
  { out == x }
}

*/
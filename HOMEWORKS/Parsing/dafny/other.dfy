method LoopToZero_2(m : nat, p : int) returns (x: nat, z: int)
  requires true
  ensures z == p - m
{
  x := m;
  z := p;
  while x > 0 
    invariant (z - x == p - m)
  {
    z := z - 1;
    x := x - 1;
  }
}

method TwoLoops(a : int, b : int, c : int) returns (x:int, y :int, z :int)
  requires a > 0
  requires b > 0
  requires c > 0
  ensures z == a + b + c
{
     x := 0;
     y := 0;
     z := c;
     while x < a
      invariant x <= a
      invariant y == 0
      invariant z == x + y + c
     {
       x := x + 1;
       z := z + 1;
     }
     while y < b
      invariant y <= b
      invariant x == a
      invariant z == a + y + c
     {
       y := y + 1;
       z := z + 1;
     }
}
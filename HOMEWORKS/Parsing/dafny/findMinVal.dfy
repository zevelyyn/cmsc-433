method FindMinVal (a : array<int>) returns (min : int)
  requires a.Length > 0
  ensures forall i : int :: (0 <= i && i < a.Length) ==> min <= a[i]
{
  min := a[0];
  var i : int := 1;
  while (i < a.Length)
  {
    if a[i] < min
    { min := a[i]; }
    i := i+1;
  }
}


method Cube(m:int) returns (y:int)
  requires m>0
  ensures y==m*m*m
{
  y := 0;
  var x := m * m;
  var z := m;
  while (z >0)
    invariant y == m*m*m - z*x
    invariant z >= 0
    // invariant x == m*m
  {
    z := z - 1;
    y := y + x;
  }
}
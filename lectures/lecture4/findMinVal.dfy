method FindMinVal (a : array<int>) returns (min : int)
    requires a.Length > 0  // Precondition
    ensures forall i : int :: 0 <= i < a.Length ==> min <= a[i] // Postcondition
{
  min := a[0];
  var i:= 1;
  while (i < a.Length)
    invariant i <= a.Length
    invariant forall j :: 0 <= j < i ==> min <= a[j]
  {
    if a[i] < min
      { min := a[i]; }
    i := i+1;
  }
  assert (forall j :: 0 <= j < a.Length ==> min <= a[j]);
}

// Define a predicate - "what an array being sorted is"
predicate sorted(a: array<int>)
  reads a
{
  forall j, k :: 0 <= j < k < a.Length ==> a[j] <= a[k]
}

lemma Foo (a: array<int>, low : int, high: int, value: int) 
    requires 0 <= a.Length && sorted(a)
    requires 0 <= low <= high <= a.Length
    requires forall i :: 0 <= i < a.Length && !(low <= i < high) ==> a[i] != value
    
    ensures forall j :: 0 <= j < low ==> a[j] != value
    ensures forall j :: high < j < a.Length ==> a[j] != value
    {
    
    }

// Binary Search Method
method BinarySearch(a: array<int>, value: int) returns (index: int)
  // Precondition:
  // When a is a sorted array with non-negative length...
  requires 0 <= a.Length && sorted(a)
  // Postconditions:
  // ...then the result (index) is either:
  // 1. non-negative, in which case it points to "value" in a
  ensures 0 <= index ==> index < a.Length && a[index] == value
  // 2. negative, in which case the value doesn't exist
  ensures index < 0 ==> forall k :: 0 <= k < a.Length ==> a[k] != value
{
  var low, high := 0, a.Length;
  while low < high
    invariant 0 <= low <= high <= a.Length
    //invariant forall j :: 0 <= j < low ==> a[j] != value
    //invariant forall j :: high < j < a.Length ==> a[j] != value
    invariant forall i :: 0 <= i < a.Length && !(low <= i < high) ==> a[i] != value
  {
    var mid := (low + high) / 2;
    if a[mid] < value {
      low := mid + 1;
    } else if value < a[mid] {
      high := mid;
    } else {
      return mid;
    }
  }
  return -1;
}












    // Invariants are required to prove facts about loops
  //  invariant 0 <= low <= high <= a.Length
    //invariant forall i :: 0 <= i < a.Length && !(low <= i < high) ==> a[i] != value
// Linear Search in an Array
// It returns the index of the key, if it finds it
// It returns -1 if the key is not in the array

method Find(a: array<int>, key: int) returns (index: int)
  // can be called on any a and key
  requires true
  // precondition: assumptions for when the code starts executing
  // postcondition: what must be satisfied when the code terminates
  ensures 0 <= index < a.Length ==> a[index] == key // ==> (then)
  ensures index < 0 ==> forall k :: 0 <= k < a.Length ==> a[k] != key
{
  index := 0;
  while index < a.Length
  {
    if a[index] == key { return; }
    index := index + 1;
  }
  index := -1;
}

method Main() {
  var a := new int[0];
  var x := Find(a, 42);
}
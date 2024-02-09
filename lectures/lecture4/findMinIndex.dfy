method Foo() returns (x : int) 
    requires true
    ensures (x == 84)
{
    // if true
    var i := 0;
    var n := 0;
    // {{ P }}
    while (i < 42) 
    { // NOTE: |-> (substitution)
        // {{ P /\ (i < 42) }}
        // {{ P[n |-> n + 2][i |-> i + 1] }}
        i := i + 1;
        // {{ P[n |-> n + 2] }}
        n := n + 2;
    }
    // {{ P /\ !(i < 42) }}
    // {{ n == 84) }}
    x := n;
    // postcondition
    // {{ x == 84 }}
}

method FindMinVal (a: array<int>) returns (min : int) 
    requires a.Length > 0 // precondition
    ensures forall i: int :: 0 <= i < a.Length ==> min <= a[i] // postcondition
{
    min := a[0];
    var i := 1;
    while (i < a.Length) 
        invariant index <= a.Length
        invariant forall k :: 0 <= k < index ==> a[k] != key
    {
        assert (i < a.Length);
        if a[i] < min {
            min := a[i];
        }
        i := i + 1;
        // assert (P /\ !(i < a.Length));
    }
}
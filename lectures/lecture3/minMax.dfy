method MinMax (x : int, y : int) returns (min : int, max : int)
    ensures (min <= x && min <= y)
    ensures (min == x || min == y)
    ensures (max >= x && max >= y)
    ensures (max == x || max == y)
{
    assert true ==> 
    ((x < y) ==> 
               ((x <= x && x <= y) && (x == x || x == y)
            && (y >= x && y >= y) && (y == x || y == y)))
            &&
    (!(x<y)) ==> 
                ((y <= x && y <= y) && (y == x || y == y)
            && (x >= x && x >= y) && (x == x || x == y));
    if (x < y) {
        assert (x < y) ==> 
               ((x <= x && x <= y) && (x == x || x == y)
            && (y >= x && y >= y) && (y == x || y == y));
        assert (x <= x && x <= y) && (x == x || x == y)
            && (y >= x && y >= y) && (y == x || y == y);
        min := x;
        assert (min <= x && min <= y) && (min == x || min == y)
            && (y >= x && y >= y) && (y == x || y == y);
        max := y;
        assert (min <= x && min <= y) && (min == x || min == y)
            && (max >= x && max >= y) && (max == x || max == y);
    }
    else {
        assert (!(x<y)) ==> 
                ((y <= x && y <= y) && (y == x || y == y)
            && (x >= x && x >= y) && (x == x || x == y));
        assert (y <= x && y <= y) && (y == x || y == y)
            && (x >= x && x >= y) && (x == x || x == y);
        max := x;
        assert (y <= x && y <= y) && (y == x || y == y)
            && (max >= x && max >= y) && (max == x || max == y);
        min := y;
        assert (min <= x && min <= y) && (min == x || min == y)
            && (max >= x && max >= y) && (max == x || max == y);
    }
    assert (min <= x && min <= y) && (min == x || min == y)
        && (max >= x && max >= y) && (max == x || max == y);
}

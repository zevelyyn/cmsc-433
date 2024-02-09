/* Exercise 1 (10 points)

Implement the following OCaml function as a dafny function:

let mystery_function x y z = 
  let a = x + y in
  let b = y + z in 
  let c = x + z in 
  (a+b+c) / 2
*/
function mystery_function(x:int,y:int,z:int):int {
    var a:int := x + y;
    var b:int := y + z;
    var c:int := x + z;

    (a + b + c)/2
}

// method Main() {
//     var a := mystery_function(1, 2, 4);
//     print(a);
// }

/* Exercise 2 (20 points)

You are given the following Dafny datatype, which is equivalent
to the following OCaml tree type:

type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)

*/

datatype Tree<T> = 
    | Leaf 
    | Node (data:T, left: Tree<T>, right:Tree<T>)

/* Implement the `map` and `fold` functions over these trees.

For reference, the OCaml definition would be:

let rec mapTree f t = 
  match t with
  | Leaf -> Leaf
  | Node (x,l,r) -> Node (f x, mapTree f l, mapTree f r)

let rec foldTree f e t = 
  match t with
  | Leaf -> e 
  | Node (x,l,r) -> f x (foldTree f e l) (foldTree f e r)

Here is the stub for map:

*/
function mapTree<A,B> (f: A -> B, t: Tree<A>): Tree<B> {
    match t
    case Leaf => Leaf
    case Node(x,l,r) => Node(f(x), mapTree(f, l), mapTree(f, r))
}

/* Fill in your own template for fold, with the same argument order as the OCaml code. */

function foldTree<A,B> (f: (A, B, B) -> B, e: B, t: Tree<A>): B {
    match t
    case Leaf => e
    case Node(x,l,r) => f(x, (foldTree(f, e, l)), (foldTree(f, e, r)))
}

/* Exercise 3 (20 points)

You are given the following MapSet wrapper arround Dafny's maps, which
fixes the type of the "values" to be booleans. As a result,
one can think of a MapSet as a set, where an element is considered
to be in the MapSet iff it maps to true in the wrapped map.

Implement the following set API calls in terms of this 
wrapper, by invoking Dafny map functions as seen on the slides.

*/

datatype MapSet<T> = MapSet (s : map<T,bool>)

predicate member<T> (m:MapSet<T>, x:T) {
    x in m.s
}

function size<T> (m:MapSet<T>): int {
    |m.s|
}

function insert<T> (m:MapSet<T>, x:T): MapSet<T> {
    MapSet(s := m.s[x := true])
}

function delete<T> (m:MapSet<T>, x:T): MapSet<T> {
    MapSet(s := m.s - {x})
}

// method Main() {
//     // Create a new MapSet
//     var mySet := MapSet(s := map[]);

//     // Insert some elements into the set
//     mySet := insert(mySet, 1);
//     mySet := insert(mySet, 2);
//     mySet := insert(mySet, 3);

//     print(mySet);
//     // Test membership
//     assert member(mySet, 1);
//     assert member(mySet, 2);
//     assert member(mySet, 3);
//     assert !member(mySet, 4);

//     // Test size
//     assert size(mySet) == 3;
//     print(size(mySet));

//     // Delete an element
//     mySet := delete(mySet, 2);
//     assert !member(mySet, 2);
//     assert size(mySet) == 2;
//     print(size(mySet));
// }

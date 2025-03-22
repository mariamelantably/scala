/** state: S:P Int[0..N)
* init S = {} */
trait IntSet{
    /** Add elem to the test
    * pre: elem ∈ [0..N)
    * post: S = S_0 ∪ {elem} */
    def add(elem : Int) : Unit

    /** Does the set contain elem?  
    * pre: elem ∈ [0..N)
    * post: S = S_0 ∧ returns elem ∈ S */
    def contains(elem : Int) : Boolean

    /** Remove elem from the set.
    * pre: elem ∈ [0..N)
    * post: S = S_0 - {elem} */
    def remove(elem : Int) : Unit

    /** The size of the set.
    * post: S = S_0 ∧ returns |S| */
    def size : Int

    //Question 6
    //Part (a)
    /** Returns the first element of the set
     * pre: |S| > 1
     * post: S = S_0 ∧ returns first element */
    def head : Int
}


//Part (b) 
/** state: S:P Int[0..N)
* init S = {} 
* DTI: ∀ i ∈ [0..N), a(i) <-> i ∈ S, ¬a(i) <-> i ∉ S
* Abs: S = {i | a(i)} */
class BitMapSet(N: Int) extends IntSet{
require(N >= 0) //needs range to be at least 0
private val a = new Array[Boolean](N)
for (i <- 0 until N) {a(i) = false} //initialise it to empty

/** Add elem to the test
     * pre: elem ∈ [0..N)
     * post: S = S_0 ∪ {elem} */
def add(elem : Int) : Unit = {
    require(elem >= 0 && elem < N) 
    a(elem) = true //set the element to be true i.e. add it in
}

/** Does the set contain elem?  
     * pre: elem ∈ [0..N)
     * post: S = S_0 ∧ returns elem ∈ S */
def contains(elem : Int) : Boolean = {
    require(elem >= 0 && elem < N)
    a(elem) 
}

/** Remove elem from the set.
     * pre: elem ∈ [0..N)
     * post: S = S_0 - {elem} */
def remove(elem : Int) : Unit = {
    require(elem >= 0 && elem < N)
    a(elem) = false 
}

/** The size of the set.
     * post: S = S_0 ∧ returns |S| */
def size : Int = {
    var count = 0 //count the number of trues in the list
    var i = 0
    //Inv: count = num. of tries in a[0..i), 0 <= i <= N
    //Pre-condition of Inv: a[0..0) is empty, so count = 0
    //Var: N - i
    while (i < N){
    if (a(i)) count += 1
    i += 1
    }
    //Inv ∧ ¬(i < N) -> count = num of tries in a[0..N) = |S| -> post
    count
}
}
//State: bag : Int -> Int
//Init: bag is empty i.e. ∀ x ∈ [0..MAX), bag(x) = 0
trait Bag{
    /** Adds an integer into the bag
         * pre: num ∈ [0..MAX)
         * post: bag = bag_0 + {num} */
    def add(num : Int) : Unit

    /** Returns the number of copies of an integer in the bag
         * pre: num ∈ [0..MAX)
         * post: bag = bag_0 ∧ returns bag(num) */
    def retreive(num : Int) : Int
    }
    val MAX = 20
    object BagArray extends Bag{
    private val c = new Array[Int](MAX)
    /** Abs: bag = (i, c(i) times | i ∈ [0..MAX))
         * DTI: c(i) >= 0 ∀ i ∈ [0..MAX) */

    /** Adds an integer into the bag
         * pre: num ∈ [0..MAX)
         * post: bag = bag_0 + {num} */
    def add(num : Int) : Unit = {
        require(num >= 0 && num < MAX)
        c(num) += 1
    }

    /** Returns the number of copies of an integer in the bag
         * pre: num ∈ [0..MAX)
         * post: bag = bag_0 ∧ returns bag(num) */
    def retreive(num : Int) : Int ={
        require(num >= 0 && num < MAX)
        c(num)
    }
}

/** Sorts an array where values are all between 0 and MAX
* post: returns b[0.. N), a sorted permutation of arr */
def countingSort(arr : Array[Int]) : Array[Int] ={
    var i = 0
    val N = arr.length
    val bag = BagArray //the bag we will use to store in
    while(i< N){bag.add(arr(i)); i += 1} //add every element of array to the bag

    val b = new Array[Int](N)
    var curr = 0 //holds the number of values of i stored in b
    i = 0 
    var j = 0 //pointer for b
    //Inv: b[0..j) is ordered and contains the values in [0..i)  ∧ 0 <= j <= N ∧ 0 <= i <= MAX
    //Pre-condition of Inv: b[0..j) is empty so it is true trivially
    //Var: N - j 
    while(j < N){
        if (curr == bag.retreive(i)){
        curr = 0
        i += 1
        }
        else {
        b(j) = i
        curr += 1
        j += 1
        }
    }
    //Inv ∧ ¬(j < N) -> j = N ∧ b[0..j) is ordered and contains values in [0..i) ∧ 0 <= i <= MAX -> b[0..N) is ordered and contains values in [0..MAX) -> post
    b
}
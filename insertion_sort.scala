def search(a : Array[Int], x: Int) : Int = {
  val N = a.size 
  var i = 0; var j = N
  while (i < j){
    val m = (i + j)/2
    if (a(m) < x) i = m + 1 else j = m 
  }
  i
}
  
/** Sorts an array a[0..N) by insertion sort
  * Post: a[0..N) is sorted in increasing order*/ 
def insertion_sort(a : Array[Int]) : Array[Int] = {
  var i = 1
  val N = a.length
  //Inv1 : a[0..i) is in increasing order, 1 <= i <= N
  //Var1: N - i
  //Pre-condition of Inv1 : a[0..1) is a singleton so must be increasing
  while (i < N){
    var x = a(i)
    var j = search(a.slice(0, i), a(i))
    var k = i 
    //Inv2: a[k+1 .. i+1) contains all elements greater than a(i), j <= k <= i
    //Var2: j -  k
    //Pre-condition of Inv2: a[i+1 .. i+1) is empty so trivially true
    while (k > j){
      a(k) = a(k-1)
      k -= 1
    }
    //Inv2 ∧ ¬(k > j) -> Inv2 ∧ k = j -> a[j+1 ... i+1) contains all elements > a(i)
    a(j) = x
    i += 1
  }
  //Inv1 ∧ ¬(i < N) -> a[0..i) is in increasing order ∧ i = N -> a[0..N) is increasing order -> post-condition
  a
}
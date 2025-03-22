/** Divides the array segment a[l..r) into 3 sub-arrays - less than pivot, equal to pivot, more than pivot.
  * Post: returns (i, j) s.t a[l..i) < pivot, a[i..j) = pivot, a[j..r) > pivot */
def partitionAdvanced(l : Int, r : Int) : (Int, Int) = {
  val pivot = a(l)
  var i = l //position of less than section
  var j = l + 1 //position of equal to section
  var k = r //position of more than section
  //Inv:  a[l..i) < pivot && a[i..j) = pivot, and a[k..r) > pivot. l <= i < j <= k <= r
  //Var: k - j 
  //Pre-condition of Inv: a[l..i) is empty so trivially less than pivot. a[i..j) = a[l..l+1) = pivot, a[k..r) is empty so trivially > pivot
  while (j < k){
    if (a(j) == pivot){
      j += 1 //if equal to just move equal pointer
    }
    else if (a(j) > pivot){
      val t = a(j) //switch into upper array
      a(j) = a(k - 1)
      a(k-1) = t
      k -= 1 //decrement upper array pointer
    }
    else {
      val t = a(j) //switch into lower subarray
      a(j) = a(i)
      a(i) = t
      i += 1 //increment pointers for upper and lower subarrays
      j += 1
    }
  }
  //Inv && ¬(j < k) -> Inv && j = k -> a[l..i) < pivot && a[i..j) = pivot, a[j..r) > pivot -> post
  (i, j)
}

/** Sorts a[l..r) using quick-sort in place */
def qsortAdvanced(l : Int, r : Int) : Unit = {
  if (r-1 > l) { //nothing to do if empty or singleton
    val (i,j) = partitionAdvanced(l, r) //divide
    qsortAdvanced(l,i); qsortAdvanced(j,r) //sort each segment accordingly
  }
}
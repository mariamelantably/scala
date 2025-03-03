package intset
// A class of objects to represent a set
class IntSet{
  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)

  // Init: S = {}
  private var theSet : Node = Node(-1, null) // or however empty set is represented

  def stringify(x : Node) : String = {
    var y = x
    if (y.datum == -1){return "{}"}
    var string : String = "{"
    while (y.datum != -1){
        string = string + y.datum.toString + ", "
        y = y.next
    }
    string = string.dropRight(2)
    return (string + "}")
  }

  /** Convert the set to a string.
    * (The given implementation is not sufficient.) */
  override def toString : String = { //will build the linked list in a way that it won't have repeats
    var y = theSet
    if (theSet.datum == -1){return "{}"}
    var string : String = "{"
    while (y.datum != -1){
        string = string + y.datum.toString + ", "
        //println(y.datum)
        y = y.next
        //println(stringify(y.next))
    }
    string = string.dropRight(2)
    return (string + "}")
  }
  private def find_position(e : Int) : Node = {
    var x = theSet
    while (x.next.datum != -1 && x.next.datum < e){
            x = x.next
        }
    return x

  }
  /** Add element e to the set
    * Post: S = S_0 U {e} */
  def add(e: Int) : Unit = { //store them in ascending order
    var newNode = Node(e, null)
    if (theSet.datum == -1) {
        newNode.next = theSet
        theSet = newNode
    }
    else {
      var x = theSet
      if (e < x.datum) {
        newNode.next = theSet
        theSet = newNode
      }
      else {
        x = find_position(e)
        if (x.datum != e && x.next.datum != e){
            newNode.next = x.next
            x.next = newNode
        }
      }
    }
  }  

  /** Length of the list
    * Post: S = S_0 && returns #S */
  def size : Int = {
    var count = 0
    var x = theSet
    while (x.datum != -1){
        count += 1
        x = x.next
    }
    return count
  }

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  def contains(e: Int) : Boolean = {
    if (theSet.datum == -1) {return false}
    var x = find_position(e)
    return (x.datum == e || x.next.datum == e)
  }

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Int = {
    return theSet.datum 
  }

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  override def equals(that: Any) : Boolean = that match {
    case s: IntSet => {
        var y = that.asInstanceOf[IntSet]
        return this.toString == y.toString 
    }
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean = {
    //case 1 - empty list
    if (theSet.datum  == -1){return false}
    var removed = false
    var x = theSet
    //case 2 - it is at the start of the list 
    if (x.datum == e){
        theSet = theSet.next
        removed = true
    }
    else {
        x = find_position(e)
        if (x.next.datum == e){
            x.next = x.next.next
            removed = true
        }
    }
    removed
  }
    
  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  def subsetOf(that: IntSet) : Boolean = {
    var y = theSet 
    var subset = true 
    while (subset && y.datum != -1){
        subset = that.contains(y.datum)
        y = y.next
    }
    subset 
  }

  // ----- optional parts below here -----

  /** return union of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: IntSet) : IntSet = {
    var x = theSet
    while (x.datum != -1){
        that.add(x.datum)
        x = x.next
    }
    return that.asInstanceOf[IntSet] 
  }

  /** return intersection of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: IntSet) : IntSet = {
    var x = theSet
    var n = new IntSet
    while (x.datum != -1){
        if (that.contains(x.datum)) {
            n.add(x.datum)
        } 
        x = x.next
    }
    return n 
  }

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map(f: Int => Int) : IntSet = {
    var n = new IntSet
    var x = theSet
    while (x.datum != -1){
        n.add(f(x.datum))
        x = x.next
    }
    return n 
  }

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : Int => Boolean) : IntSet = {
    var n = new IntSet
    var x = theSet
    while (x.datum != -1){
        if (p(x.datum)) {n.add(x.datum)}
        x = x.next
    }
    return n 
  }

  /** dropWhile 
    * Post: S = S_0 && returns res s.t. res.S is the longest prefix satisfying the predicate */
  def dropWhile(p : Int => Boolean) : IntSet = {
    var n = new IntSet
    var x = theSet
    while (x.datum != -1 && p(x.datum)) {
      n.add(x.datum)
      x = x.next
    }
    return n 
  }

}


// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node)

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined 
    * the main constructor and the add operation. 
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}
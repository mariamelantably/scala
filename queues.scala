/** A queue of data of type A. 
* state: q:seqA
* init: q=[] */
trait Queue[A]{
    /** Add x to the back of the queue
         * post: q = q0 ++ [x]  */ 
    def enqueue(x: A) : Unit

    /** Remove and return the first element.
         * pre: q ̸= []
         * post: q = tail q0 ∧ returns head q0
         * or post: returns xs.t.q0 =[x]++q */
    def dequeue(): A

    /** Is the queue empty?
         * post: q=q0 ∧returns q=[] */
    def isEmpty: Boolean
}

class ArrayQueue extends Queue[Int]{
    val MAX = 100 //max number of pieces of data 
    val data = new Array[Int](MAX)
    var topPointer = 0
    var bottomPointer = 0
    var count = 0
    /** topPointer represents the posititon of the front of the queue (where to remove)
         * bottomPointer represents the position of the back of the queue (where to add to)
         * Abs: queue = seq(data[topPointer..bottomPointer) iff bottomPointer >= topPointer or data[topPointer ..MAX) ++ data[0..BottomPointer) iff bottomPointer < topPointer 
         * DTI: count = |topPointer - bottomPointer|, 0 <= topPointer, bottomPointer < MAX */

    /** Add x to the back of the queue
         * post: returns !(q.isFull) ∧  (q = q0 ++ [x] <-> !(q.isFull)) */ 
    def enqueue(x: Int) : Boolean ={
        if (count < MAX){
        data(bottomPointer) = x
        bottomPointer = (bottomPointer + 1)%MAX
        count += 1
        }
        false
    }

    /** Remove and return the first element.
         * pre: q ̸= []
         * post: q = tail q0 ∧ returns head q0 */
    def dequeue(): Int = {
        require(count != 0)
        val t = topPointer
        topPointer = (topPointer + 1)%MAX
        count -= 1
        data(t)
    }

    /** Is the queue empty?
         * post: q=q0 ∧returns q=[] */
    def isEmpty: Boolean = count == 0

    /** Is the queue full?
         * post: q = q0 ∧ returns |q| = MAX */
    def isFull : Boolean = count == MAX
}

/** Implements the queue is a tailed linked list
  * state: q = seq(Int)
  * init: q = {} */ 
class IntQueue extends Queue[Int]{
    private var queueTop = new IntQueue.Node(-1, null) //points to the front of the queue
    //first node is a dummy headerm so stores no data
    private var queueEnd = queueTop //points to the end of the queue
    /** Abs: q = {n.number} where n is a node reachable from queueTop.next by any number of nodes
         * DTI: the elements in the linked list are finite ∧ queueTop points to an element before or same element as queueEnd */

    // toString function used for debugging
    override def toString : String = {
        if (queueTop.next == null) return ""
        var s = queueTop.next
        var st = ""
        while(s.next != null){
        st = st+ s.number + " -> "
        s = s.next
        }
        st = st  + s.number
        st
    }

    /** Add x to the back of the queue
         * post: q = q0 ++ [x]  */ 
    def enqueue(x: Int) : Unit = {
        queueEnd.next = new IntQueue.Node(x, null)
        queueEnd = queueEnd.next
    }

    /** Remove and return the first element.
         * pre: q ̸= []
         * post: q = tail q0 ∧ returns head q0 */
    def dequeue(): Int = {
        require(!(this.isEmpty))
        val t = queueTop.next.number
        queueTop.next = queueTop.next.next
        return t
    }

    /** Is the queue empty?
         * post: q=q0 ∧returns q=[] */
    def isEmpty: Boolean = queueTop.next == null

}

object IntQueue{
    private class Node(var number:Int, var next:Node)
}

/** state: q = seq Int
* init: q = [] */
class DoubleEndedQueue{
    private var myList : DoubleEndedQueue.Node = null
    private var count = 0
    private var myListEnd = myList
    /** myListEnd points to the end of the list, myList points to the start
         * Abs: L(myList) = seq(n.datum), where n is a node reachable from myList in a finite number of steps
         * DTI: the elements in the linked list are finite ∧ count >= 0 ∧ myList points to the same element or an element before myListEnd */

    // toString function used for debugging 
    override def toString : String = {
        if (this.isEmpty) return ""
        var s = myList
        var st = ""
        while(s.next != null){
        st = st+ s.datum + " -> "
        s = s.next
        }
        st = st  + s.datum
        st
    }

    /** Is the queue empty? 
         * post: q = q0 ∧ returns q = [] */
    def isEmpty : Boolean = count == 0

    /** Add x to the start of the queue
         * post: q = x:q0 */
    def addLeft(x : Int) : Unit = {
        if (this.isEmpty){ //empty list - add to front
        myList = new DoubleEndedQueue.Node(x, null, null)
        myListEnd = myList //re-set the end pointer
        }
        else{
        var newNode = new DoubleEndedQueue.Node(x, null, null) //create a newnode
        myList.prev = newNode //set the previous pointer to newNode
        newNode.next = myList //set newNode to point to the rest of the list
        myList = newNode //append new node to the front
        }
        count += 1 //increment count
    }

    /** Get and remove element at the start of the queue
         * pre: q != []
         * post: returns head(q0) ∧ q = tail(q0) */
    def getLeft() : Int = {
        require(!this.isEmpty)
        val t = myList.datum
        myList = myList.next 
        count -= 1
        return t
    }

    /** Add x to the end of the queue
         * post: q = q0 ++ [x] */
    def addRight(x : Int) : Unit = {
        var newNode = new DoubleEndedQueue.Node(x, myListEnd, null)
        myListEnd.next = newNode //change next-pointer to newNode
        myListEnd = newNode //change the end to newNode
        count += 1
    }

    /** Get and remove element at the end of the queue
         * pre: q != []
         * post: returns last(q0) ∧ q = init(q0) */
    def getRight() : Int = {
        require(!this.isEmpty)
        count -= 1
        val t = myListEnd.datum //get data at the end of the list
        myListEnd = myListEnd.prev //override the end 
        myListEnd.next = null //ensure end-pointer has next = null
        return t
    } 
}

object DoubleEndedQueue{
    private class Node(var datum : Int, var prev : Node, var next : Node)
}
/** The state of a phone book, mapping names (Strings) to numbers (also * Strings).
* state: book : String → String
* init: book = {} */
trait Book{
    /** Add the maplet name -> number to the mapping.
         * post: book = book0 ⊕ {name → number} */ 
    def store(name: String, number: String) : Unit

    /** Return the number stored against name.
         * pre: name ∈ dom book
         * post: book = book0 ∧ returns book(name) */
    def recall(name: String) : String

    /** Is name in the book?
         * post: book = book0 ∧ returns name ∈ dom book */
    def isInBook(name: String) : Boolean

    /** Delete the number stored against a name (if it exists)
         * post: returns (name ∈ dom book) ∧ (if name ∈ dom book, book = book0 - {name -> number} else book = book0) */
    def delete(name : String) : Boolean
}

//Part (b)
// Representing the phone book using a pair of arrays
object ArraysBook extends Book{
    private val MAX = 1000 // max number of names we can store
    private val names = new Array[String](MAX)
    private val numbers = new Array[String](MAX)
    private var count = 0
    // These variables together represent the mapping
    // Abs: book = 
    // { names(i) -> numbers(i) | i <- [0..count) }
    // DTI: 0 <= count <= MAX && 
    // entries in names[0..count) are distinct

    // Return the index i<count s.t. names(i) = name; or 
    //              return count if no such index exists
    private def find(name: String) : Int = {
        // Invariant: name not in names[0..i) && i <= count
        var i = 0
        while(i < count && names(i) != name) i += 1
        i
    }

    /** Return the number stored against name */
    def recall(name: String) : String = {
        val i = find(name)
        assert(i < count)
        numbers(i)
    }

    /** Is name in the book? */
    def isInBook(name: String) : Boolean = find(name) < count

    /** Add the maplet name -> number to the mapping */
    def store(name: String, number: String): Unit = {
        val i = find(name)
        if(i == count){
        assert(count < MAX); names(i) = name; count += 1
        }
        numbers(i) = number
    }

    /** Delete the number stored against a name (if it exists)
         * post: returns (name ∈ dom book) ∧ (if name ∈ dom book, book = book0 - {name -> number} else book = book0) */
    def delete(name : String) : Boolean = {
        if (!this.isInBook(name)) return false //no need to check if we know its not in the book

        var i = find(name) //find its location. i != count by above statement
        //switch it with final entry, decrement count
        names(i) = names(count-1); numbers(i) = numbers(count-1)
        count -= 1
        true
    }
        
}

//Question 8
object OrderedPhoneBook extends Book{
    private val MAX = 1000 // max number of names we can store
    private val names = new Array[String](MAX)
    private val numbers = new Array[String](MAX)
    private var count = 0
    // These variables together represent the mapping
    // Abs: book = 
    // { names(i) -> numbers(i) | i <- [0..count) }
    // DTI: 0 <= count <= MAX && 
    // entries in names[0..count) are distinct

    /** does a binary search to find position needed
         * post: returns i s.t. a[0..i) < x <= a[i..count) */
    private def search(x : String) : Int = { 
        var i = 0; var j = count
        //Inv: names[0..i) < x ∧ names[j..count) >= x ∧ 0 <= i <= j <= count
        //Pre-condition of Inv: names[0..i) and names[j..count) are empty -> trivially true
        //Var: j - i
        while (i < j) {
        val m = (i + j)/2
        if (names(m) < x) i = m + 1 else j = m
        }
        //Inv ∧ ¬(i < j) -> a[0..i) < x <= a[j..count) ∧ i= j -> a[0..i) < x <= a[i..count) -> post
        i 
    }

    /** Return the number stored against name.
         * pre: name ∈ dom book
         * post: book = book0 ∧ returns book(name) */
    def recall(name: String) : String = {
        val i = search(name) 
        numbers(i)
    }

    /** Is name in the book?
         * post: book = book0 ∧ returns name ∈ dom book */
    def isInBook(name: String) : Boolean = names(search(name)) == name

    /** Add the maplet name -> number to the mapping.
         * post: book = book0 ⊕ {name → number} */ 
    def store(name: String, number: String): Unit = {
        val i = search(name) //find its position
        if(names(i) != name){
        assert(count < MAX)
        var j = i 
        //Inv: names[i+1 ... j+1) = names_0[i..j) ∧ numbers[i+1 ... j+1) = numbers_0[i..j) ∧ i <= j <= count
        //Pre-condition of Inv: names[i+1 .. j+1) and numbers[i+1 .. j+1) are empty so trivially true
        //Var: count - j
        while (j < count){
            names(j+1) = names(j)
            numbers(j+1) = numbers(j)
            j += 1
        }
        //Inv ∧ ¬(j > count) -> names[i+1 .. j+1) = names_0[i..j) ∧ numbers[i+1...j+1) ∧ j = count -> names[i+1 ... count +1) = names_0[i..count) ∧ numbers[i+1 .. count + 1) = numbers_0[i..count)
        names(i) = name //insert new name
        count += 1 //increment count
        }
        numbers(i) = number //update number
    }

    /** Delete the number stored against a name (if it exists)
         * post: returns (name ∈ dom book) ∧ (if name ∈ dom book, book = book0 - {name -> number} else book = book0) */
    def delete(name : String) : Boolean = {
        if (!this.isInBook(name)) return false //no need to check if we know its not in the book

        var i = search(name) //find its location. i != count by above statement
        //by above, we know names(i) == name
        //Inv: names[search(name) .. i) = names_0[search(name) + 1 .. i + 1) ∧ numbers[search(name) .. i) = numbers_0[search(name) + 1 .. i + 1) ∧ search(name) <= i < count
        //Pre-condition of Inv: names[search(name)..i) and numbers[search(name)..i) are empty so trivially true
        //Var: count - 1 - i
        while (i < count - 1){
        names(i) = names(i + 1) //shift each element down
        numbers(i) = numbers(i + 1)
        i += 1
        }
        //Inv ∧ ¬(i < count - 1) -> names[search(name) .. i) = names_0[search(name) + 1 .. i + 1) ∧ numbers[search(name) .. i) = numbers_0[search(name) + 1 .. i + 1) ∧ i = count - 1
        // -> names[search(name) .. count -1) = names_0[search(name) + 1 .. count) ∧ numbers[search(name) .. count -1) = numbers_0[search(name) + 1 .. count) -> post 
        count -= 1
        true
    }
}

//stores phone book as a linked list with a dummy header
class LinkedListHeaderBook extends Book{
    private var list = new LinkedListHeaderBook.Node("?", "?", null)
    // list represents the mapping composed of (n.name -> n.number)
    // maplets, when n is a node reached by following 1 or more 
    // next references.

    /** Return the node before the one containing name.
         * Post: book = book_0 && returns n s.t. n in L(list) &&
         * (n.next.name=name or n.next=null if no such Node exists)*/
    private def find(name:String) : LinkedListHeaderBook.Node = {
        var n = list
        // Invariant: name does not appear in the nodes up to and  
        // including n; i.e., 
        // for all n1 in L(list.next, n.next), n1.name != name
        while(n.next != null && n.next.name != name) n = n.next
        n
    }


    /** Add the maplet name -> number to the mapping 
         * Post: n = n_0 ⊕ {n.name -> n.number} */
    def store(name: String, number: String): Unit = {
        val n = find(name)
        if(n.next == null){ // store new info in current list header
        n.next = new LinkedListHeaderBook.Node(name, number, null)
        }
        else n.next.number = number
    }
}

  // Companion object
object LinkedListHeaderBook{
    private class Node(var name:String, var number:String, var next:Node)
}

//Question 3
/** State: list = {n.name -> n.number}
  * Init: list = {}  */
//Stores as a sorted linked list in order of names
class LinkedListSortedBook extends Book{
    private var list = new LinkedListSortedBook.Node("?", "?", null) //Initialises as empty with dummy header

    /** Abs: L(list) = {n.name -> n.number}, where n is a node reachable from list.next by a finite number of steps
         * DTI: L(list) is finite and increasing lexiographic order of names, and each name in L(list) is distinct */

    /** Return the node before the one containing name.
         * Post: book = book_0 && returns n s.t. n in L(list) &&
         * (n.next.name>=name or n.next=null if no such Node exists)*/
    private def find(name:String) : LinkedListSortedBook.Node = {
        var n = list
        // Inv: for all n' in L(list, n), n'.name < name
        while(n.next!= null && n.next.name < name) n = n.next
        n
    }

    // toString function, used for debugging purposes
    override def toString : String = {
        if (list.next == null) return ""
        var s = list.next
        var st = ""
        while(s.next != null){
        st = st + s.name + " " + s.number + " -> "
        s = s.next
        }
        st = st + s.name + " " + s.number
        st
    }

    /** Add the maplet name -> number to the mapping
         * post: list = list_0 ⊕ (n.name -> n.number) */
    def store(name: String, number: String): Unit = {
        val n = find(name)
        //case 1: add to the end of the array
        if (n.next == null) n.next = new LinkedListSortedBook.Node(name, number, null)
        else if (n.next.name == name) n.next.number = number //case 2: name already in array, override number
        else{
        n.next = new LinkedListSortedBook.Node(name, number, n.next) //case 3: n.next.name > name, so name should go between the nodes
        }
    }


    /** Checks if given name is in book
         * post: list = list_0 ∧ returns name ∈ L(list) */
    def isInBook(name : String) = find(name).next.name == name

    /** Deletes a name from the book
         * post: list = list_0 - {n.name -> n.number} if name ∈ L(list), list = list_0 otherwise */
    def delete(name : String) : Unit = {
        val n = find(name)
        if (n.next != null && n.next.name == name) n.next = n.next.next
    }
}


object LinkedListSortedBook{
    private class Node(var name:String, var number:String, var next:Node)
}
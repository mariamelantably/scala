/** State: map = {table.word -> table.count}
  * Init: map = {} */
class HashBagProbe{
    private val MAX = 100
    private val table = new Array[(String, Int)](MAX) //This table stores the values

    /** Abs: map ={n.word -> n.count | n ∈ table, n != null}
      * DTI: table is finite, and all the words in the table are distinct, number of non-null entries <= MAX*/


    private def hash(key : String) : Int = (key.hashCode()%MAX) //Uses built-in function to compute hashcode

    /** Returns the position of a word in the table
         * Post: table = table_0 ∧ returns i s.t. table(i)._1 = word, i == -1 if no such i exists */
    private def find(word : String) : Int = { 
        if (table(hash(word))._1 == word) return hash(word) //check hash position
        else {
        var i = (hash(word) + 1)%MAX //check linearly in the rest of the array
        //Inv: table[hash(word)..i)._1 != word && (if i < hash(word) table[0..i)._1 != word) && 0 <= i <= MAX
        //Pre-condition of Inv: trivially true as the intervals only contain hash-word, which we know is not equal to word
        while (i != hash(word) && table(i)._1 != word) i = (i+1)%MAX
        
        //Inv ∧ ¬(i != hash(word) && table(i)._1 !- word)
        //Case 1 -> Inv ∧ i == hash(word) -> word not in table -> return -1 -> post
        //Case 2 -> Inv ∧ table(i)._1 == word -> post

        if (i == hash(word)) return -1
        else (return i)
        }
    } 

    /** Finds the count of a word
         * Pre: word ∈ table
         * Post: tables = table_0 ∧ returns count associated with the word (in its pair) */
    def count(word : String) : Int = {
        require(find(word) != -1)
        return table(find(word))._2
    }

    /** Adds a word to the table
         * Post: table = table_0 if the table is full, else table = table_0 ⊕ {table.word -> table.count + 1 if table.word ∈ table, else table.word -> 1} */
    def add(word : String) : Unit = {
        val n = find(word)
        if (n != -1) table(n) = (word, table(n)._2 + 1) //Already in the table, just incremenet the count
        else {
        val h = hash(word) //find the hash value
        if (table(h) != null){ //the hashed space is not free, search linearly around it 
            var i = (h + 1)%MAX

            //Inv: table[hash(word)..i) != null && (if i < hash(word) table[0..i)._1 != null) && 0 <= i <= MAX
            //Pre-condition of Inv: trivially true as the interval only contains table[(hash(word))], which we know is not null
            while (table(i) != null && i != h) i = (i + 1)%MAX
            //Inv ∧ ¬(table(i) != null && i != h) 
            //Case 1: table(i) == null -> insert at this position
            //Case 2: i == h -> no null spaces -> table full

            if (i == h){println("table full")}
            else {table(i) = (word, 1)}
        }
        else table(h) = (word, 1) //the hash space is free
        }
    }

    /** Deletes one instance of a word from the bag
         * Pre: word ∈ table
         * Post table = If count(word) > 1, then table = table_0 ⊕ {table.word -> table.count - 1}. Else, table = table_0 - {table.word -> table.count}, 
         * and we fill this gap with another element that hashes to its index (or null if no element exist)*/
    def delete(word : String) : Unit = {
        require(find(word) != -1)
        table(find(word)) = (word, table(find(word))._2 - 1) //decrement the count
        if (count(word) == 0){
        //this entry needs to be fully deleted, so we find a replacement for it (if one exists), and set to null
        //a replacement is another value which hashes to that position -> prevents unnecessary gaps & makes finding simpler
        var i = (find(word) + 1)%MAX

        //Inv: hash values of table[find(word)..i) != find(word) && (if i < find(word), hash values of table[0..i) != find(word)) && 0 <= i <= MAX
        //Pre-condition of Inv: trivially true as it contains only table[find(word)], which has been deleted
        while (hash(table(i)._1) != find(word) && i != find(word)) i = (i + 1)%MAX
        //Inv ∧ ¬(hash(table(i)._1) != find(word) && i != find(word))
        //Case 1: hash(table(i)._1) == find(word) -> plug this spot with table(i)
        //Case 2: i == find(word) -> no such spot to plug -> just leave it as null

        if (i == find(word)) table(i) = null //no other potential filler
        else {
            table(find(word)) = table(i)
            table(i) = null
        }
        }
    }
}
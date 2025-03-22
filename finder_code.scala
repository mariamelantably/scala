class Dictionary(fname: String){
    /** A Set object holding the words - taken from lectures */
    private val words = new scala.collection.mutable.HashSet[String]

    /** Initialise dictionary with all words from file fname */
    private def initDict(fname: String) = {
        val allWords = scala.io.Source.fromFile(fname).getLines()
        // Should word w be included?
        def include(w:String) = w.forall(_.isLower)
        for(w <- allWords; if include(w)) words += w
        // println("Found "+words.size+" words")
    }

    // Initialise the dictionary now (on constructing this object)
    initDict(fname)

    /** @return <code>true</code> if w is in the dictionary */
    def isWord(w: String) : Boolean = words.contains(w)
}

val ourDict = new Dictionary("/Users/mariamelantably/Desktop/programs/scala/anagram_finder/knuth_words.txt")

/** Inserting char into all positions of string
* Post: returns list s.t. char has been inserted into every position from 0 to string.length of original string */
def insert(char : Char, string : String) : List[String] = {
    var list = List[String]()
    var i = 0
    //Inv: list contains all new strings with char inserted at positions [0..i), 0 <= i <= string.length + 1
    //Pre-condition of Inv: i = 0 -> list empty -> no positions yet inserted -> trivially true
    //Var: string.length + 1 - i 
    while (i <= string.length) {
        val newWord = string.slice(0, i) ++ char.toString ++ string.slice(i, string.length) //insert at position i
        list = newWord::list //cons to the list
        i += 1 //move to next index
    }
    //Inv ∧ ¬(i <= string.length) -> Inv ∧ i = string.length + 1 -> list contains all new strings with char inserted at positions [0..string.length + 1) -> post
    return list
}


/** Generates permutations
* Post: returns list of all permutations of string */
def permutations(string : String) : List[String] = {
    var list = List[String]()
    var i = 0
    //Inv1: list contains all possible permutations of string[0..i), 0 <= i <= string.length
    //Pre-condition of Inv1: string[0..i) is empty and so is list -> trivially true
    //Var1: string.length - 1
    while (i < string.length){
        var current_char = string(i) //which character we want to insert
        if (list.length == 0){ // special case for first character
        list = current_char.toString::list //if 1 character, only 1 permutation (itself)
        } else {
        var newList = List[String]() //create newList to store new permutations
        var j = 0 //iterate through all the permutations, and run insert
        //Inv2: newList contains current_char inserted in all possible positions of all strings in list[0..j), 0 <= j <= string.length
        //Pre-condition of Inv2: newList is empty and so is list[0..j) -> trivially true
        //Var2: list.length - j
        while (j < list.length){
            newList = insert(current_char, list(j)) ++ newList
            j += 1
        }
        //Inv2 ∧ ¬(j < list.length) -> Inv2 ∧ j = list.length -> newList contains current_char inserted in all possible positions of all strings in list
        // -> newList contains all possible permutations of string[0..i]
        list = newList
        }
        i += 1
    }
    //Inv1 ∧ ¬(i < string.length) -> Inv1 ∧ i = string.length -> list contains all possible permutations of string -> post
    return list
}

def bruteForceAnagrams(word : String) : List[String] = {
    val perms = permutations(word)
    var anags = List[String]()
    var i = 0
    //Inv: anags contains all possible anagrams in perms[0..i), 0 <= i <= perms.length
    //Pre-condition of Inv: perms[0..i) is empty and so is anags -> trivially true
    //Var: perms.length - i
    while (i < perms.length){ 
        if (ourDict.isWord(perms(i))) anags = perms(i)::anags
        i += 1
    }
    //Inv ∧ ¬(i < perms.length) -> Inv ∧ i == perms.length -> anags contains all possible anagrams in perms[0..perms.length) -> contains all possible anagrams -> post
    anags
}

//This has a factorial time complexity, which is very slow for larger numbers

//Create a new anagram dictionary
/** State: {(w', w)} where w' is w sorted lexiographically, w ∈ file(name)
* Init: contains all words in fname */
class AnagramDictionary(fname : String){
    var words = new Array[(String, String)](10000000)
    /** Abs: dict - {w | (_, w) ∈ words}
         * DTI: elements of the dictionary are distinct and in the lexiographic order of their sorted permutation */
    private def initDict(fname: String) = {
        val allWords = scala.io.Source.fromFile(fname).getLines()
        var i = 0 
        // Should word w be included?
        def include(w:String) = w.forall(_.isLower)
        for(w <- allWords; if include(w)){
        words(i) = (w.sorted, w) //put the sorted and the non-sorted version as a pair
        i += 1
        }
        words = words.slice(0, i)
        words = words.sorted
    }

    // Initialise the dictionary now (on constructing this object)
    initDict(fname)
}

val ourDict2 = new AnagramDictionary("/Users/mariamelantably/Desktop/programs/scala/anagram_finder/knuth_words.txt")


def dictionaryAnagrams(word : String) : List[String] = {
    val targ = word.sorted
    var list = List[String]()
    var i = 0
    //Inv: list contains all the anags of word in ourDict2.words[0..i) ∧  0 <= i <= ourDict2.words.length
    //Pre-condition of Inv: ourDict2.words[0..i) is empty and so is list -> trivially true
    //Var: ourDict2.words.length - i
    while (i < ourDict2.words.length){
        if (ourDict2.words(i)._1 == targ) {
        list = ourDict2.words(i)._2::list
        }
        i += 1
    }
    //Inv ∧ ¬ (i < ourDict2.words.length) -> Inv ∧ i = ourDict2.words.length -> list contains all the anags of word in ourDict2.words -> post
    list
}
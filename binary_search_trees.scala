//Part (a)
/** Post: prints the tree in prefix order, with each node on a new line and indents representing the depth */
var depth = 0;
def printRecursivePrefix(tree : Tree) : Unit = {
    if (tree != null){
        println(". "*depth + tree.word) //print the root
        depth += 1 //increment depth
        printRecursivePrefix(tree.left) //print left sub-tree
        printRecursivePrefix(tree.right) //print right sub-tree
        depth -= 1 //decrement length
    }
    else {
        println(". "*depth + "null")
    }
}

//Part (b) 
/** Prefix order printing
* Post: tree = tree_0 ∧ prints the tree in prefix order, with each node on a new line and indents representing the depth */
def printIterativePrefix(tree : Tree) : Unit = {
    var t = tree
    val stack = new scala.collection.mutable.Stack[(Tree, Int)]
    var d = 0 //we store each tree with its depth, to allow quick printing
    //Inv: We still need to print t; for each t' in the stack, we have reached null for this tree, and need to print null for it.
    while (t != null || !stack.isEmpty) {
        if (t != null) { //not null, when you meet it print it
        println(". "*d + t.word)
        d += 1
        stack.push((t, d))
        t = t.left
        }
        else { 
        val (t1, d1) = stack.pop()
        println(". "*d + "null")
        d = d1
        t = t1.right
        }
    }
    //Inv ∧ ¬(t != null || !stack.isEmpty) -> Inv ∧ t is null ∧ stack is empty -> everything has been printed
}

//Question 5
case class Tree(var word : String, var left : Tree, var right : Tree)
var tr = Tree("three", Tree("four", Tree("five",null,null), Tree("six",
Tree("seven", Tree("one",null,null), null), null)), Tree("two",null,null))

/** Flips a tree destructively
* Post: throughout the entire tree, the left and right subtrees have been swapped */
def flip(t : Tree) : Unit = {
    var tr = t
    val stack = new scala.collection.mutable.Stack[Tree]

    //Inv: we still need to flip t, and for each t' in the stack, we still need to flip its left and right sub-trees, and flip the originally right-subtree.
    while (tr != null || !stack.isEmpty) {
        if (tr != null) {stack.push(tr); tr = tr.left}
        else {
        val t1 = stack.pop()
        val temp = t1.left //flip left and right sub-trees
        t1.left = t1.right 
        t1.right = temp
        tr = t1.left //move to left-subtree (originally the right sub-tree)
        }
    }
    //Inv ∧ ¬(tr != null || !stack.isEmpty) -> tr == null ∧ stack.isEmpty ∧ Inv -> entire tree has been reversed
}

//Question 6
case class WordTree(var word : String, var count : Int, var left : WordTree, var right : WordTree)

//Part (a)
/** Sums the tree count values
* Post: tree = tree_0 ∧ returns the sum of all count values on the nodes of the trees */
def recursiveSum(tree : WordTree) : Int = {
    if (tree != null) return recursiveSum(tree.left) + tree.count + recursiveSum(tree.right)
    else return 0
}

//Part (b)
/** Sums the tree count values
* Post: tree = tree_0 ∧ returns the sum of all count values on the nodes of the trees */
def iterativeSum(tree : WordTree) : Int = {
var t = tree
val stack = new scala.collection.mutable.Stack[WordTree]
var sum = 0
//Inv: we still need to sum t; for all t' in the stack, we still need to sum their counts and the counts in their right sub-trees.
while (t != null || !stack.isEmpty) {
    if (t != null) {stack.push(t); t = t.left} //explore left sub-trees
    else {
    val t1 = stack.pop() //pop from stack
    sum += t1.count //add count
    t = t1.right //move to right sub-tree
    }
}
//Inv ∧ ¬(t != null || !stack.isEmpty) -> Inv ∧ t== null ∧ stack.isEmpty -> we have finished summing -> post
sum
}
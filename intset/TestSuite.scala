/**  With Scala 2.12 on Lab machines:

 * In normal circumstances the CLASSPATH is already set for you:

fsc TestTest.scala
scala org.scalatest.run TestTest

 * If you use jar files in your own space:

fsc -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar TestTest.scala
scala -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar org.scalatest.run TestTest

 * (Once this is working you can set your CLASSPATH in .bashrc) 
*/
import intset.IntSet 
import org.scalatest.funsuite.AnyFunSuite

class AddingTest extends AnyFunSuite {
  var mySet = new IntSet
  test("empty set"){
    assert(mySet.toString === "{}")
  }
  test("adding an element"){
    mySet.add(2)
    assert(mySet.toString === "{2}")
  }
  test("adding a second element to the end"){
    mySet.add(4)
    //mySet.add(3)
    assert(mySet.toString === "{2, 4}")
  }
  test("adding in the middle"){
    mySet.add(3)
    assert(mySet.toString === "{2, 3, 4}")
  }

  test("adding to the front"){
    mySet.add(1)
    assert(mySet.toString === "{1, 2, 3, 4}")
  }

  test("adding an already-present element to the front"){
    mySet.add(1)
    assert(mySet.toString === "{1, 2, 3, 4}")
  }

  test("adding an already-present element to the middle"){
    mySet.add(3)
    assert(mySet.toString === "{1, 2, 3, 4}")
  }

  test("adding an already-present element to the end"){
    mySet.add(4)
    assert(mySet.toString === "{1, 2, 3, 4}")
  }

  test("ensuring ascending order"){
    mySet.add(7)
    mySet.add(9)
    mySet.add(23)
    mySet.add(8)
    mySet.add(16)
    mySet.add(75)
    assert(mySet.toString === "{1, 2, 3, 4, 7, 8, 9, 16, 23, 75}")
  }
}

class SizeTest extends AnyFunSuite {
  var mySet = new IntSet

  test("empty set size") {
    assert(mySet.size === 0)
  }

  test("adding elements and checking size"){
    mySet.add(1)
    mySet.add(2)
    mySet.add(3)
    mySet.add(4)
    assert(mySet.size === 4)
  }

  test("already present element doesn't affect size"){
    mySet.add(2)
    assert(mySet.size === 4)
  }
}

class ContainmentTest extends AnyFunSuite {
  test("nothing is an element of the empty set"){
    var TestingSet : IntSet = new IntSet 
    assert(TestingSet.contains(1) === false)
    assert(TestingSet.contains(2) === false)
    assert(TestingSet.contains(789) === false)
  }
  
  test("checking first element"){
    var TestingSet = IntSet(1,2,3,17,18,5,4,9,80,72)
    assert(TestingSet.contains(1) === true)

  }

  test("checking last element"){
    var TestingSet = IntSet(1,2,3,17,18,5,4,9,80,72)
    assert(TestingSet.contains(72) === true)
  }

  test("checking middle elements"){
    var TestingSet = IntSet(1,2,3,17,18,5,4,9,80,72)
    assert(TestingSet.contains(2) === true)
    assert(TestingSet.contains(789) === false)
    assert(TestingSet.contains(12) === false)
    assert(TestingSet.contains(9) === true)
  }

  test("adding changes the answer"){
    var TestingSet = IntSet(1,2,3,17,18,5,4,9,80,72)
    assert(TestingSet.contains(10) === false) 
    TestingSet.add(10)
    assert(TestingSet.contains(10) === true)
  }
}

class AnyTest extends AnyFunSuite{
  var TestingSet = new IntSet
  test("empty set"){
    assert(TestingSet.any === -1)
  }

  test("non-empty set"){
    TestingSet.add(3)
    assert(TestingSet.any === 3)
  }

  test("adding to the end"){
    TestingSet.add(5)
    assert(TestingSet.any === 3)
  }

  test("adding to the front"){
    TestingSet.add(1)
    assert(TestingSet.any === 1)
  }
}

class TestEquality extends AnyFunSuite {
  var Set1 = IntSet(1,2,3)
  var Set2 = IntSet(2,1,3)

  test("try on empty set"){
    var EmptySet = new IntSet
    assert((EmptySet == EmptySet) === true)
    assert((EmptySet == Set1) === false)
    assert((Set2 == EmptySet) === false)
  }

  test("equal to itself"){
    assert((Set1 == Set1) === true)
  }

  test("order doesn't matter"){
    assert((Set1 == Set2) === true)
  }

  test("different elements are equal"){
    Set2.add(5)
    assert((Set1 == Set2) === false)
  }

  test("other types are false"){
    assert((Set1 == 4) === false)
    assert((Set1 == "Hello") === false)
    assert((Set1 == true) === false)
  }
}

class TestRemoval extends AnyFunSuite{
  var Set1 = new IntSet
  test("removing from empty set"){
    assert(Set1.remove(1) === false)
    assert(Set1.remove(3) === false)
    assert(Set1.toString === "{}")
  }

  test("removing from a set with one element"){
    Set1.add(1)
    assert(Set1.remove(1) === true)
    assert(Set1.toString === "{}")
  }

  test("removing from the front"){
    Set1.add(3)
    Set1.add(2)
    Set1.add(18)
    Set1.add(12)
    assert(Set1.remove(2) === true)
    assert(Set1.toString === "{3, 12, 18}")
  }

  test("removing from the middle"){
    Set1.add(3)
    Set1.add(2)
    Set1.add(18)
    Set1.add(12)
    assert(Set1.remove(12) === true)
    assert(Set1.toString === "{2, 3, 18}")
  }

  test("removing from the end"){
    Set1.add(3)
    Set1.add(2)
    Set1.add(18)
    Set1.add(12)
    assert(Set1.remove(18) === true)
    assert(Set1.toString === "{2, 3, 12}")
  }

  test("removing elements not in the list"){
    Set1.add(3)
    Set1.add(2)
    Set1.add(18)
    Set1.add(12)
    assert(Set1.remove(7) === false)
    assert(Set1.remove(9) === false)
    assert(Set1.remove(17809) === false)
    assert(Set1.toString === "{2, 3, 12, 18}")
  }
}

class SubsetTest extends AnyFunSuite{
  var MySet = IntSet(2,3, 8, 1, 17, 13, 5, 4, 23)
  var EmptySet = new IntSet
  var MySet3 = IntSet(2,3, 8, 1, 17, 13, 5, 4, 23)
  test("empty set subsets"){
    assert(MySet.subsetOf(EmptySet) === false)
    assert(EmptySet.subsetOf(MySet) === true)
  }

  test("one set subsets"){
    var OneSet1 = IntSet(2)
    var OneSet2 = IntSet(34)
    assert(OneSet1.subsetOf(MySet) === true)
    assert(OneSet2.subsetOf(MySet) === false)
    assert(MySet.subsetOf(OneSet1) === false)
    assert(MySet.subsetOf(OneSet2) === false)
  }

  test("same size, not a subset"){
    var MySet2 = IntSet(2,3, 6, 1, 17, 13, 5, 4, 23)
    assert(MySet2.subsetOf(MySet) === false)
    assert(MySet.subsetOf(MySet2) === false)
  }

  test("subset of itself") {
    assert(MySet.subsetOf(MySet) === true)
    assert(MySet.subsetOf(MySet3) === true)
    assert(MySet3.subsetOf(MySet) === true)
    assert(EmptySet.subsetOf(new IntSet) === true)
  }

  test("adding makes something not a subset"){
    assert(MySet3.subsetOf(MySet) === true)
    MySet3.add(11)
    assert(MySet3.subsetOf(MySet) === false)
  }

  test("adding makes something a subset"){
    var MyMissingSet = IntSet(2,3, 8, 1, 17, 13, 5, 4)
    assert(MySet.subsetOf(MyMissingSet) === false)
    assert(MyMissingSet.subsetOf(MySet) === true)
    MyMissingSet.add(23)
    assert(MySet.subsetOf(MyMissingSet) === true)
  }

  test("removing makes something not a subset"){
    assert(MySet.subsetOf(MySet3) === true)
    MySet3.remove(1)
    assert(MySet.subsetOf(MySet3) === false)
  }
}

class UnionTest extends AnyFunSuite{
  var MySet = IntSet(9, 2, 5, 3, 7)
  var EmptySet = new IntSet
  
  test("union empty set with empty set"){
    val z = EmptySet.union(EmptySet)
    assert(z.toString === "{}")
  }

  test("union full set with empty set"){
    val y = MySet.union(EmptySet)
    assert(y.toString === "{2, 3, 5, 7, 9}")
  }

  test("union empty set with full set"){
    val x = EmptySet.union(MySet)
    assert(x.toString === "{2, 3, 5, 7, 9}")
  }

  test("union a set with itself"){
    val x = MySet.union(MySet)
    assert(x.toString === "{2, 3, 5, 7, 9}")
  }

  test("union a full set with another full set (no duplicates)"){
    val y = IntSet(4, 17, 20)
    val x = MySet.union(y)
    assert(x.toString === "{2, 3, 4, 5, 7, 9, 17, 20}")
  }

  test("union a full set with another full set (duplicates)"){
    val y = IntSet(4, 17, 20, 9, 3)
    val x = MySet.union(y)
    assert(x.toString === "{2, 3, 4, 5, 7, 9, 17, 20}")
  }
}

class IntersectionTest extends AnyFunSuite{
  var MySet = IntSet(9, 2, 5, 3, 7)
  var EmptySet = new IntSet
  
  test("intersect empty set with empty set"){
    val z = EmptySet.intersect(EmptySet)
    assert(z.toString === "{}")
  }

  test("intersect full set with empty set"){
    val y = MySet.intersect(EmptySet)
    assert(y.toString === "{}")
  }

  test("intersect empty set with full set"){
    val x = EmptySet.intersect(MySet)
    assert(x.toString === "{}")
  }

  test("intersect a set with itself"){
    val x = MySet.intersect(MySet)
    assert(x.toString === "{2, 3, 5, 7, 9}")
  }

  test("intersect a full set with another full set (no duplicates)"){
    val y = IntSet(4, 17, 20)
    val x = MySet.intersect(y)
    assert(x.toString === "{}")
  }

  test("intersect a full set with another full set (duplicates)"){
    val y = IntSet(4, 17, 20, 9, 3)
    val x = MySet.intersect(y)
    assert(x.toString === "{3, 9}")
  }
}

class MapTest extends AnyFunSuite{
  var MyTestSet = IntSet(7, 8, 9, 12, 3, 2, 5, 6, 10, 25)

  test("mapping on an empty set gives an empty set") {
    var EmptySet = new IntSet
    assert(EmptySet.map((i : Int) => i + 3).toString === "{}")
    assert(EmptySet.map((i : Int) => i*i).toString === "{}")
  }

  test("map to a full set - add"){
    assert(MyTestSet.toString === "{2, 3, 5, 6, 7, 8, 9, 10, 12, 25}")
    assert(MyTestSet.map((i : Int) => i+3).toString === "{5, 6, 8, 9, 10, 11, 12, 13, 15, 28}")
  }

  test("map to a full set - square"){
    assert(MyTestSet.map((i : Int) => i*i).toString === "{4, 9, 25, 36, 49, 64, 81, 100, 144, 625}")
  }

  test("map a constant function"){
    assert(MyTestSet.map((i : Int) => 5).toString === "{5}")
  }

  test("map doesn't edit the set"){
    assert(MyTestSet.toString === "{2, 3, 5, 6, 7, 8, 9, 10, 12, 25}")
    MyTestSet.add(1)
    assert(MyTestSet.toString === "{1, 2, 3, 5, 6, 7, 8, 9, 10, 12, 25}")
  }

}

class FilterTest extends AnyFunSuite{
  var MyTestSet = IntSet(7, 8, 9, 12, 3, 2, 5, 6, 10, 25)
  test("filtering an empty set"){
    var EmptySet = new IntSet
    assert(EmptySet.filter((i : Int) => (i%2) == 1).toString === "{}")
    assert(EmptySet.filter((i : Int) => false).toString === "{}")
    assert(EmptySet.filter((i : Int) => true).toString === "{}")
  }

  test("filter - remove all elements"){
    var EvenSet = IntSet(2,4,8,18,20)
    assert(EvenSet.filter((i : Int) => (i%2) == 1).toString === "{}")
  }

  test("filter - remove no elements"){
    var OddSet = IntSet(1,3,19,23,101)
    assert(OddSet.filter((i : Int) => (i%2) == 1).toString === "{1, 3, 19, 23, 101}")
  }

  test("filter - remove some elements"){
    assert(MyTestSet.filter((i : Int) => (i%2) == 1).toString === "{3, 5, 7, 9, 25}")
  }

  test("filter a constant true function"){
    assert(MyTestSet.filter( (i : Int) => true).toString === "{2, 3, 5, 6, 7, 8, 9, 10, 12, 25}" )
  }

  test("filter a constant false function"){
    assert(MyTestSet.filter( (i : Int) => false).toString === "{}" )
  }

  test("filter doesn't edit the set"){
    assert(MyTestSet.toString === "{2, 3, 5, 6, 7, 8, 9, 10, 12, 25}")
  }
} 

class DropWhileTest extends AnyFunSuite {
  var MyTestSet = IntSet(7, 8, 9, 12, 3, 2, 5, 6, 10, 25)
  test("dropping an empty set"){
    var EmptySet = new IntSet
    assert(EmptySet.dropWhile((i : Int) => (i%2) == 1).toString === "{}")
    assert(EmptySet.dropWhile((i : Int) => false).toString === "{}")
    assert(EmptySet.dropWhile((i : Int) => true).toString === "{}")
  }

  test("drop - remove all elements"){
    var EvenSet = IntSet(2,4,8,18,20)
    assert(EvenSet.dropWhile((i : Int) => (i%2) == 1).toString === "{}")
  }

  test("drop - remove no elements"){
    var OddSet = IntSet(1,3,19,23,101)
    assert(OddSet.dropWhile((i : Int) => (i%2) == 1).toString === "{1, 3, 19, 23, 101}")
  }

  test("drop - remove some elements"){
    assert(MyTestSet.dropWhile((i : Int) => (i <= 7)).toString === "{2, 3, 5, 6, 7}")
  }

  test("drop a constant true function"){
    assert(MyTestSet.dropWhile( (i : Int) => true).toString === "{2, 3, 5, 6, 7, 8, 9, 10, 12, 25}" )
  }

  test("drop a constant false function"){
    assert(MyTestSet.dropWhile( (i : Int) => false).toString === "{}" )
  }

  test("drop doesn't edit the set"){
    assert(MyTestSet.toString === "{2, 3, 5, 6, 7, 8, 9, 10, 12, 25}")
  }
}

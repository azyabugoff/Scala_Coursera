package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val setPositiveNumbers = union(singletonSet(1), singletonSet(300))
    val setNegativeNumbers = union(singletonSet(-10), singletonSet(-99))
    val setPositiveAndNegativeNumbers = union(setPositiveNumbers, setNegativeNumbers)
    val setEvenNumbers = union(singletonSet(4), singletonSet(6))
    val setOddNumbers = union(singletonSet(3), singletonSet(9))
    val setEvenAndOddNumbers = union(setEvenNumbers, setOddNumbers)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one"/*.ignore*/) {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect set one two contains 1, 2; doesn't contain 3") {
    new TestSets:
      val u1 = union(union(s1, s2), singletonSet(3))
      val u2 = union(union(s1, s2), singletonSet(4))
      val intersect12 = intersect(u1, u2)
      assert(contains(intersect12, 1), "Intersect 1")
      assert(contains(intersect12, 2), "Intersect 2")
      assert(!contains(intersect12, 3), "Intersect 3")
  }

  test("diff set three contains 3; doesn't contain 1, 2, 4") {
    new TestSets:
      val u1 = union(union(s1, s2), singletonSet(3))
      val u2 = union(union(s1, s2), singletonSet(4))
      val diff3 = diff(u1, u2)
      assert(!contains(diff3, 1), "Diff 1")
      assert(!contains(diff3, 2), "Diff 2")
      assert(!contains(diff3, 4), "Diff 4")
      assert(contains(diff3, 3), "Diff 3")
  }

  test("filter returns the subset of one set for which a parameter function holds") {
    new TestSets {
      val filterSet1 = filter(s1, (elem: Int) => {elem < 2})
      assert(contains(filterSet1, 1), "filter 1")
      val filterSet2 = filter(s3, (elem: Int) => {elem > 5})
      assert(!contains(filterSet2, 3), "filter 3")
    }
  }

  test("forall function") {
    new TestSets {
      assert(forall(setPositiveNumbers, (elem:Int) => {elem > 0}), "forall 1")
      assert(forall(setNegativeNumbers, (elem:Int) => {elem < 0}), "forall 2")
      assert(!forall(setPositiveAndNegativeNumbers, (elem:Int) => {elem < 0}), "forall 3")
      assert(forall(setEvenNumbers, (elem:Int) => {(elem % 2) == 0}), "forall 4")
      assert(forall(setOddNumbers, (elem:Int) => {(elem % 2) != 0}), "forall 5")
      assert(!forall(setEvenAndOddNumbers, (elem:Int) => {(elem % 2) == 0}), "forall 6")
    }
  }

  test("exists function") {
    new TestSets {
      assert(exists(setPositiveAndNegativeNumbers, (elem: Int) => {elem > 0}), "exists 1")
      assert(exists(setEvenAndOddNumbers, (elem: Int) => {(elem % 2) ==  0}), "exists 2")
    }
  }

  test("map function"){
    new TestSets {
      val mapEvenSetToOdd = map(setEvenNumbers, (elem: Int) => {elem + 1})
      assert(contains(mapEvenSetToOdd,5) && contains(mapEvenSetToOdd,7))
      val mapOddSetToEven = map(setOddNumbers, (elem: Int) => {elem * 2})
      assert(forall(mapOddSetToEven, (elem: Int) => {(elem % 2) == 0}))
    }
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds

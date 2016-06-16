package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

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

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val sa = union(s1, s2)
      assert(contains(sa, 1), "Union 1")
      assert(contains(sa, 2), "Union 2")
      assert(!contains(sa, 3), "Union 3")
      val sb = union(sa, s3)
      assert(contains(sb, 3), "3 in sb")
    }
  }

  test("intersect contains all common elements of each set") {
    new TestSets {
      val sa = union(s1, s2)
      val sb = union(s1, s3)
      val s = intersect(sa, sb)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }
  
  test("diff contains the first set elements that are not in the second one") {
    new TestSets {
      val sa = union(s1, s2)
      val sb = union(s1, s3)
      val sq1 = diff(sa, sb)
      assert(!contains(sq1, 1), "Diff 1")
      assert(contains(sq1, 2), "Diff 2")
      assert(!contains(sq1, 3), "Diff 3")
      
      val sc = union(union(s1, s2), s4)
      val sq2 = diff(sc, sb)
      assert(!contains(sq2, 1), "Diff 4")
      assert(contains(sq2, 2), "Diff 5")
      assert(!contains(sq2, 3), "Diff 6")
      assert(contains(sq2, 4), "Diff 7")
    }
  }

  test("filter") {
    new TestSets {
      val sa = union(s1, s2)
      val sb = union(s1, s3)
      val sq1 = filter(sa, x => x > 1)
      assert(!contains(sq1, 1), "Filter 1")
      assert(contains(sq1, 2), "Filter 2")
      assert(!contains(sq1, 3), "Filter 3")
      
      val sc = union(union(s1, s2), s4)
      val sq2 = filter(sc, x => x > 1 && x <= 4)
      assert(!contains(sq2, 1), "Filter 4")
      assert(contains(sq2, 2), "Filter 5")
      assert(!contains(sq2, 3), "Filter 6")
      assert(contains(sq2, 4), "Filter 7")
    }
  }
  
  test("forall") {
    new TestSets {
      val sa = union(s1, s2)
      val sb = union(s1, s3)
      val sc = union(union(s1, s2), s4)
      assert(forall(sa, x => x > 0 && x < 3), "forall 1")
      assert(!forall(sb, x => x > 0 && x < 3), "forall 2")
      assert(forall(sc, x => x != 3), "forall 3")
    }
  } 
  
  test("exists") {
    new TestSets {
      val sa = union(s1, s2)
      val sb = union(s1, s3)
      val sc = union(union(s1, s2), s4)
      assert(exists(sa, x => x > 0 && x < 3), "exists 1")
      assert(!exists(sb, x => x < 0), "exists 2")
      assert(exists(sc, x => x == 4), "exists 3")
    }
  }   
  
  test("map") {
    new TestSets {
      val sc = union(union(s1, s2), s4)
      val s = map(sc, x => x + 10)
      assert(contains(s, 11), "map 1")
    }
  }   
}

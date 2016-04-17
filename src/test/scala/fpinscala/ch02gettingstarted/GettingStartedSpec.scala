package fpinscala.ch02gettingstarted

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks

import scala.collection.immutable.{BitSet, HashSet, TreeSet}

class GettingStartedSpec extends WordSpec with PropertyChecks with Matchers {

  "fibonacci (with scalacheck experiments)" must {
    val fibonacci = Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233)

    "match the calculated fib number in a results Table" in {
      val values = Table(("fib", "num"), fibonacci.zipWithIndex: _*)
      forAll(values) { (fib, num) =>
        MyModule.fib(num) shouldBe fib
        MyModule.fib_non_tailrec(num) shouldBe fib
      }
    }

    "match the index Generator in a results Table" in {
      val values = Table("fib", fibonacci: _*)
      val indexes = Gen.choose(0, values.length - 1)
      forAll((indexes, "num")) { n => MyModule.fib(n) shouldBe values(n) }
    }

    // Adapted from “QuickCheck in Action” in:
    // [A brief intro to QuickCheck | Stuart Gunter](http://www.stuartgunter.org/intro-to-quickcheck/)
    "comply with fib(n) + fib(n + 1) = f(n + 2) for non-negative numbers Generator" in {
      // Gen.posNum[Int] goes from 1...Int.MaxValue which goes around to negative values
      forAll((Gen.choose(0, 42), "num")) { n: Int =>
        val x = MyModule.fib(n)
        val y = MyModule.fib(n + 1)
        val z = MyModule.fib(n + 2)
        x should be >= 0
        y should be >= 0
        z should be >= 0
        x should (be < y or be < z)
        x + y shouldEqual z
      }
    }
  }
}

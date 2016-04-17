package fpinscala.ch02gettingstarted

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks

import scala.collection.immutable.{BitSet, HashSet, TreeSet}

class GettingStartedSpec extends WordSpec with PropertyChecks with Matchers {
  import MyModule._

  "fibonacci (with scalacheck Tables)" must {
    val fibonacci = Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233)

    "match the calculated fib number in a results Table" in {
      val values = Table(("fib", "num"), fibonacci.zipWithIndex: _*)
      forAll(values) { (x, n) =>
        fib(n) shouldBe x
        fib_non_tailrec(n) shouldBe x
      }
    }

    "match the index Generator in a results Table" in {
      val values = Table("fib", fibonacci: _*)
      val indexes = Gen.choose(0, values.length - 1)
      forAll((indexes, "num")) { n => fib(n) shouldBe values(n) }
    }
  }

  "fibonacci (with scalacheck Generator)" must {
    // Gen.posNum[Int] goes from 1...Int.MaxValue which causes `fib` to go around to negative values
    val smallNonNegativeNums = Gen.choose(0, 42)

    // Adapted from “QuickCheck in Action” in:
    // [A brief intro to QuickCheck | Stuart Gunter](http://www.stuartgunter.org/intro-to-quickcheck/)
    "comply with the rule fib(n) + fib(n+1) = f(n+2)" in {
      forAll((smallNonNegativeNums, "num")) { n: Int =>
        fib(n) + fib(n + 1) shouldEqual fib(n + 2)
      }
    }

    "always be a non-negative value" in {
      forAll((smallNonNegativeNums, "num")) { n: Int =>
        fib(n) should be >= 0
      }
    }

    "be strictly ordered fib(n) < fib(n+1) for every n != 1" in {
      forAll((smallNonNegativeNums, "num")) { n: Int =>
        whenever(n != 1)(fib(n) should be < fib(n + 1))
      }
    }
  }
}

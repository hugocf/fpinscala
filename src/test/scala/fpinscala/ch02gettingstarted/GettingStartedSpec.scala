package fpinscala.ch02gettingstarted

import fpinscala.BaseSpec
import org.scalacheck.{Gen, Shrink}

class GettingStartedSpec extends BaseSpec {

  "fibonacci (with scalacheck Tables)" must {
    import MyModule._
    val fibonacci = Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233)

    "match the calculated fib number in a results Table" in {
      val values = Table(("fib", "num"), fibonacci.zipWithIndex: _*)
      forAll(values) { (x, n) =>
        fib(n) shouldBe x
        fib_nonTailrecVersion(n) shouldBe x
      }
    }

    "match the index Generator in a results Table" in {
      val values = Table("fib", fibonacci: _*)
      val indexes = Gen.choose(0, values.length - 1)
      forAll((indexes, "num")) { n => fib(n) shouldBe values(n) }
    }
  }

  "fibonacci (with scalacheck Generator)" must {
    import MyModule._
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

  "isSorted" must {
    import PolymorphicFunctions._
    def gt(a: Int, b: Int) = a > b
    val intArray = Gen.containerOf[Array, Int](Gen.chooseNum(Int.MinValue, Int.MaxValue))

    "return true for any sorted array" in {
      // Array(2, 2, 3, 1) => Array(1, 2, 3)
      val sortedUniqueInts = intArray map { _.sorted.distinct }
      forAll(sortedUniqueInts) { xs =>
        isSorted(xs, gt) shouldBe true
      }
    }

    "return false for unordered lists" in {
      // turn off shrinking to show the exact error case!
      implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
      // Array() => X
      // Array(1, 2, 3) => Array(1, 2, 3, 1)
      val unorderedDupInts = intArray suchThat { _.length > 0 } map { xs => xs :+ xs.head }

      forAll(unorderedDupInts) { xs =>
        isSorted(xs, gt) shouldBe false
      }
    }
  }
}

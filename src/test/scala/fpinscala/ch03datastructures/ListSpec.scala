package fpinscala.ch03datastructures

import fpinscala.BaseSpec
import fpinscala.ch03datastructures.List._
import org.scalacheck.{Arbitrary, Gen, Shrink}

class ListSpec extends BaseSpec {

  "tail" must {
    "return the list when concatenating an element with a list" in {
      forAll { (x: Int, xs: Seq[Int]) =>
        val xl = List(xs: _*)
        tail(Cons(x, xl)) shouldBe xl
      }
    }
  }

  "setHead" must {
    "return the element when concatenating an element with a list" in {
      forAll { (o: Int, x: Int, xs: Seq[Int]) =>
        val xl = List(xs: _*)
        setHead(Cons(o, xl), x) shouldBe Cons(x, xl)
      }
    }
  }

  "drop" must {
    "return the second list after dropping all elements of the first, when concatenating two lists" in {
      forAll { (xs: Seq[Int], ys: Seq[Int]) =>
        val xyl = List(xs ++ ys: _*)
        drop(xyl, xs.length) shouldBe List(ys: _*)
      }
    }

    "return the list when dropping nothing" in {
      forAll { xs: Seq[Int] =>
        val xl = List(xs: _*)
        drop(xl, 0) shouldBe xl
      }
    }

    "return nothing when dropping all elements of the list" in {
      forAll { xs: Seq[Int] =>
        val xl = List(xs: _*)
        drop(xl, xs.length) shouldBe Nil
      }
    }
  }

  "dropWhile" must {
    "return the second list if all elements of the first list match, when concatenating two lists" in {
      val negNums = Gen.listOf(Gen.negNum[Int])
      val posNums = Gen.listOf(Gen.posNum[Int])
      val predicate = (x: Int) => x < 0

      forAll(negNums, posNums) { (xs, ys) =>
        val xyl = List(xs ++ ys: _*)
        dropWhile(xyl, predicate) shouldBe List(ys: _*)
      }
    }
  }

  "init" must {
    "return the initial list without the last element" in {
      forAll { (xs: Seq[Int], y: Int) =>
        val xyl = List(xs :+ y: _*)
        init(xyl) shouldBe List(xs: _*)
      }
    }
  }
}

class ListSpec_firstVersion extends BaseSpec {

  // See [scala - How to define an arbitrary for a custom list in scalacheck? - Stack Overflow][1]
  // [1]: http://stackoverflow.com/questions/31878928/how-to-define-an-arbitrary-for-a-custom-list-in-scalacheck

  private val myNilGen: Gen[List[Nothing]] = Gen.delay(Nil)

  implicit def myListArbitrary[T: Arbitrary]: Arbitrary[List[T]] = Arbitrary[List[T]](Gen.oneOf(myNilGen, myConsGen[T]))

  private def myConsGen[T: Arbitrary]: Gen[List[T]] = for {
    head <- Arbitrary.arbitrary[T]
    tail <- Gen.oneOf(myNilGen, myConsGen[T])
  } yield Cons(head, tail)

  "tail" must {
    "return the list when concatenating an element with a list" in {
      cancel("(old implementation; see ListSpec!)")
      forAll { (x: Int, xs: List[Int]) =>
        tail(Cons(x, xs)) shouldBe xs
      }
    }
  }

  "setHead" must {
    "return the element when concatenating an element with a list" in {
      cancel("(old implementation; see ListSpec!)")
      forAll { (o: Int, x: Int, xs: List[Int]) =>
        setHead(Cons(o, xs), x) shouldBe Cons(x, xs)
      }
    }
  }

  /** Generate Cons list with fixed length */
  private def myConsGenOfN[T: Arbitrary](n: Int): Gen[List[T]] =
    if (n == 0) myNilGen
    else for {
      head <- Arbitrary.arbitrary[T]
      tail <- if (n == 1) myNilGen else myConsGenOfN[T](n - 1)
    } yield Cons(head, tail)

  def myConsGenWithLength = for {
    n <- Gen.choose(1, 5)
    xs <- myConsGenOfN[Int](n)
  } yield (xs, n)

  "drop" must {
    // turn off shrinking to show the exact error case!
    implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

    "return the second list after dropping all elements of the first, when concatenating two lists" in {
      cancel("(old implementation; see ListSpec!)")
      forAll(myConsGenWithLength, myConsGen[Int]) { case ((xs, len), ys) =>
        drop(append(xs, ys), len) shouldBe ys
      }
    }

    "return the list when dropping nothing" in {
      cancel("(old implementation; see ListSpec!)")
      forAll(myConsGenWithLength) { case (xs, len) =>
        drop(xs, 0) shouldBe xs
      }
    }

    "return nothing when dropping all elements of the list" in {
      cancel("(old implementation; see ListSpec!)")
      forAll(myConsGenWithLength) { case (xs, len) =>
        drop(xs, len) shouldBe Nil
      }
    }
  }
}

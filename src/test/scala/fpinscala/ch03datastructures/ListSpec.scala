package fpinscala.ch03datastructures

import fpinscala.BaseSpec
import fpinscala.ch03datastructures.List.{length => lengthCons, _}
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._

import scala.util.Random

object ConsListGenerator {
  // See [scala - How to define an arbitrary for a custom list in scalacheck? - Stack Overflow][1]
  // [1]: http://stackoverflow.com/questions/31878928/how-to-define-an-arbitrary-for-a-custom-list-in-scalacheck
  val myNilGen: Gen[List[Nothing]] = Gen.delay(Nil)
  implicit def myListArbitrary[T: Arbitrary]: Arbitrary[List[T]] = Arbitrary[List[T]](Gen.oneOf(myNilGen, myConsGen[T]))

  def myConsGen[T: Arbitrary]: Gen[List[T]] = for {
    head <- Arbitrary.arbitrary[T]
    tail <- Gen.oneOf(myNilGen, myConsGen[T])
  } yield Cons(head, tail)

  /** Generate Cons list with fixed length */
  def myConsGenOfN[T: Arbitrary](n: Int): Gen[List[T]] =
    if (n == 0) myNilGen
    else for {
      head <- Arbitrary.arbitrary[T]
      tail <- if (n == 1) myNilGen else myConsGenOfN[T](n - 1)
    } yield Cons(head, tail)
}

class ListSpec_firstVersion extends BaseSpec {
  import ConsListGenerator._

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

  "drop" must {
    // turn off shrinking to show the exact error case!
    implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

    // See also https://gist.github.com/davidallsopp/f65d73fea8b5e5165fc3
    // for other solutions to prevent shrinking from generating invalid values

    def myConsGenWithLength = for {
      n <- Gen.choose(1, 5)
      xs <- myConsGenOfN[Int](n)
    } yield (xs, n)

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
      val negNums = listOf(negNum[Int])
      val posNums = listOf(posNum[Int])
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

  "exercise 3.8" should {
    "see what happens when you pass Nil and Cons themselves to foldRight" in {
      foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) shouldBe List(1,2,3)
    }
  }

  "length" must {
    "return the size of the list" in {
      forAll { xs: Seq[Int] =>
        val xl = List(xs: _*)
        lengthCons(xl) shouldBe xs.length
      }
    }
  }

  "foldLeft" must {
    "tail recurse the aggregated value of the list" in {
      forAll { xs: Seq[Int] =>
        val xl = List(xs: _*)
        foldLeft(xl, 0)((res, _) => res + 1) shouldBe xs.length
      }
    }
  }

  "sum3" must {
    "return the result of adding all list elements" in {
      forAll { xs: Seq[Int] =>
        val xl = List(xs: _*)
        val lx = List(xs.reverse: _*)
        sum3(xl) shouldBe sum3(lx)
      }
    }
  }

  "product3" must {
    "return the result of multiplying all list elements" in {
      val smallListOfDoubles = for {
        n <- chooseNum(0, 10)
        xs <- listOfN(n, chooseNum[Double](0, 100))
      } yield xs

      forAll(smallListOfDoubles) { xs =>
        val xl = List(xs: _*)
        val lx = List(xs.reverse: _*)
        product3(xl) shouldBe (product3(lx) +- 0.1)
      }
    }
  }

  "length3" must {
    "return the size of the list" in {
      forAll { xs: Seq[Int] =>
        val xl = List(xs: _*)
        length3(xl) shouldBe xs.length
      }
    }
  }

  "reverse" must {
    "return the original list when reverse is applied twice" in {
      forAll { xs: Seq[Int] =>
        val xl = List(xs: _*)
        reverse(reverse(xl)) shouldBe xl
      }
    }

    "reverse two appended lists like the reverse of the second appended to the first" in {
      forAll { (xs: Seq[Int], ys: Seq[Int]) =>
        val xl = List(xs: _*)
        val yl = List(ys: _*)
        val xyl = List(xs ++ ys: _*)
        reverse(xyl) shouldBe append(reverse(yl), reverse(xl))
      }
    }
  }

  "foldLeft2 vs. foldRight2" when {
    val listOfOnes = for {
      n <- chooseNum(0, 100)
      xs <- listOfN(n, 1)
    } yield xs

    "foldLeft2" must {
      "display the aggregated value of the list" in {
        forAll(listOfOnes) { xs =>
          val xl = List(xs: _*)
          // List(1, 1, 1) => (((0 - 1) - 1) - 1) = -3
          foldLeft2(xl, 0)(_ - _) shouldBe -xs.length
        }
      }
    }

    "foldRight2" must {
      "display the aggregated value of the list" in {
        forAll(listOfOnes) { xs =>
          val xl = List(xs: _*)
          // List(1, 1)    =>      (1 - (1 - 0))  = 0
          // List(1, 1, 1) => (1 - (1 - (1 - 0))) = 1
          foldRight2(xl, 0)(_ - _) shouldBe xs.length % 2
        }
      }
    }
  }

  "append2" must {
    val nonEmptyList = nonEmptyListOf(arbitrary[Int])

    "have total length equal to the sum of both list lengths" in {
      forAll { (xs: Seq[Int], ys: Seq[Int]) =>
        val xl = List(xs: _*)
        val yl = List(ys: _*)
        val xyl = List(xs ++ ys: _*)
        lengthCons(append2(xl, yl)) shouldBe lengthCons(xl) + lengthCons(yl)
      }
    }

    "respect the order of non-empty lists" in {
      forAll(nonEmptyList, nonEmptyList) { (xs, ys) =>
        val xl = List(xs: _*)
        val yl = List(ys: _*)

        val result = append2(xl, yl)

        result match {
          case Cons(head, tail) =>
            val last = foldLeft(tail, None:Option[Int])((_, e) => Some(e)).value
            last shouldBe ys.last
            head shouldBe xs.head
          case _ => assert(true)
        }
      }
    }

    "ignore empty lists" in {
      forAll(nonEmptyList) { xs =>
        val xl = List(xs: _*)
        append2(xl, Nil) shouldBe xl
        append2(Nil, xl) shouldBe xl
      }
    }
  }

  "listOfLists" must {
    import ConsListGenerator._

    "have total length equal to the sum of all list lengths" in {
      forAll { xl: List[List[Int]] =>
        val totalLength = foldLeft(xl, 0)(_ + lengthCons(_))
        lengthCons(listOfLists(xl)) shouldBe totalLength
      }
    }

    "respect the order of the lists" in {
      implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

      val sortedLists = for {
        chunks <- choose(1, 10)
        length <- choose(chunks, 100)
        ls <- listOfN(length, arbitrary[Int]) map { _.sorted.distinct }
      } yield (ls, ls.grouped(chunks).toList)

      forAll(sortedLists) { case (xs, xss) =>
        val xll: List[List[Int]] = List(xss.map(List(_: _*)): _*)
        listOfLists(xll) shouldBe List(xs: _*)
      }
    }
  }

  "addOne" must {
    import ConsListGenerator._

    "have the sum of the new list equal the sum of the original one plus its length" in {
      forAll { xl: List[Int] =>
        sum(addOne(xl)) shouldBe sum(xl) + lengthCons(xl)
      }
    }
  }

  "listToString" must {
    val tuple = for(n <- arbitrary[Double]) yield { (n, n.toString) }

    "return the list of each converted source value" in {
      forAll(listOf(tuple)) { ts =>
        val nl = List(ts.unzip._1: _*)
        val sl = List(ts.unzip._2: _*)
        listToString(nl) shouldBe sl
      }
    }
  }

  "map" must {
    def f(d: Double) = s"This is a $d number"
    val tuple = for(n <- arbitrary[Double]) yield { (n, f(n)) }

    "return the list of each converted source value" in {
      forAll(listOf(tuple)) { ts =>
        val nl = List(ts.unzip._1: _*)
        val sl = List(ts.unzip._2: _*)
        map(nl)(f) shouldBe sl
      }
    }
  }

  "filter" must {
    import ConsListGenerator._
    val isEven: Int => Boolean = _ % 2 == 0
    val isOdd: Int => Boolean = !isEven(_)

    "be idempotent: apply it twice is the same as doing it once" in {
      forAll { xl: List[Int] =>
        val evenNums = filter(xl)(isEven)
        filter(evenNums)(isEven) shouldBe evenNums
      }
    }

    "return the same if applied before or after appending two lists" in {
      forAll { (xl: List[Int], yl: List[Int]) =>
        filter(append(xl, yl))(isEven) shouldBe append(filter(xl)(isEven), filter(yl)(isEven))
      }
    }

    "keep only the even numbers of the list" in {
      // turn off shrinking to show the exact error case!
      implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

      val evenGen = for (n <- choose(-5000, 5000)) yield n * 2
      val oddGen = for (n <- choose(-5000, 5000)) yield n * 2 + 1
      val evens = (listOf(evenGen), "evens")
      val odds = (listOf(oddGen), "odds")

      forAll(odds, evens) { (os, es) =>
        val oel = List(Random.shuffle(os ++ es): _*)
        val el = List(es: _*)
        filter(oel)(isEven) shouldBe el
      }
    }
  }

  "flatMap" must {
    "return a single list of duplicate items" in {
      forAll { xs: Seq[Int] =>
        val xl = List(xs.sorted: _*)
        val xxl = List((xs ++ xs).sorted: _*)
        flatMap(xl)(i => List(i, i)) shouldBe xxl
      }
    }
  }
}

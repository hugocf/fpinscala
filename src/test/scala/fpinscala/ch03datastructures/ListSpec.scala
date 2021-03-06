package fpinscala.ch03datastructures

import fpinscala.BaseSpec
import fpinscala.ch03datastructures.List.{length => lengthCons, _}
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck._

object ConsListGenerator {
  // See [scala - How to define an arbitrary for a custom list in scalacheck? - Stack Overflow][1]
  // [1]: http://stackoverflow.com/questions/31878928/how-to-define-an-arbitrary-for-a-custom-list-in-scalacheck
  val myNilGen: Gen[List[Nothing]] = Gen.const(Nil)

  implicit def myListArbitrary[T: Arbitrary]: Arbitrary[List[T]] = Arbitrary[List[T]](Gen.oneOf(myNilGen, myConsGen[T]))

  def myConsGen[T: Arbitrary]: Gen[List[T]] = for {
    head <- Arbitrary.arbitrary[T]
    tail <- Gen.oneOf(myNilGen, myConsGen[T])
  } yield Cons(head, tail)

  /** Generate Cons list with fixed length */
  def myConsGenOfN[T: Arbitrary](n: Int): Gen[List[T]] = {
    if (n == 0) myNilGen
    else for {
      head <- Arbitrary.arbitrary[T]
      tail <- if (n == 1) myNilGen else myConsGenOfN[T](n - 1)
    } yield Cons(head, tail)
  }
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

    // https://github.com/dabd/fpscala/commit/73ba1c798db6bac7d3caf9f32b6a78cf78f16e67
    // i.e. using pattern matching to obtain the head/tail of a generated list,
    //      instead of prepending a single element to the generated list.
    "see another way of doing it, via @dabd" in {
      forAll { xs: Seq[Int] =>
        val xl = List(xs: _*)
        xl match {
          case Cons(h, t) => tail(xl) shouldBe t
          case Nil => ()
        }
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

    // https://github.com/dabd/fpscala/commit/a1835fec4f60fd87ad7c8015d95bcbee8102c473#diff-453a8faaaebabd41f8181f2318ee68d2R62
    // Nice tip about using arbitrary functions
    "see another way of doing it, via @dabd" in {
      forAll { (xs: Seq[Int], f: Int => Boolean) =>
        val xl = List(xs: _*)
        xl match {
          case Nil => dropWhile(Nil, f) shouldBe Nil
          case Cons(h, t) if f(h) => dropWhile(xl, f) shouldBe dropWhile(t, f)
          case Cons(h, t) if !f(h) => dropWhile(xl, f) shouldBe xl
        }
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
      foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
    }
  }

  "length" must {
    "return the size of the list" in {
      forAll { xs: Seq[Int] =>
        val xl = List(xs: _*)
        lengthCons(xl) shouldBe xs.length // well, this is cheating a bit
      }
    }

    // https://github.com/dabd/fpscala/commit/5adb9c03225143d3c0036c4ec44378ed4a67847a
    "see another way of doing it, via @dabd" in {
      forAll { xs: Seq[Int] =>
        val xl = List(xs: _*)
        xl match {
          case Cons(_, t) => lengthCons(xl) shouldBe lengthCons(t) + 1
          case Nil => ()
        }
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
        reverse(append(xl, yl)) shouldBe append(reverse(yl), reverse(xl))
      }
    }
  }

  // See also “universal property of fold” (http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)
  // via @dabd (https://github.com/dabd/fpscala/commit/a1835f#diff-453a8faaaebabd41f8181f2318ee68d2R92)
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
            val last = foldLeft(tail, None: Option[Int])((_, e) => Some(e)).value
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
        ls <- listOfN(length, arbitrary[Int]) map (_.sorted.distinct)
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
    val tuple = for (n <- arbitrary[Double]) yield (n, n.toString)

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
    val tuple = for (n <- arbitrary[Double]) yield (n, f(n))

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
      implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

      // List(a, b, c), List(d, e)
      // => List((a,0), (b,1), (c,2), (d,0), (e,1)) // zipWithIndex & ++
      // => List((a,0), (d,0), (b,1), (e,1), (c,2)) // sortBy
      // => List(a, d, b, e, c)                     // map
      def interleave(as: Seq[Int], bs: Seq[Int]) = (as.zipWithIndex ++ bs.zipWithIndex).sortBy(_._2).map(_._1)

      val evenGen = for (n <- choose(-5000, 5000)) yield n * 2
      val oddGen = for (n <- choose(-5000, 5000)) yield n * 2 + 1
      val evens = (listOf(evenGen), "evens")
      val odds = (listOf(oddGen), "odds")

      forAll(odds, evens) { (os, es) =>
        val oes = interleave(os, es)
        filter(List(oes: _*))(isEven) shouldBe List(es: _*)
      }
    }
  }

  "flatMap" must {
    "return a single list of duplicate items, in the original order" in {
      val dup = (i: Int) => List(i, i)
      forAll { xs: Seq[Int] =>
        val xxs = xs.foldLeft(Seq.empty[Int]) { (res, el) => res :+ el :+ el }
        flatMap(List(xs: _*))(dup) shouldBe List(xxs: _*)
      }
    }
  }

  "addLists" when {
    implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
    import ConsListGenerator._

    "[GOOD] testing with properties that force the correct implementation" must {
      "add a list to another with M multiples of each element is the same multiplying the first by M+1" in {
        val smallNumber = choose(0, 10)
        forAll(smallNumber, arbitrary[List[Int]]) { (m, xl) =>
          val yl = map(xl)(_ * m)
          val zl = map(xl)(_ * (m + 1))
          addLists(xl, yl) shouldBe zl
        }
      }
    }

    "[BAD] testing with properties that allow bogus implementations" must {
      // For example, this implementation passes all the properties below:
      // addLists(List(1, 2, 3), List(4, 5)) => List(0, 0, 15)

      "preserve the length of the longest list" in {
        forAll { (xl: List[Int], yl: List[Int]) =>
          val xn = lengthCons(xl)
          val yn = lengthCons(yl)
          val maxLen = if (xn > yn) xn else yn
          lengthCons(addLists(xl, yl)) shouldBe maxLen
        }
      }

      "be the same as adding the lists independently" in {
        forAll { (xl: List[Int], yl: List[Int]) =>
          List(sum(addLists(xl, yl))) shouldBe addLists(List(sum(xl)), List(sum(yl)))
        }
      }

      "be commutative" in {
        forAll { (xl: List[Int], yl: List[Int]) =>
          addLists(xl, yl) shouldBe addLists(yl, xl)
        }
      }

      "return one of the lists when adding zeros" in {
        val listOfZeros = for {
          n <- chooseNum(0, 100)
          xs <- listOfN(n, 0)
        } yield xs

        forAll(arbitrary[List[Int]], listOfZeros) { (xl, zs) =>
          val zl = List(zs: _*)
          addLists(xl, zl) shouldBe addLists(zl, xl)
        }
      }
    }
  }

  "zipWith" must {
    implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
    import ConsListGenerator._

    "apply the function to each pair of elements from both lists" in {
      val smallNumber = choose(0, 10)
      forAll(smallNumber, arbitrary[List[Int]]) { (m, xl) =>
        val yl = map(xl)(_ * m)
        val zl = map(xl)(_ * (m + 1))
        zipWith(xl, yl)(_+_) shouldBe zl
      }
    }
  }

  "hasSubsequence" must {
    val chunk = choose(1, 100)

    "be true for every chunk taken from the original list" in {
      forAll(chunk, arbitrary[Seq[Int]]) { (n, xs) =>
        val chunks = xs.grouped(n).toList
        chunks.foreach { l =>
          hasSubsequence(List(xs: _*), List(l: _*)) shouldBe true
        }
      }
    }

    "be false for every list chunk taken from the reverse list" in {
      val list = arbitrary[Seq[Int]] suchThat { _.length >= 2 }
      forAll(chunk, list) { (n, xs) =>
        val sorted = xs.distinct.sorted
        val reverseChunks = sorted.reverse.grouped(n).toList.filterNot(_.length == 1)
        reverseChunks.foreach { r =>
          hasSubsequence(List(sorted: _*), List(r: _*)) shouldBe false
        }
      }
    }

    "be false for every sub list greater than the main list" in {
      forAll(nonEmptyListOf(arbitrary[Int])) { xs =>
        hasSubsequence(List(xs: _*), List(xs ++ xs: _*)) shouldBe false
      }
    }
  }
}

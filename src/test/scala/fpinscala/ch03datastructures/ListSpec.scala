package fpinscala.ch03datastructures

import fpinscala.BaseSpec
import fpinscala.ch03datastructures.List._
import org.scalacheck.{Arbitrary, Gen}

class ListSpec extends BaseSpec {

  // See [scala - How to define an arbitrary for a custom list in scalacheck? - Stack Overflow][1]
  // [1]: http://stackoverflow.com/questions/31878928/how-to-define-an-arbitrary-for-a-custom-list-in-scalacheck
  private def myConsGen[T:Arbitrary]: Gen[List[T]] = for {
    head <- Arbitrary.arbitrary[T]
    tail <- Gen.oneOf(myNilGen, myConsGen[T])
  } yield Cons(head, tail)

  private val myNilGen: Gen[List[Nothing]] = Gen.delay(Nil)

  implicit def myListArbitrary[T:Arbitrary]: Arbitrary[List[T]] = Arbitrary[List[T]](Gen.oneOf(myNilGen, myConsGen[T]))

  "tail" must {
    "return the list when concatenating an element with a list" in {
      forAll { (x: Int, xs: List[Int]) =>
        tail(Cons(x, xs)) shouldBe xs
      }
    }
  }

  "setHead" must {
    "return the element when concatenating an element with a list" in {
      forAll { (o: Int, x: Int, xs: List[Int]) =>
        setHead(Cons(o, xs), x) shouldBe Cons(x, xs)
      }
    }
  }
}

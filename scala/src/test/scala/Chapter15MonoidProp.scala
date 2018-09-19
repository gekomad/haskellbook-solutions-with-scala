import cats.{Monoid, Semigroup}
import cats.instances.all._

import cats.syntax.semigroup._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class Chapter15MonoidProp extends Properties("ch15") {

  { //1

    final case class Trivial() {}

    implicit val mySemigroup: Semigroup[Trivial] = (x: Trivial, y: Trivial) => Trivial()

    implicit val mymonoid: Monoid[Trivial] = new Monoid[Trivial] {
      def combine(x: Trivial, y: Trivial): Trivial = Trivial()

      override def empty: Trivial = Trivial()
    }

    val rand = for {
      x <- const(Trivial())
      y <- const(Trivial())
      z <- const(Trivial())
    } yield (x, y, z)

    property("1 - Trivial") = forAll(rand) { case (x, y, z) => (x |+| (y |+| z)) == ((x |+| y) |+| z) }
  }

  { //2

    final case class Identity[A](a: A) {}

    implicit def mySemigroup[A: Semigroup]: Semigroup[Identity[A]] = (x: Identity[A], y: Identity[A]) => Identity(x.a |+| y.a)

    implicit def mymonoid[A: Monoid]: Monoid[Identity[A]] = new Monoid[Identity[A]] {
      def combine(x: Identity[A], y: Identity[A]): Identity[A] = (x, y) match {
        case (Identity(a), Identity(b)) => Identity(a |+| b)
      }

      override def empty: Identity[A] = Identity(Monoid[A].empty)
    }

    val randInt = for {
      x <- arbitrary[Int]
      y <- arbitrary[Int]
      z <- arbitrary[Int]
    } yield (Identity(x), Identity(y), Identity(z))

    val randString = for {
      x <- arbitrary[String]
      y <- arbitrary[String]
      z <- arbitrary[String]
    } yield (Identity(x), Identity(y), Identity(z))

    property("2 - Identity a") = forAll(randInt) { case (x, y, z) => (x |+| (y |+| z)) == ((x |+| y) |+| z) }
    property("2 - Identity b") = forAll(randString) { case (x, y, z) => (x |+| (y |+| z)) == ((x |+| y) |+| z) }

    assert((Identity(1) |+| Identity(Monoid[Int].empty)) == Identity(1))
  }

  { //3

    final case class Two[A, B](a: A, b: B)

    implicit def mySemigroup[A: Semigroup, B: Semigroup]: Semigroup[Two[A, B]] = (x: Two[A, B], y: Two[A, B]) => Two(x.a |+| y.a, x.b |+| y.b)

    implicit def mymonoid[A: Monoid, B: Monoid]: Monoid[Two[A, B]] = new Monoid[Two[A, B]] {
      def combine(x: Two[A, B], y: Two[A, B]): Two[A, B] = (x, y) match {
        case (Two(a, b), Two(a1, b1)) => Two(a |+| a1, b |+| b1)
      }

      override def empty: Two[A, B] = Two(Monoid[A].empty, Monoid[B].empty)
    }


    val randInt = for {
      x <- arbitrary[Int]
      y <- arbitrary[Int]
      z <- arbitrary[Int]
      x1 <- arbitrary[Int]
      y1 <- arbitrary[Int]
      z1 <- arbitrary[Int]
    } yield (Two(x, x1), Two(y, y1), Two(z, z1))

    val randString = for {
      x <- arbitrary[String]
      y <- arbitrary[String]
      z <- arbitrary[String]
      x1 <- arbitrary[String]
      y1 <- arbitrary[String]
      z1 <- arbitrary[String]
    } yield (Two(x, x1), Two(y, y1), Two(z, z1))

    property("3 - Two a") = forAll(randInt) { case (x, y, z) => (x |+| (y |+| z)) == ((x |+| y) |+| z) }
    property("3 - Two b") = forAll(randString) { case (x, y, z) => (x |+| (y |+| z)) == ((x |+| y) |+| z) }
    assert((Two(1, 1) |+| Two(Monoid[Int].empty, Monoid[Int].empty)) == Two(1, 1))
    assert((Two(1, 1) |+| Two(2, 4)) == Two(3, 5))
  }

  { //4

    import MyPredef.boolConjMonoid

    final case class BoolConj(a: Boolean)

    implicit val mySemigroup1: Monoid[BoolConj] = new Monoid[BoolConj] {
      def combine(x: BoolConj, y: BoolConj): BoolConj = (x, y) match {
        case (BoolConj(a), BoolConj(b)) => BoolConj(a |+| b)
      }

      override def empty: BoolConj = BoolConj(true)
    }

    assert((BoolConj(true) |+| BoolConj(true)) == BoolConj(true))
    assert((BoolConj(false) |+| BoolConj(true)) == BoolConj(false))
    assert((BoolConj(false) |+| BoolConj(false)) == BoolConj(false))

    val randBool = for {
      x0 <- arbitrary[Boolean]
      x1 <- arbitrary[Boolean]
      x2 <- arbitrary[Boolean]
    } yield (BoolConj(x0), BoolConj(x1), BoolConj(x2))

    property("4 - BoolConj") = forAll(randBool) { case (x, y, z) => (x |+| (y |+| z)) == ((x |+| y) |+| z) }

    assert((BoolConj(true) |+| BoolConj(true)) == BoolConj(true))
    assert((BoolConj(true) |+| BoolConj(false)) == BoolConj(false))
    assert((BoolConj(true).combine(BoolConj(true))) == BoolConj(true))

    assert((BoolConj(false) |+| BoolConj(Monoid[Boolean].empty)) == BoolConj(false))

  }

  { //5

    import MyPredef.boolDisjMonoid

    final case class BoolDisj(a: Boolean)

    implicit val mySemigroup1: Monoid[BoolDisj] = new Monoid[BoolDisj] {
      def combine(x: BoolDisj, y: BoolDisj): BoolDisj = (x, y) match {
        case (BoolDisj(a), BoolDisj(b)) => BoolDisj(a |+| b)
      }

      override def empty: BoolDisj = BoolDisj(true)
    }

    assert((BoolDisj(true) |+| BoolDisj(true)) == BoolDisj(true))
    assert((BoolDisj(false) |+| BoolDisj(true)) == BoolDisj(true))
    assert((BoolDisj(false) |+| BoolDisj(false)) == BoolDisj(false))

    val randBool = for {
      x0 <- arbitrary[Boolean]
      x1 <- arbitrary[Boolean]
      x2 <- arbitrary[Boolean]
    } yield (BoolDisj(x0), BoolDisj(x1), BoolDisj(x2))

    property("5 - BoolDisj") = forAll(randBool) { case (x, y, z) => (x |+| (y |+| z)) == ((x |+| y) |+| z) }

    assert((BoolDisj(true) |+| BoolDisj(false)) == BoolDisj(true))
    assert((BoolDisj(false) |+| BoolDisj(false)) == BoolDisj(false))

    assert((BoolDisj(true) |+| BoolDisj(Monoid[Boolean].empty)) == BoolDisj(true))
    assert((BoolDisj(Monoid[Boolean].empty) |+| BoolDisj(false)) == BoolDisj(false))

  }


}

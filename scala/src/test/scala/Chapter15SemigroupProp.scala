import cats.Semigroup
import cats.instances.all._
import cats.syntax.semigroup._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Properties, _}

class Chapter15SemigroupProp extends Properties("Chapter15Semigroup") {

  {
    // 1
    final case class Trivial() {}

    implicit val mySemigroup: Semigroup[Trivial] = new Semigroup[Trivial] {
      def combine(x: Trivial, y: Trivial): Trivial = Trivial()
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

    implicit def mySemigroup[A: Semigroup]: Semigroup[Identity[A]] = new Semigroup[Identity[A]] {
      override def combine(x: Identity[A], y: Identity[A]): Identity[A] = Identity(x.a |+| y.a)
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

  }

  { //3
  final case class Two[A, B](a: A, b: B)

    implicit def mySemigroup[A: Semigroup, B: Semigroup]: Semigroup[Two[A, B]] = (x: Two[A, B], y: Two[A, B]) => Two(x.a |+| y.a, x.b |+| y.b)

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
  }

  {
    //4
    final case class Three[A, B, C](a: A, b: B, c: C)

    implicit def mySemigroup[A: Semigroup, B: Semigroup, C: Semigroup]: Semigroup[Three[A, B, C]] =
      (x: Three[A, B, C], y: Three[A, B, C]) => Three(x.a |+| y.a, x.b |+| y.b, x.c |+| y.c)


    val randInt = for {
      x0 <- arbitrary[Int]
      x1 <- arbitrary[Int]
      x2 <- arbitrary[Int]

      y0 <- arbitrary[Int]
      y1 <- arbitrary[Int]
      y2 <- arbitrary[Int]

      z0 <- arbitrary[Int]
      z1 <- arbitrary[Int]
      z2 <- arbitrary[Int]

    } yield (Three(x0, x1, x2), Three(y0, y1, y2), Three(z0, z1, z2))

    property("4 - Three a") = forAll(randInt) { case (x, y, z) => (x |+| (y |+| z)) == ((x |+| y) |+| z) }

  }

  {
    //6
    final case class BoolConj(a: Boolean)

    implicit val mySemigroup1: Semigroup[BoolConj] = new Semigroup[BoolConj] {
      def combine(x: BoolConj, y: BoolConj): BoolConj = (x, y) match {
        case (BoolConj(true), BoolConj(true)) => BoolConj(true)
        case _ => BoolConj(false)
      }
    }

    assert((BoolConj(true) |+| BoolConj(true)) == BoolConj(true))
    assert((BoolConj(false) |+| BoolConj(true)) == BoolConj(false))
    assert((BoolConj(false) |+| BoolConj(false)) == BoolConj(false))

    val randBool = for {
      x0 <- arbitrary[Boolean]
      x1 <- arbitrary[Boolean]
      x2 <- arbitrary[Boolean]
    } yield (BoolConj(x0), BoolConj(x1), BoolConj(x2))

    property("6 - BoolConj a") = forAll(randBool) { case (x, y, z) => (x |+| (y |+| z)) == ((x |+| y) |+| z) }
  }

  {
    //7
    final case class BoolDisj(a: Boolean)

    implicit val mySemigroup1: Semigroup[BoolDisj] = (x: BoolDisj, y: BoolDisj) => (x, y) match {
      case (BoolDisj(false), BoolDisj(false)) => BoolDisj(false)
      case _ => BoolDisj(true)
    }

    assert((BoolDisj(true) |+| BoolDisj(true)) == BoolDisj(true))
    assert((BoolDisj(false) |+| BoolDisj(true)) == BoolDisj(true))
    assert((BoolDisj(false) |+| BoolDisj(false)) == BoolDisj(false))

    val randBool = for {
      x0 <- arbitrary[Boolean]
      x1 <- arbitrary[Boolean]
      x2 <- arbitrary[Boolean]
    } yield (BoolDisj(x0), BoolDisj(x1), BoolDisj(x2))

    property("7 - BoolDisj a") = forAll(randBool) { case (x, y, z) => (x |+| (y |+| z)) == ((x |+| y) |+| z) }

  }

}
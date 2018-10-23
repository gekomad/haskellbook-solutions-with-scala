import cats.instances.list._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{const, listOf}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import cats.Functor
import cats.syntax.functor._

class Chapter16Prop extends Properties("Chapter16Prop") {

  {
    trait FixMePls[+A]
    final case class Pls[A](a: A) extends FixMePls[A]
    final case object FixMe extends FixMePls[Nothing]

    implicit val myFunctor: Functor[FixMePls] = new Functor[FixMePls] {
      def map[A, B](fa: FixMePls[A])(f: A => B): FixMePls[B] = fa match {
        case FixMe => FixMe
        case Pls(a) => Pls(f(a))
      }
    }

    val o: FixMePls[Int] = Pls(1)

    val p: FixMePls[Int] = o.map(a => a + 1)
    assert(p == Pls(2))
  }

  //    functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
  //    functorIdentity f = fmap id f == f

  def functorIdentity[F[_] : Functor, A](fa: F[A]): Boolean = Functor[F].map(fa)(identity) == fa

  //    functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
  //    functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

  def functorCompose[F[_] : Functor, A, B, C](f: A => B, g: B => C, fa: F[A]): Boolean = {
    val ff = Functor[F].map(fa)(f)
    Functor[F].map(ff)(g) == Functor[F].map(fa)(f.andThen(g))
  }

  {
    property("functorIdentity") = forAll(listOf(arbitrary[Int])) {
      functorIdentity(_)
    }

    property("functorCompose") = forAll(listOf(arbitrary[Int])) {
      functorCompose((_: Int) + 1, (_: Int) * 2, _)
    }
  }

  { // 1

    final case class Identity[A](a: A)

    implicit val myFunctor: Functor[Identity] = new Functor[Identity] {
      def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = fa match {
        case Identity(a) => Identity(f(a))
      }
    }

    val rand = listOf(for {
      x <- arbitrary[Int]
    } yield Identity(x))

    def plus[A <: Int](x: A)(b: Identity[A]): Identity[Int] = b match {
      case Identity(c) => Identity(c + x)
    }

    property("Instances of Func 1 id") = forAll(rand) {
      functorIdentity(_)
    }

    property("Instances of Func 1 comp") = forAll(rand) {
      functorCompose(plus(1)(_), plus(2)(_), _)
    }

  }

  { // 2

    final case class Pair[A](a1: A, a2: A)

    implicit val myFunctor: Functor[Pair] = new Functor[Pair] {
      def map[A, B](fa: Pair[A])(f: A => B): Pair[B] = fa match {
        case Pair(a1, a2) => Pair(f(a1), f(a2))
      }
    }

    val rand: Gen[List[Pair[Int]]] = listOf(for {
      x <- arbitrary[Int]
      y <- arbitrary[Int]
    } yield Pair(x, y))

    def plus[A <: Int](x: A)(b: Pair[A]): Pair[Int] = b match {
      case Pair(a, b) => Pair(a, b + x)
    }

    property("Instances of Func 1 id") = forAll(rand) {
      functorIdentity(_)
    }

    property("Instances of Func 2 comp") = forAll(rand) {
      functorCompose(plus(1)(_), plus(2)(_), _)
    }
  }

  { // 3

    final case class Two[A, B](a: A, b: B)

    implicit def myFunctor[X]: Functor[({type T[A] = Two[X, A]})#T] = new Functor[({type T[A] = Two[X, A]})#T] {

      override def map[A, B](fa: Two[X, A])(f: A => B): Two[X, B] = fa match {
        case Two(a1, a2) => Two(a1, f(a2))
      }
    }

    val rand: Gen[List[Two[Int, Int]]] = listOf(for {
      x <- arbitrary[Int]
      y <- arbitrary[Int]
    } yield Two(x, y))

    def plus[A <: Int, B <: Int](x: B)(z: Two[A, B]): Two[A, Int] = z match {
      case Two(a, b) => Two(a, b + x)
    }

    property("Instances of Func 2 id") = forAll(rand) {
      functorIdentity(_)
    }

    property("Instances of Func 2 comp") = forAll(rand) {
      functorCompose(plus(1)(_: Two[Int, Int]), plus(2)(_: Two[Int, Int]), _)
    }
  }

  { // 4

    final case class Three[A, B, C](a: A, b: B, c: C)

    implicit def myFunctor[X, Y]: Functor[({type T[A] = Three[X, Y, A]})#T] = new Functor[({type T[A] = Three[X, Y, A]})#T] {

      override def map[A, B](fa: Three[X, Y, A])(f: A => B): Three[X, Y, B] = fa match {
        case Three(a1, a2, a3) => Three(a1, a2, f(a3))
      }
    }

    val rand: Gen[List[Three[Int, Int, Int]]] = listOf(for {
      x <- arbitrary[Int]
      y <- arbitrary[Int]
      z <- arbitrary[Int]
    } yield Three(x, y, z))

    def plus[A, B <: Int, C <: Int](x: C)(z: Three[A, B, C]): Three[A, B, Int] = z match {
      case Three(a, b, c) => Three(a, b, c + x)
    }

    property("Instances of Func 3 id") = forAll(rand) {
      functorIdentity(_)
    }

    property("Instances of Func 3 comp") = forAll(rand) {
      functorCompose(plus(1)(_: Three[Int, Int, Int]), plus(2)(_: Three[Int, Int, Int]), _)
    }
  }

  { // 5

    final case class Three1[A, B](a: A, b: A, c: B)

    implicit def myFunctor[X]: Functor[({type T[A] = Three1[X, A]})#T] = new Functor[({type T[A] = Three1[X, A]})#T] {

      override def map[A, B](fa: Three1[X, A])(f: A => B): Three1[X, B] = fa match {
        case Three1(a1, a2, a3) => Three1(a1, a2, f(a3))
      }
    }

    val rand: Gen[List[Three1[Int, Int]]] = listOf(for {
      x <- arbitrary[Int]
      y <- arbitrary[Int]
      z <- arbitrary[Int]
    } yield Three1(x, y, z))

    def plus[A, B <: Int, C <: Int](x: C)(z: Three1[A, B]): Three1[A, Int] = z match {
      case Three1(a, b, c) => Three1(a, b, c + x)
    }

    property("Instances of Func 3 id") = forAll(rand) {
      functorIdentity(_)
    }

    property("Instances of Func 3 comp") = forAll(rand) {
      functorCompose(plus(1)(_: Three1[Int, Int]), plus(2)(_: Three1[Int, Int]), _)
    }
  }

  { // 6

    final case class Four[A, B, C, D](a: A, b: B, c: C, d: D)

//    implicit def myFunctor[X, Y, Z]: Functor[({type T[A] = Four[X, Y, Z, A]})#T] = new Functor[({type T[A] = Four[X, Y, Z, A]})#T] {
//
//      override def map[A, B](fa: Four[X, Y, Z, A])(f: A => B): Four[X, Y, Z, B] = fa match {
//        case Four(a1, a2, a3, a4) => Four(a1, a2, a3, f(a4))
//      }
//    }

    val rand: Gen[List[Four[String, Byte, Char, Int]]] = listOf(for {
      x <- arbitrary[String]
      y <- arbitrary[Byte]
      z <- arbitrary[Char]
      k <- arbitrary[Int]
    } yield Four(x, y, z, k))

    def plus[A, B, C, D <: Int](x: D)(z: Four[A, B, C, D]): Four[A, B, C, Int] = z match {
      case Four(a, b, c, d) => Four(a, b, c, d + x)
    }

    property("Instances of Func 3 id") = forAll(rand) {
      functorIdentity(_)
    }

    property("Instances of Func 3 comp") = forAll(rand) {
      functorCompose(plus(1)(_: Four[String, Byte, Char, Int]), plus(2)(_: Four[String, Byte, Char, Int]), _)
    }
  }

  { // 7

    final case class Four1[A, B](a: A, b: A, c: A, d: B)

    implicit def myFunctor[X]: Functor[({type T[A] = Four1[X, A]})#T] = new Functor[({type T[A] = Four1[X, A]})#T] {

      override def map[A, B](fa: Four1[X, A])(f: A => B): Four1[X, B] = fa match {
        case Four1(a1, a2, a3, a4) => Four1(a1, a2, a3, f(a4))
      }
    }

    val rand = listOf(for {
      x <- arbitrary[String]
      y <- arbitrary[String]
      z <- arbitrary[String]
      k <- arbitrary[Int]
    } yield Four1(x, y, z, k))

    def plus[A, B <: Int](x: B)(z: Four1[A, B]): Four1[A, Int] = z match {
      case Four1(a, b, c, d) => Four1(a, b, c, d + x)
    }

    property("Instances of Func 3 id") = forAll(rand) {
      functorIdentity(_)
    }

    property("Instances of Func 3 comp") = forAll(rand) {
      functorCompose(plus(1)(_: Four1[String, Int]), plus(2)(_: Four1[String, Int]), _)
    }
  }

  { // Possibly

    trait Possibly[+A]

    final case class Yeppers[A](a: A) extends Possibly[A]
    case object LolNope extends Possibly[Nothing]

    implicit val myFunctor: Functor[Possibly] = new Functor[Possibly] {

      override def map[A, B](fa: Possibly[A])(f: A => B): Possibly[B] = fa match {
        case LolNope => LolNope
        case Yeppers(a) => Yeppers(f(a))
      }
    }

    val rand = listOf(for {
      x <- arbitrary[Int]
    } yield Yeppers(x))

    def plus[A <: Int](x: A)(z: Possibly[A]): Possibly[Int] = z match {
      case Yeppers(a) => Yeppers(a + x)
      case LolNope => LolNope
    }

    property("Possibly id") = forAll(rand) {
      functorIdentity(_)
    }

    property("Possibly comp") = forAll(rand) {
      functorCompose(plus(1)(_: Possibly[Int]), plus(2)(_: Possibly[Int]), _)
    }

    assert(plus(1)(Yeppers(1)) == Yeppers(2))
    assert(plus(1)(LolNope) == LolNope)

  }
}

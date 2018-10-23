import cats.Semigroup
import cats.instances.all._
import cats.syntax.semigroup._
import org.scalatest.FunSuite

class Chapter15SemigroupTest extends FunSuite {

  test("Semigroup 8") {
    //8
    sealed trait Or[_]

    final case class Fst[A](a: A) extends Or[A]
    final case class Snd[B](a: B) extends Or[B]

    implicit val mySemigroup: Semigroup[Or[Int]] = (x, y) => (x, y) match {
      case (Snd(a), _) => Snd(a)
      case (_, Snd(b)) => Snd(b)
      case (_, Fst(b)) => Fst(b)
    }

    assert(((Fst(1): Or[Int]) |+| Snd(2)) == Snd(2))
    assert(((Fst(1): Or[Int]) |+| Fst(2)) == Fst(2))
    assert(((Snd(1): Or[Int]) |+| Fst(2)) == Snd(1))
    assert(((Snd(1): Or[Int]) |+| Snd(2)) == Snd(1))

  }

  test("Semigroup 9") {
    //9
    trait Combine[A, B] {
      def unCombine(a: A): B
    }

    val f = new Combine[Int, Int] {
      override def unCombine(n: Int): Int = n + 1
    }

    val g = new Combine[Int, Int] {
      override def unCombine(n: Int): Int = n - 1
    }

    implicit val mySemigroup: Semigroup[Combine[Int, Int]] = (x, y) => a => x.unCombine(a) |+| y.unCombine(a)

    val fg: Combine[Int, Int] = f |+| g
    val gf: Combine[Int, Int] = g |+| f
    val ff: Combine[Int, Int] = f |+| f

    assert(fg.unCombine(0) == 0)
    assert(fg.unCombine(10) == 20)
    assert(fg.unCombine(42) == 84)
    assert(fg.unCombine(1) == 2)
    assert(gf.unCombine(1) == 2)
    assert(ff.unCombine(1) == 4)

  }

  test("Semigroup 10") {
    //10

    trait Comp[A] {
      def unComp(a: A): A
    }

    val f = new Comp[Int] {
      override def unComp(a: Int): Int = a + 1
    }

    val g = new Comp[Int] {
      override def unComp(a: Int): Int = a - 1
    }


    implicit val mySemigroup: Semigroup[Comp[Int]] = (x, y) => a => x.unComp(a) |+| y.unComp(a)

    val fg: Comp[Int] = f |+| g
    val gf: Comp[Int] = g |+| f
    val ff: Comp[Int] = f |+| f

    assert(fg.unComp(0) == 0)
    assert(fg.unComp(10) == 20)
    assert(fg.unComp(42) == 84)
    assert(fg.unComp(1) == 2)
    assert(gf.unComp(1) == 2)
    assert(ff.unComp(1) == 4)

  }

  test("Semigroup 11") {
    //11
    /*
        data Validation a b =  Failure a | Success b deriving (Eq, Show)

        instance Semigroup a => Semigroup (Validation a b) where
          Success x <> _ = Success x
          _         <> Success x = Success x
          Failure a <> Failure b = Failure (a <> b)
    */

    sealed trait Validation[A, B]

    final case class Failure[A, B](a: A) extends Validation[A, B]
    final case class Success[A, B](b: B) extends Validation[A, B]

    implicit val mySemigroup: Semigroup[Validation[String, Int]] = new Semigroup[Validation[String, Int]] {

      def combine(x: Validation[String, Int], y: Validation[String, Int]): Validation[String, Int] = (x, y) match {
        case (Failure(a), Failure(b)) => Failure(a |+| b)
        case (Success(a), _) => Success(a)
        case (_, Success(a)) => Success(a)

      }
    }

    assert(((Failure("ko"): Validation[String, Int]) |+| (Success(1): Validation[String, Int])) == Success(1))
    assert(((Failure("ko1"): Validation[String, Int]) |+| (Failure("ko2"): Validation[String, Int])) == Failure("ko1ko2"))

    assert(((Success(1): Validation[String, Int]) |+| (Success(2): Validation[String, Int])) == Success(1))
  }

  test("Semigroup 12") {
    //12
    /*
    newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

    instance Semigroup b => Semigroup (AccumulateRight a b) where
      (<>) (AccumulateRight (Success' b)) (AccumulateRight (Success' b'))  = AccumulateRight $ Success' (b <> b')
      (<>) (AccumulateRight (Success' _)) (AccumulateRight (Failure' a))   = AccumulateRight $ Failure' a
      (<>) (AccumulateRight (Failure' a)) _ = AccumulateRight $ Failure' a
    */

    trait Validation[A, B]
    final case class Failure[A, B](a: A) extends Validation[A, B]
    final case class Success[A, B](b: B) extends Validation[A, B]

    final case class AccumulateRight[A, B](v: Validation[A, B])

    implicit val mySemigroup: Semigroup[AccumulateRight[String, Int]] = new Semigroup[AccumulateRight[String, Int]] {

      def combine(x: AccumulateRight[String, Int], y: AccumulateRight[String, Int]): AccumulateRight[String, Int] = (x, y) match {
        case (AccumulateRight(Failure(a)), _) => AccumulateRight(Failure(a))
        case (AccumulateRight(Success(a)), _) => AccumulateRight(Success(a))
        case (_, AccumulateRight(Success(a))) => AccumulateRight(Success(a))

      }
    }

    val x = ((AccumulateRight(Failure("ko")): AccumulateRight[String, Int]) |+| (AccumulateRight(Success(1)): AccumulateRight[String, Int]))
    val y: AccumulateRight[String, Int] = AccumulateRight(Failure("ko"))
    assert(x == y)
  }

  test("Semigroup 13") {
    //13
    /*
        newtype AccumulateBoth a b =  AccumulateBoth (Validation a b) deriving (Eq, Show)
        instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
          (<>) (AccumulateBoth (Success' a))     (AccumulateBoth (Success' b)) = AccumulateBoth $ Success' (a <> b)
          (<>) (AccumulateBoth (Success' _))     (AccumulateBoth (Failure' a)) = AccumulateBoth $ Failure' a
          (<>) (AccumulateBoth (Failure' a))     (AccumulateBoth (Success' _)) = AccumulateBoth $ Failure' a
          (<>) (AccumulateBoth (Failure' a))     (AccumulateBoth (Failure' b)) = AccumulateBoth $ Failure' (a <> b)
      */


    trait Validation[A, B]
    final case class Failure[A, B](a: A) extends Validation[A, B]
    final case class Success[A, B](b: B) extends Validation[A, B]

    final case class AccumulateBoth[A, B](v: Validation[A, B])

    implicit val mySemigroup: Semigroup[AccumulateBoth[String, Int]] = new Semigroup[AccumulateBoth[String, Int]] {

      def combine(x: AccumulateBoth[String, Int], y: AccumulateBoth[String, Int]): AccumulateBoth[String, Int] = (x, y) match {
        case (AccumulateBoth(Failure(a)), AccumulateBoth(Failure(b))) => AccumulateBoth(Failure(a |+| b))
        case (AccumulateBoth(Success(_)), AccumulateBoth(Failure(a))) => AccumulateBoth(Failure(a))
        case (AccumulateBoth(Failure(a)), AccumulateBoth(Success(_))) => AccumulateBoth(Failure(a))
        case (AccumulateBoth(Success(a)), AccumulateBoth(Success(b))) => AccumulateBoth(Success(a |+| b))

      }
    }

    {
      val x = ((AccumulateBoth(Failure("ko")): AccumulateBoth[String, Int]) |+| (AccumulateBoth(Success(1)): AccumulateBoth[String, Int]))
      val y: AccumulateBoth[String, Int] = AccumulateBoth(Failure("ko"))
      assert(x == y)
    }

    {
      val x = ((AccumulateBoth(Failure("ko1")): AccumulateBoth[String, Int]) |+| (AccumulateBoth(Failure("ko2")): AccumulateBoth[String, Int]))
      val y: AccumulateBoth[String, Int] = AccumulateBoth(Failure("ko1ko2"))
      assert(x == y)
    }

    {
      val x = ((AccumulateBoth(Success(1)): AccumulateBoth[String, Int]) |+| (AccumulateBoth(Success(2)): AccumulateBoth[String, Int]))
      val y: AccumulateBoth[String, Int] = AccumulateBoth(Success(3))
      assert(x == y)
    }
  }
}
import cats.Monoid
import cats.instances.all._
import cats.syntax.semigroup._
import org.scalatest.FunSuite

class Chapter15MonoidTest extends FunSuite {


  test("Monoid 6") {

    trait Combine[A, B] {
      def unCombine(a: A): B
    }

    val f = new Combine[Int, Int] {
      override def unCombine(n: Int): Int = n + 1
    }

    val g = new Combine[Int, Int] {
      override def unCombine(n: Int): Int = n - 1
    }

    implicit val myMonoid: Monoid[Combine[Int, Int]] = new Monoid[Combine[Int, Int]] {
      override def empty: Combine[Int, Int] = _ => 0

      override def combine(x: Combine[Int, Int], y: Combine[Int, Int]): Combine[Int, Int] = a => x.unCombine(a) |+| y.unCombine(a)
    }

    val fg: Combine[Int, Int] = f |+| g
    val gf: Combine[Int, Int] = g |+| f
    val ff: Combine[Int, Int] = f |+| f
    val fe: Combine[Int, Int] = f |+| Monoid[Combine[Int, Int]].empty
    val ge: Combine[Int, Int] = g |+| Monoid[Combine[Int, Int]].empty

    assert(fg.unCombine(0) == 0)
    assert(fg.unCombine(10) == 20)
    assert(fg.unCombine(42) == 84)
    assert(fg.unCombine(1) == 2)
    assert(gf.unCombine(1) == 2)
    assert(ff.unCombine(1) == 4)

    assert(fe.unCombine(1) == 2)
    assert(ge.unCombine(1) == 0)

  }

  test("Monoid 7") {

    trait Comp[A] {
      def f(a: A): A
    }

    implicit val myMonoid: Monoid[Comp[Int]] = new Monoid[Comp[Int]] {
      override def empty: Comp[Int] = _ => 0

      override def combine(x: Comp[Int], y: Comp[Int]): Comp[Int] = a => x.f(a) |+| y.f(a)
    }

    def f1: Comp[Int] = n => n + 1

    val fe: Comp[Int] = f1 |+| Monoid[Comp[Int]].empty
    val ff: Comp[Int] = f1 |+| f1
    assert(fe.f(1) == 2)
    assert(ff.f(1) == 4)
    assert(fe.f(1) == 2)
  }

}

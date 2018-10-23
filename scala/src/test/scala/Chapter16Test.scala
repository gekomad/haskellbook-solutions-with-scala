import org.scalatest.FunSuite

class Chapter16Test extends FunSuite {

  test("Functor Four") { // 6

    import cats.Functor
    import cats.instances.list._

    final case class Four[A, B, C, D](a: A, b: B, c: C, d: D)


    def plus[A, B, C, D <: Int](x: D)(z: Four[A, B, C, D]): Four[A, B, C, Int] = z match {
      case Four(a, b, c, d) => Four(a, b, c, d + x)
    }

    def functorCompose[F[_] : Functor, A, B, C](f: A => B, g: B => C, fa: F[A]): F[C] = {
      val ff = Functor[F].map(fa)(f)
      Functor[F].map(ff)(g)
    }

    type MyFourType = Four[String, Byte, Char, Int]

    val o = functorCompose(plus(1)(_: MyFourType), plus(2)(_: MyFourType), List(Four("hi", 2: Byte, 'a', 4), Four("ho", 4: Byte, 'b', 2)))

    assert(o == List(Four("hi", 2: Byte, 'a', 7), Four("ho", 4: Byte, 'b', 5)))
  }
}

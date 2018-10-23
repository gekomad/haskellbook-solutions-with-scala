import org.scalatest.FunSuite

class Chapter20Test extends FunSuite {


  test("Two a b") {

    /*
    data Two a b = Two a b deriving (Eq, Show)

    instance Foldable (Two a) where
      foldMap f (Two _ b) = f b

    fold (+) Two (0 0) [Two (1 3)]
    * */


    import cats.Monoid

    final case class Two[A, B](a: A, b: B)

    type TwoStringIntType = Two[String, Int]
    implicit val twoAdditionMonoid: Monoid[TwoStringIntType] = new Monoid[TwoStringIntType] {
      override def empty: TwoStringIntType = Two("", 0)

      override def combine(x: TwoStringIntType, y: TwoStringIntType): TwoStringIntType = Two(x.a + y.a, x.b + y.b)
    }

    import cats.implicits._

    assert(List.empty[TwoStringIntType].combineAll == Two("", 0))

    assert(List(Two("a", 1), Two("b", 2)).combineAll == Two("ab", 3))

  }
}

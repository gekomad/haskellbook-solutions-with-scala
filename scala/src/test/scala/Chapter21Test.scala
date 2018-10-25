
import org.scalatest.FunSuite

class Chapter21Test extends FunSuite {

  test("traverse Optional") {

    sealed trait Optional[+A]
    case object Nada extends Optional[Nothing]
    final case class Yep[A](a: A) extends Optional[A]

    import cats._
    import cats.instances.list._

    implicit val df: Applicative[Optional] = new Applicative[Optional] {
      override def pure[A](x: A): Optional[A] = Yep(x)

      override def ap[A, B](ff: Optional[A => B])(fa: Optional[A]): Optional[B] = (fa, ff) match {
        case (Yep(a), Yep(b)) => Yep(b(a))
        case _ => Nada
      }
    }

    //    implicit val myFunctor: Functor[Optional] = new Functor[Optional] {
    //      def map[A, B](fa: Optional[A])(f: A => B): Optional[B] = fa match {
    //        case Nada => Nada
    //        case Yep(a) => Yep(f(a))
    //      }
    //    }

    val emptyList = List.empty[List[Optional[Int]]]
    val myList = List(Yep(1), Yep(2), Yep(3)): List[Optional[Int]]
    val myList2 = List(Yep(1), Yep(2), Nada): List[Optional[Int]]

    assert(Traverse[List].sequence(myList) == Yep(List(1, 2, 3)))
    assert(Traverse[List].sequence(myList2) == Nada)
    //    assert(Traverse[List].sequence(emptyList) == Yep(emptyList)) TODO


  }

  test("traverse option") {
    import cats._
    import cats.instances.list._
    import cats.instances.option._
    val x: List[Option[Int]] = List()
    assert(Traverse[List].sequence(x) == Option(List()))

  }


}

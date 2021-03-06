import cats.{Monoid, Semigroup}

object MyPredef {

  import scala.math.Ordered._

  def words(s: String): Seq[String] = s.split(" ").toList

  def mySplit(s: String, sep: String = " "): Seq[String] = {
    val ss = s.split(sep).toList
    if (s.endsWith(sep)) ss ++ List("") else ss
  }

  def max[A: Ordering](a: A, b: A): A = (a, b) match {
    case ("", b) => b
    case (a, "") => a
    case (a, b) => if (a.compare(b) > 0) a else b
  }

  def min[A: Ordering](a: A, b: A): A = (a, b) match {
    case ("", b) => b
    case (a, "") => a
    case (a, b) => if (a.compare(b) == -1) a else b
  }

  //  const :: a -> b -> a
  //  const x _ = x
  def const[A](a: A, b: Any): A = a

  //  flip                    :: (a -> b -> c) -> b -> a -> c
  //  flip f x y              =  f y x
  def flip[A, B, C](f: (A, B) => C, b: B, a: A): C = f(a, b)

  def infiniteFalse: Stream[Boolean] = false #:: infiniteFalse

  def infiniteTrue: Stream[Boolean] = true #:: infiniteTrue

  def even(number: Int): Boolean = number % 2 == 0

  def odd(number: Int): Boolean = !even(number)

  def capitalize(s: String): String = s.capitalize

  implicit val boolConjMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = (x, y) match {
      case (true, true) => true
      case _ => false
    }
  }

  implicit val boolDisjMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = (x, y) match {
      case (false, false) => false
      case _ => true
    }
  }

}

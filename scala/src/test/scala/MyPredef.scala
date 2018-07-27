object MyPredef {

  import scala.math.Ordered._

  def words(s: String): List[String] = s.split(" ").toList

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
  def flip[A, B, C](f: ((A, B) => C), b: B, a: A): C = f(a, b)

  def infiniteFalse: Stream[Boolean] = false #:: infiniteFalse

  def infiniteTrue: Stream[Boolean] = true #:: infiniteTrue

  def even(number: Int): Boolean = number % 2 == 0

  def odd(number: Int): Boolean = !even(number)
}


import cats.Eq
import cats.Order
import org.scalatest.FunSuite


sealed trait DayOfWeek

case object Mon extends DayOfWeek

case object Tue extends DayOfWeek

case object Weds extends DayOfWeek

case object Thu extends DayOfWeek

case object Fri extends DayOfWeek

case object Sat extends DayOfWeek

case object Sun extends DayOfWeek

final case class Cat(name: String, age: Int, color: String)

final case class Foo(foo: Int)


object testEq {

  import cats.Eq

  import cats.syntax.eq._ // for === and =!=

  import cats.instances.string._
  import cats.instances.int._

  implicit val _1: Eq[Cat] =
    Eq.instance[Cat] { (c1, c2) =>
      c1.name === c2.name &&
        c1.age === c2.age &&
        c1.color === c2.color
    }

  implicit val _2: Eq[Foo] =
    Eq.instance[Foo] { (c1, c2) =>
      c1.foo === c2.foo
    }

  implicit val _3: Eq[DayOfWeek] =
    Eq.instance[DayOfWeek] { (c1, c2) =>
      c1 == c2
    }

  assert(Cat("Garfield", 38, "ginger and black") === Cat("Garfield", 38, "ginger and black"))
  assert(Cat("Garfield", 38, "ginger and black") =!= Cat("Garfield", 1, "ginger and black"))


  val mon: DayOfWeek = Mon
  val sun: DayOfWeek = Sun
  val tue: DayOfWeek = Tue

  assert(Foo(3) === Foo(3))
  assert(Foo(3) =!= Foo(2))
  assert(mon === mon)
  assert(sun === sun)
  assert(sun =!= tue)
  assert(1 =!= 2)

}

//object testOrd {
//
//
//  case class Person(first: String, middle: String, last: String)
//
//  implicit val ord = new Ordering[Person] {
//    def compare(self: Person, that: Person): Int =
//      (self.last compare that.last) match {
//        case 0 =>
//          (self.first compare that.first) match {
//            case 0 => self.middle compare that.middle
//            case c => c
//          }
//        case c => c
//      }
//  }
//
//  val personList = List(Person("john", "a", "smith"), Person("steve", "x", "scott"), Person("bill", "w", "smith"))
//  val a = personList.sorted
//  val a1 = personList.min
//  val a2 = personList.max
//
//  println
//
//}


class Chapter6Test extends FunSuite {

  test("scala standard") {

    trait Show[A] {
      def show(a: A): String
    }

    object Show {

      def apply[A](implicit sh: Show[A]): Show[A] = sh

      object ops {
        def show[A: Show](a: A) = Show[A].show(a)

        implicit class ShowOps[A: Show](a: A) {
          def show = Show[A].show(a)
        }

      }

    }

    import Show.ops._

    implicit object _1 extends Show[Int] {
      override def show(int: Int): String = s"int: $int"
    }

    implicit object _2 extends Show[String] {
      override def show(str: String): String = s"string: $str"
    }

    implicit object _3 extends Show[DayOfWeek] {
      override def show(str: DayOfWeek): String = s"DayOfWeek: $str"
    }

    implicit object _4 extends Show[Foo] {
      override def show(str: Foo): String = s"Foo: ${str.foo.show}"
    }

    case class Foo(foo: Int)

    val mon: DayOfWeek = Mon
    val sun: DayOfWeek = Sun

    assert(Foo(3).show == "Foo: int: 3")
    assert(mon.show == "DayOfWeek: Mon")
    assert(sun.show == "DayOfWeek: Sun")
    assert(30.show == "int: 30")
  }

  test("cats show") {
    println(" -- cats show -- ")
    import cats.Show
    import cats.Order
    import cats.instances.int._
    import cats.instances.string._
    import cats.syntax.show._

    implicit val catShow = Show.show[Cat] { cat =>
      val name = cat.name.show
      val age = cat.age.show
      val color = cat.color.show
      s"$name is a $age year-old $color cat."
    }

    implicit val dateEq: Eq[Cat] =
      Eq.instance[Cat] { (cat1, cat2) =>
        cat1 === cat2
      }

    assert(Cat("Garfield", 38, "ginger and black").show == "Garfield is a 38 year-old ginger and black cat.")


    implicit val _1 = cats.Show.show[DayOfWeek] { a => s"** $a **" }
    implicit val _2 = cats.Show.show[Int] { a => s"** $a **" }
    implicit val _3 = cats.Show.show[Foo] { a => s"** $a **" }
    implicit val _4 = cats.Show.show[String] { a => s"** $a **" }

    val mon: DayOfWeek = Mon
    val sun: DayOfWeek = Sun

    //assert(30.show == "** 30 **") TODO

    assert(Foo(3).show == "** Foo(3) **")
    assert(mon.show == "** Mon **")
    assert(sun.show == "** Sun **")

  }


  test("cats eq") {
    println(" -- cats eq -- ")

    testEq

  }

//  test("cats ord") {
//    println(" -- cats ord -- ")
//
//    testOrd
//
//  }
}
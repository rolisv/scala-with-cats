package sandbox.cats

object AppPrintable {

  final case class Cat(name: String, age: Int, color: String)

  import cats.Show
  import cats.instances.string._
  import cats.instances.int._
  import cats.syntax.show._
  implicit val catPrn: Show[Cat] =
    (v: Cat) => {
      val name = v.name.show
      val age = v.age.show
      val color = v.color.show

      s"$name is a $age year-old $color cat"
    }

  import cats.Eq
  import cats.syntax.eq._
  implicit val catEq: Eq[Cat] = (c1: Cat, c2: Cat) => {
    c1.name === c2.name && c1.age === c2.age && c1.color === c2.color
  }

  final case class Box[A](v: A)
  implicit def boxPrn[A](implicit prnA: Printable[A]): Printable[Box[A]] =
    prnA.contramap(_.v)

  def main(args: Array[String]): Unit = {
    val c = Cat("bz", 12, "orange")

    println(s"syntax show formatted: ${c.show}")

    val c1 = Cat("Gf", 38, "orange and black")
    val c2 = Cat("Hc", 33, "orange and black")
    val optC1 = Option(c1)
    val optC2 = Option(c2)

    println(s"c1 == c2? ${c1 === c2}")

    import cats.instances.option._
    println(s"opt(c1) == opt(c2)? ${optC1 === optC2}")
  }
}

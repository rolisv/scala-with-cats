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

  def main(args: Array[String]): Unit = {
    val c = Cat("bz", 12, "orange")

    println(s"syntax show formatted: ${c.show}")
  }
}

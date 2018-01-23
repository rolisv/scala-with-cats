package sandbox.cats

object AppPrintable {

  final case class Cat(name: String, age: Int, color: String)

  import PrintableInstances._
  implicit val catPrn: Printable[Cat] =
    (v: Cat) => {
      val name = Printable.format(v.name)
      val age = Printable.format(v.age)
      val color = Printable.format(v.color)

      s"$name is a $age year-old $color cat"
    }

  def main(args: Array[String]): Unit = {
    val c = Cat("bz", 12, "orange")
    println(s"formatted: ${Printable.format(c)}")
    Printable.print(c)

    import PrintableSyntax._
    println(s"syntax formatted: ${c.format}")
    println("syntax print:")
    c.print
  }
}

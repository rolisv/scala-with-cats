package sandbox.cats

trait Printable[A] {
  def format(v: A): String
}

object PrintableInstances {
  implicit val strPrintable: Printable[String] = (v: String) => v

  implicit val intPrintable: Printable[Int] = (v: Int) => v.toString
}

object Printable {
  def format[A](v: A)(implicit prn: Printable[A]): String = prn.format(v)

  def print[A](v: A)(implicit prn: Printable[A]): Unit = println(format(v))
}

object PrintableSyntax {
  implicit class PrintableOps[A](o: A) {
    def format(implicit prn: Printable[A]): String = Printable.format(o)

    def print(implicit prn: Printable[A]): Unit = Printable.print(o)
  }
}

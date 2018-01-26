package sandbox.cats

trait Printable[A] { self =>
  def format(v: A): String

  def contramap[B](f: B => A): Printable[B] = (v: B) => self.format(f(v))
}

object PrintableInstances {
  implicit val strPrintable: Printable[String] = (v: String) => v

  implicit val intPrintable: Printable[Int] = (v: Int) => v.toString

  implicit val boolPrn: Printable[Boolean] = (v: Boolean) => if (v) "yes" else "no"
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

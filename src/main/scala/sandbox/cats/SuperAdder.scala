package sandbox.cats

object SuperAdder {

  import cats.Monoid

  def add[A](items: List[A])(implicit m: Monoid[A]): A = {
    items.fold(m.empty)(m.combine)
  }
}

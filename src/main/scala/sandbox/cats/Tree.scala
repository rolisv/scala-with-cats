package sandbox.cats

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](v: A) extends Tree[A]

object Tree {
  import cats.Functor
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(v)      => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def leaf[A](v: A): Tree[A] = Leaf(v)
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
}

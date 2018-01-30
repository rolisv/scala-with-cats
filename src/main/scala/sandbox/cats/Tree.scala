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

  import cats.Monad
  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = f(a) match {
      case Leaf(Left(a))  => tailRecM(a)(f)
      case Leaf(Right(b)) => Leaf(b)
      case Branch(l, r)   => ???
    }
  }

  def leaf[A](v: A): Tree[A] = Leaf(v)
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
}

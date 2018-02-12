package sandbox.cats.casestudy

import cats.Monoid

trait BoundedSemiLattice[A] extends Monoid[A] {
  def combine(x: A, y: A): A
  def empty: A
}

object BoundedSemiLattice {
  implicit val intBsl: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
    override def combine(x: Int, y: Int): Int = x max y

    override def empty: Int = 0
  }

  implicit def setsBsl[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y

    override def empty: Set[A] = Set.empty
  }
}

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: Monoid[V]): V
}

object GCounter {

  def apply[F[_, _], K, V](implicit c: GCounter[F, K, V]): GCounter[F, K, V] = c

  implicit def mapGCounter[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {

    import cats.syntax.semigroup._

    override def increment(f: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] = {
      val newV = f.getOrElse(k, m.empty) |+| v
      f + (k -> newV)
    }

    import cats.instances.map._

    override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
      f1 |+| f2

    import Crdt._
    import cats.syntax.foldable._

    override def total(f: Map[K, V])(implicit m: Monoid[V]): V = f.values.combineAll
  }
}

object Crdt {

  import cats.{Eval, Foldable}

  implicit val iterableFoldable: Foldable[Iterable] = new Foldable[Iterable] {
    override def foldLeft[A, B](fa: Iterable[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    override def foldRight[A, B](fa: Iterable[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Eval.now(fa.foldRight(lb.value)((a, b) => f(a, Eval.now(b)).value))
  }
}

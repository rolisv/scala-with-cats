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

final case class GCounter[D](counters: Map[String, D]) {

  import cats.syntax.semigroup._

  def increment(machine: String, amount: D)(implicit m: Monoid[D]): GCounter[D] = {
    val newAmount = counters.getOrElse(machine, m.empty) |+| amount
    copy(counters = counters + (machine -> newAmount))
  }

  import cats.instances.map._

  def merge(that: GCounter[D])(implicit b: BoundedSemiLattice[D]): GCounter[D] =
    GCounter(counters |+| that.counters)

  import cats.{Eval, Foldable}
  import cats.syntax.foldable._

  implicit val iterableFoldable: Foldable[Iterable] = new Foldable[Iterable] {
    override def foldLeft[A, B](fa: Iterable[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    override def foldRight[A, B](fa: Iterable[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Eval.now(fa.foldRight(lb.value)((a, b) => f(a, Eval.now(b)).value))
  }

  def total(implicit m: Monoid[D]): D = counters.values.combineAll
}

object Crdt {}

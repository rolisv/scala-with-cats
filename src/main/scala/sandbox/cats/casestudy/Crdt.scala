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

trait KeyValueStore[F[_, _]] {
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def getOrElse[K, V](f: F[K, V])(k: K, default: => V): V =
    get(f)(k).getOrElse(default)

  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def values[K, V](f: F[K, V]): Iterable[V]
}

object KeyValueStore {
  implicit val mapInstance: KeyValueStore[Map] = new KeyValueStore[Map] {
    override def values[K, V](f: Map[K, V]): Iterable[V] = f.values

    override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

    override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    override def getOrElse[K, V](f: Map[K, V])(k: K, default: => V): V = f.getOrElse(k, default)
  }

  implicit class KvsOps[F[_, _], K, V](val f: F[K, V]) extends AnyVal {
    def get(k: K)(implicit kvs: KeyValueStore[F]): Option[V] =
      kvs.get(f)(k)

    def getOrElse(k: K, default: => V)(implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(k, default)

    def put(k: K, v: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(k, v)

    def values(implicit kvs: KeyValueStore[F]): Iterable[V] =
      kvs.values(f)
  }
}

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: Monoid[V]): V
}

object GCounter {

  def apply[F[_, _], K, V](implicit c: GCounter[F, K, V]): GCounter[F, K, V] = c

  implicit def gCounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F],
                                               km: Monoid[F[K, V]]): GCounter[F, K, V] =
    new GCounter[F, K, V] {
      import KeyValueStore._
      import cats.syntax.semigroup._

      override def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V] = {
        val newV = f.getOrElse(k, m.empty) |+| v
        f.put(k, newV)
      }

      override def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] =
        f1 |+| f2

      import Crdt._
      import cats.syntax.foldable._

      override def total(f: F[K, V])(implicit m: Monoid[V]): V =
        f.values.combineAll
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

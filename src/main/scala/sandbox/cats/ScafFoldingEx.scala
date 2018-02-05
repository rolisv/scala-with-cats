package sandbox.cats

import cats.Monoid

object ScafFoldingEx {

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    xs.foldRight(List.empty[B])((a, bs) => f(a) :: bs)

  def sum[A: Monoid](xs: List[A]): A =
    xs.foldRight(Monoid[A].empty)(Monoid[A].combine)

  def filter[A](xs: List[A])(p: A => Boolean): List[A] =
    xs.foldRight(List.empty[A])((a, as) => if (p(a)) a :: as else as)

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
    xs.foldRight(List.empty[B])((a, bs) => f(a) ::: bs)
}

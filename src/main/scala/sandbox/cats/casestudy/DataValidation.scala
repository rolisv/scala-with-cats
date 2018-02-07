package sandbox.cats.casestudy

import cats.Semigroup
import cats.Semigroupal
import cats.syntax.either._
import cats.syntax.semigroup._

trait CheckF[E, A] { self =>

  def apply(a: A): Either[E, A]

  def and(that: CheckF[E, A])(implicit esemi: Semigroup[E]): CheckF[E, A] =
    (a: A) =>
      Semigroupal
        .map2(
          self.apply(a).toValidated,
          that.apply(a).toValidated
        )((a0, a1) => a0)
        .toEither
}

sealed trait Check[E, A] {

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def apply(a: A)(implicit se: Semigroup[E]): Either[E, A] = this match {
    case Pure(f) => f(a)
    case And(l, r) =>
      (l(a), r(a)) match {
        case (Left(e1), Left(e2))   => (e1 |+| e2).asLeft
        case (Left(e), Right(a))    => e.asLeft
        case (Right(a), Left(e))    => e.asLeft
        case (Right(a1), Right(a2)) => a.asRight
      }
  }
}

final case class Pure[E, A](chk: A => Either[E, A]) extends Check[E, A]
final case class And[E, A](l: Check[E, A], r: Check[E, A]) extends Check[E, A]

object DataValidation {}

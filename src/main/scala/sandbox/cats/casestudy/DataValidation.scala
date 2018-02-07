package sandbox.cats.casestudy

import cats.data.Validated.{Invalid, Valid}
import cats.{Semigroup, Semigroupal}
import cats.syntax.either._

trait CheckF[E, A] { self =>

  def apply(a: A): Either[E, A]

  def and(that: CheckF[E, A])(implicit se: Semigroup[E]): CheckF[E, A] =
    (a: A) =>
      Semigroupal
        .map2(
          self.apply(a).toValidated,
          that.apply(a).toValidated
        )((a0, a1) => a0)
        .toEither
}

import cats.data.Validated
import cats.syntax.semigroup._

sealed trait Check[E, A] {

  def and(that: Check[E, A]): Check[E, A] = And(this, that)
  def or(that: Check[E, A]): Check[E, A] = Or(this, that)

  def apply(a: A)(implicit se: Semigroup[E]): Validated[E, A] = this match {
    case Pure(f)   => f(a)
    case And(l, r) => Semigroupal.map2(l(a), r(a))((_, _) => a)
    case Or(l, r) =>
      l(a) match {
        case v @ Valid(_) => v
        case Invalid(e1) =>
          r(a) match {
            case v @ Valid(_) => v
            case Invalid(e2)  => Invalid(e1 |+| e2)
          }
      }
  }
}

final case class Pure[E, A](chk: A => Validated[E, A]) extends Check[E, A]
final case class And[E, A](l: Check[E, A], r: Check[E, A]) extends Check[E, A]
final case class Or[E, A](l: Check[E, A], r: Check[E, A]) extends Check[E, A]

object DataValidation {}

package sandbox.cats.casestudy

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.either._
import cats.{Semigroup, Semigroupal}

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

sealed trait Predicate[E, A] {
  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

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

object Predicate {

  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
    Pure(f)

  def lift[E, A](e: E, f: A => Boolean): Predicate[E, A] =
    Pure(a => Validated.cond(f(a), a, e))

  final case class Pure[E, A](chk: A => Validated[E, A]) extends Predicate[E, A]
  final case class And[E, A](l: Predicate[E, A], r: Predicate[E, A]) extends Predicate[E, A]
  final case class Or[E, A](l: Predicate[E, A], r: Predicate[E, A]) extends Predicate[E, A]
}

sealed trait Check[E, A, B] { self =>
  import Check._

  def apply(a: A)(implicit se: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] =
    Map(this, f)

  def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] =
    FlatMap(this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
    AndThen(this, that)
}

object Check {

  def apply[E, A](p: Predicate[E, A]): Check[E, A, A] =
    Pure(p)

  final case class Pure[E, A](p: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit se: Semigroup[E]): Validated[E, A] =
      p(a)
  }

  final case class Map[E, A, B, C](c: Check[E, A, B], f: B => C) extends Check[E, A, C] {
    def apply(a: A)(implicit se: Semigroup[E]): Validated[E, C] =
      c(a).map(f)
  }

  final case class FlatMap[E, A, B, C](c: Check[E, A, B], f: B => Check[E, A, C])
      extends Check[E, A, C] {
    def apply(a: A)(implicit se: Semigroup[E]): Validated[E, C] =
      c(a).withEither(_.flatMap(b => f(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](c1: Check[E, A, B], c2: Check[E, B, C])
      extends Check[E, A, C] {
    def apply(a: A)(implicit se: Semigroup[E]): Validated[E, C] =
      c1(a).withEither(_.flatMap(b => c2(b).toEither))
  }
}

object DataValidation {}

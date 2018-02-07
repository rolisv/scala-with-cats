package sandbox.cats.casestudy

import cats.Semigroup
import cats.Semigroupal
import cats.syntax.either._

trait Check[E, A] { self =>

  def apply(a: A): Either[E, A]

  def and(that: Check[E, A])(implicit esemi: Semigroup[E]): Check[E, A] =
    (a: A) =>
      Semigroupal
        .map2(
          self.apply(a).toValidated,
          that.apply(a).toValidated
        )((a0, a1) => a0)
        .toEither
}

object DataValidation {}

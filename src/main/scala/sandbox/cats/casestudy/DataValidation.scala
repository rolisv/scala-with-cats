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

  def apply[E, A, B](f: A => Validated[E, B]): Check[E, A, B] =
    Pure(f)

  def apply[E, A](p: Predicate[E, A]): Check[E, A, A] =
    PurePredicate(p)

  final case class Pure[E, A, B](f: A => Validated[E, B]) extends Check[E, A, B] {
    def apply(a: A)(implicit se: Semigroup[E]): Validated[E, B] =
      f(a)
  }

  final case class PurePredicate[E, A](p: Predicate[E, A]) extends Check[E, A, A] {
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

object DataValidation {
  import cats.data.NonEmptyList

  type Errors = NonEmptyList[String]

  def err(msg: String) = NonEmptyList(msg, Nil)

  // Business rules:
  // - a username must contain at least 4 characters and consist entirely of alphanumeric characters
  // - an email address must contain an @ sign. Split the string at the @. The string to the left
  //   must not be empty. The string to the right must be at least three characters long and
  //   contain a dot.

  val usernameCheck = Check(atLeastLong(4) and alphaNum)

  import cats.syntax.validated._

  val emailContainsNameAndDomain: Check[Errors, String, (String, String)] =
    Check(_.split('@') match {
      case Array(n, d) => (n, d).validNel
      case _ => "Must contain a single '@' character".invalidNel
    })

  val validName = Check(atLeastLong(1))
  val validDomain = Check(atLeastLong(3) and contains('.'))

  val validNameAndDomain: Check[Errors, (String, String), String] =
    Check { case (n, d) =>
      Semigroupal.map2(validName(n), validDomain(d))(_ + '@' + _)
    }

  val emailCheck: Check[Errors, String, String] = emailContainsNameAndDomain andThen validNameAndDomain

  def atLeastLong(minLen: Int): Predicate[Errors, String] =
    Predicate.lift(err(s"Must contain at least $minLen chars"), _.length >= minLen)

  def alphaNum: Predicate[Errors, String] =
    Predicate.lift(err("Must only contain alphanumeric chars"), _.forall(_.isLetterOrDigit))

  def contains(c: Char): Predicate[Errors, String] =
    Predicate.lift(err(s"Must contain '$c'"), _.contains(c))

  def containsOnce(c: Char): Predicate[Errors, String] =
    Predicate.lift(err(s"Must contain '$c' only once"), _.count(_ == c) == 1)

  final case class User(name: String, email: String)

  def validateUser(name: String, email: String): Validated[Errors, User] =
    Semigroupal.map2(usernameCheck(name), emailCheck(email))(User)

  def main(args: Array[String]): Unit = {
    val vu1 = validateUser("ro", "user@mail.c")
    println(vu1)

    val vu2 = validateUser("", "user@mc")
    println(vu2)
  }
}

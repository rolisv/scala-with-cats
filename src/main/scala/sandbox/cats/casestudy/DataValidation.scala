package sandbox.cats.casestudy

import cats.data.Validated.{Invalid, Valid}
import cats.{Semigroup, Semigroupal}
import sandbox.cats.Util.eitherMap2

trait CheckF[E, A] { self =>

  def apply(a: A): Either[E, A]

  def and(that: CheckF[E, A])(implicit se: Semigroup[E]): CheckF[E, A] =
    (a: A) => eitherMap2(self(a), that(a))((_, _) => a)
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

  def run(implicit se: Semigroup[E]): A => Either[E, A] =
    (a: A) => apply(a).toEither
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

object DataValidation {
  import cats.data.NonEmptyList

  type Errors = NonEmptyList[String]

  def err(msg: String) = NonEmptyList(msg, Nil)

  import cats.data.Kleisli

  type Result[A] = Either[Errors, A]
  type Check[A, B] = Kleisli[Result, A, B]

  def check[A, B](f: A => Result[B]): Check[A, B] =
    Kleisli(f)
  def checkPred[A](p: Predicate[Errors, A]): Check[A, A] =
    Kleisli[Result, A, A](p.run)

  // Business rules:
  // - a username must contain at least 4 characters and consist entirely of alphanumeric characters
  // - an email address must contain an @ sign. Split the string at the @. The string to the left
  //   must not be empty. The string to the right must be at least three characters long and
  //   contain a dot.

  val usernameCheck = checkPred(atLeastLong(4) and alphaNum)

  val emailContainsNameAndDomain: Check[String, (String, String)] =
    check(_.split('@') match {
      case Array(n, d) => Right((n, d))
      case _           => Left(err("Must contain a single '@' character"))
    })

  val validName = checkPred(atLeastLong(1))
  val validDomain = checkPred(atLeastLong(3) and contains('.'))

  import cats.instances.either._

  val validNameAndDomain: Check[(String, String), String] =
    check {
      case (n, d) =>
        eitherMap2(validName(n), validDomain(d))(_ + '@' + _)
    }

  val emailCheck: Check[String, String] =
    emailContainsNameAndDomain andThen validNameAndDomain

  def atLeastLong(minLen: Int): Predicate[Errors, String] =
    Predicate.lift(err(s"Must contain at least $minLen chars"), _.length >= minLen)

  def alphaNum: Predicate[Errors, String] =
    Predicate.lift(err("Must only contain alphanumeric chars"), _.forall(_.isLetterOrDigit))

  def contains(c: Char): Predicate[Errors, String] =
    Predicate.lift(err(s"Must contain '$c'"), _.contains(c))

  def containsOnce(c: Char): Predicate[Errors, String] =
    Predicate.lift(err(s"Must contain '$c' only once"), _.count(_ == c) == 1)

  final case class User(name: String, email: String)

  def validateUser(name: String, email: String): Result[User] =
    eitherMap2(usernameCheck(name), emailCheck(email))(User)

  def main(args: Array[String]): Unit = {
    val vu1 = validateUser("ro", "user@mail.c")
    println(vu1)

    val vu2 = validateUser("", "user@mc")
    println(vu2)
  }
}

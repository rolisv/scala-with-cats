package sandbox.cats

import cats.Semigroupal
import cats.data.Validated
import cats.syntax.either._
import cats.instances.list._

case class User(name: String, age: Int)

object FormValidationEx {

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def readUser(rq: FormData): FailSlow[User] = {
    Semigroupal.map2(
      readName(rq).toValidated,
      readAge(rq).toValidated
    )(User.apply)
  }

  def readName(rq: FormData): FailFast[String] = {
    val fn = "name"
    getRequiredValue(fn)(rq)
      .flatMap(nonBlank(fn))
  }

  def readAge(rq: FormData): FailFast[Int] = {
    val fn = "age"
    getRequiredValue(fn)(rq)
      .flatMap(nonBlank(fn))
      .flatMap(parseInt(fn))
      .flatMap(nonNegative(fn))
  }

  def getValue(name: String)(rq: FormData): Option[String] =
    rq.get(name)

  def getRequiredValue(name: String)(rq: FormData): FailFast[String] =
    getValue(name)(rq).toRight(List(s"Field '$name' is not specified."))

  def parseInt(name: String)(value: String): FailFast[Int] =
    Either
      .catchOnly[NumberFormatException](value.toInt)
      .leftMap(e => List(s"Field '$name' value '$value' is not an integer."))

  def nonBlank(name: String)(value: String): FailFast[String] =
    Right(value).ensure(List(s"Field '$name' is empty."))(_.nonEmpty)

  def nonNegative(name: String)(value: Int): FailFast[Int] =
    Right(value).ensure(List(s"Field '$name' is negative."))(_ >= 0)
}

package sandbox.cats

import cats.data.Reader
import cats.syntax.applicative._

case class Db(usernames: Map[Int, String], passwords: Map[String, String])

object ReaderEx {
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(_.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      pwdOk <- username.fold(false.pure[DbReader])(checkPassword(_, password))
    } yield pwdOk

  def main(args: Array[String]): Unit = {
    val uns = Map(1 -> "ro", 2 -> "bl")
    val pwds = Map("ro" -> "ropass", "bl" -> "blpass")
    val db = Db(uns, pwds)

    val chk = checkLogin(1, "ropass")(db)
    println(s"pass Ok? $chk")
  }
}

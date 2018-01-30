package sandbox.cats

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object WorkingEx {

  def slowly[A](b: => A): A =
    try b
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val res = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"${Thread.currentThread().getName}: fact $n: $res")
    res
  }

  type Logged[A] = Writer[Vector[String], A]

  def fLogged(n: Int): Logged[Int] = {
    val res = slowly {
      if (n == 0) 1.pure[Logged] else fLogged(n - 1).map(n * _)
    }
    res.mapWritten(_ :+ s"${Thread.currentThread().getName}: fact $n: ${res.value}")
  }

  def main(args: Array[String]): Unit = {
//    println("sync:")
//    factorial(10)
//
//    println("async:")
//    Await.result(Future(factorial(10)), 5.seconds)
//
//    println("async multi:")
//    Await.result(
//      Future.sequence(Seq(Future(factorial(10)), Future(factorial(10)), Future(factorial(10)))),
//      5.seconds)

    println("async multi writer:")
    val res = Await.result(
      Future.sequence(
        Seq(Future(fLogged(10).run), Future(fLogged(10).run), Future(fLogged(10).run))),
      5.seconds)

    for {
      (log, _) <- res
      ln <- log
    } {
      println(ln)
    }
  }
}

package sandbox.cats

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

object Util {

  def timedF[A](future: Future[A]): Future[(A, FiniteDuration)] = {
    val start = System.nanoTime()
    future.map(addTime(start))
  }

  def timedF[A](b: => A): Future[(A, FiniteDuration)] = {
    val start = System.nanoTime()
    Future(b).map(addTime(start))
  }

  private def addTime[A](start: Long)(res: A): (A, FiniteDuration) =
    (res, ((System.nanoTime() - start) / 1000L).micros)

  import cats.{Semigroup, Semigroupal}

  def eitherMap2[E, A0, A1, Z](e0: Either[E, A0], e1: Either[E, A1])(f: (A0, A1) => Z)(
      implicit se: Semigroup[E]): Either[E, Z] = {
    import cats.syntax.either._
    Semigroupal.map2(e0.toValidated, e1.toValidated)(f).toEither
  }
}

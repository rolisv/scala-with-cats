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
}

package sandbox.cats.casestudy

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MapReduce {

  import cats.Monoid

  def foldMap[A, B: Monoid](fa: Vector[A])(f: A => B): B =
    fa.map(f).foldLeft(Monoid[B].empty)(Monoid[B].combine)

  def parFoldMap[A, B: Monoid](fa: Vector[A])(f: A => B): Future[B] = {
    val threadCount = sys.runtime.availableProcessors()
    val batchSize = (fa.size - 1) / threadCount + 1

    Future
      .traverse(fa.grouped(batchSize))(fab => Future(foldMap(fab)(f)))
      .map(bs => foldMap(bs.toVector)(identity))
  }

  import cats.Foldable
  import cats.Traverse
  import cats.instances.vector._
  import cats.instances.future._

  def parFoldMapCats[A, B: Monoid](fa: Vector[A])(f: A => B): Future[B] = {
    val threadCount = sys.runtime.availableProcessors()
    val batchSize = (fa.size - 1) / threadCount + 1
    val fld = Foldable[Vector]

    Traverse[Vector]
      .traverse(fa.grouped(batchSize).toVector)(fab => Future(fld.foldMap(fab)(f)))
      .map(bs => fld.combineAll(bs))
  }
}

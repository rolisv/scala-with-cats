package sandbox.cats

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.either._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationLong
import scala.concurrent.{Await, Future}

object TransformerEx {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10,
  )

  def getPowerLevel(autobot: String): Future[Either[String, Int]] = {
    Future {
      powerLevels.get(autobot).fold(s"Cannot reach $autobot".asLeft[Int])(_.asRight[String])
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Future[Either[String, Boolean]] =
    for {
      pl1 <- getPowerLevel(ally1)
      pl2 <- getPowerLevel(ally2)
    } yield
      for {
        pll1 <- pl1
        pll2 <- pl2
      } yield pll1 + pll2 >= 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val csmF = for {
      csmE <- canSpecialMove(ally1, ally2)
    } yield
      csmE match {
        case Right(true)  => s"$ally1 and $ally2 are ready to roll out!"
        case Right(false) => s"$ally1 and $ally2 need a recharge"
        case Left(e)      => e
      }

    Await.result(csmF, 1.second)
  }

  def getPowerLevelCats(autobot: String): Response[Int] =
    EitherT(Future(powerLevels.get(autobot)).map {
      case Some(pl) => pl.asRight
      case None     => s"Cannot reach $autobot".asLeft
    })

  def canSpecialMoveCats(ally1: String, ally2: String): Response[Boolean] =
    for {
      pl1 <- getPowerLevelCats(ally1)
      pl2 <- getPowerLevelCats(ally2)
    } yield pl1 + pl2 >= 15

  def tacticalReportCats(ally1: String, ally2: String): String = {
    val csmF = for {
      csmE <- canSpecialMoveCats(ally1, ally2).value
    } yield
      csmE match {
        case Right(true)  => s"$ally1 and $ally2 are ready to roll out!"
        case Right(false) => s"$ally1 and $ally2 need a recharge"
        case Left(e)      => e
      }

    Await.result(csmF, 1.second)
  }
}

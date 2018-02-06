package sandbox.cats.casestudy

import cats.Id

import scala.concurrent.Future

trait UptimeClient[F[_]] {
  def getUptime(host: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(host: String): Future[Int]
}
class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  override def getUptime(host: String): Int = hosts.getOrElse(host, 0)
}

import cats.Applicative
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._

class UptimeServer[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hosts: List[String]): F[Int] =
    hosts.traverse(client.getUptime).map(_.sum)
}

object TestingAsyncCode {
  def testTotalUptime(): Unit = {
    val hosts = Map("h1" -> 10, "h2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeServer(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum

    assert(actual == expected)
  }
}

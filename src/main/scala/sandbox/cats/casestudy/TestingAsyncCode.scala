package sandbox.cats.casestudy

import scala.concurrent.Future
import cats.Id

trait UptimeClient[F[_]] {
  def getUptime(host: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(host: String): Future[Int]
}
class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  override def getUptime(host: String): Int = hosts.getOrElse(host, 0)
}

import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._

import scala.concurrent.ExecutionContext.Implicits.global

class UptimeServer(client: UptimeClient) {
  def getTotalUptime(hosts: List[String]): Future[Int] =
    hosts.traverse(client.getUptime).map(_.sum)
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient {
  override def getUptime(host: String): Future[Int] =
    Future.successful(hosts.getOrElse(host, 0))
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

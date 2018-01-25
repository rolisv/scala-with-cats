package sandbox.cats

object AppPos {

  case class Order(totalCost: Double, quantity: Double)

  import cats.Monoid
  implicit val orderMonoid = new Monoid[Order] {
    override def empty: Order = Order(0, 0)

    override def combine(o1: Order, o2: Order): Order =
      Order(o1.totalCost + o2.totalCost, o1.quantity + o2.quantity)
  }

  def main(args: Array[String]): Unit = {}
}

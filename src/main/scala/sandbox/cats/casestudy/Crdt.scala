package sandbox.cats.casestudy

final case class GCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int): GCounter = {
    val newAmount = counters.getOrElse(machine, 0) + amount
    copy(counters = counters + (machine -> newAmount))
  }

  def merge(that: GCounter): GCounter = {
    val m1 = counters.foldLeft(Map.empty[String, Int]) { (m, kv) =>
      kv match {
        case (k, v) =>
          m.updated(k, v max that.counters.getOrElse(k, 0))
      }
    }
    val m2 = that.counters.foldLeft(m1) { (m, kv) =>
      kv match {
        case (k, v) =>
          m.updated(k, v max counters.getOrElse(k, 0))
      }
    }

    GCounter(m2)
  }

  def total: Int = counters.values.sum
}

object Crdt {}

package sandbox.cats

trait Codec[A] { self =>
  def encode(v: A): String
  def decode(s: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    override def encode(v: B): String = (enc andThen self.encode)(v)
    override def decode(s: String): B = (self.decode _ andThen dec)(s)
  }
}

object Codec {
  def encode[A](v: A)(implicit c: Codec[A]): String = c.encode(v)
  def decode[A](s: String)(implicit c: Codec[A]): A = c.decode(s)
}

object CodecInstances {
  implicit val strCodec: Codec[String] = new Codec[String] {
    override def encode(v: String): String = v
    override def decode(s: String): String = s
  }

  implicit val doubleCodec: Codec[Double] =
    strCodec.imap(_.toDouble, _.toString)
}

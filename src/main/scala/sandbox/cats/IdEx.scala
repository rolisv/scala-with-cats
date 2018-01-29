package sandbox.cats

import cats.Monad

import scala.annotation.tailrec

object IdEx {
  type Id[A] = A

  implicit def idMonad: Monad[Id] = new Monad[Id] {
    override def pure[A](x: A): Id[A] = x

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = {
      f(a) match {
        case Left(aRes)  => tailRecM(aRes)(f)
        case Right(bRes) => bRes
      }
    }

    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  }
}

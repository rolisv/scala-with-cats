package sandbox.cats

import cats.Eval

object EvalEx {

  def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = as match {
    case Nil     => acc
    case a :: as => f(a, foldRight(as, acc)(f))
  }

  def foldRightSsafe[A, B](as: List[A], acc: Eval[B])(f: (A, B) => B): Eval[B] = as match {
    case Nil     => acc
    case a :: as => Eval.defer(foldRightSsafe(as, acc)(f).map(f(a, _)))
  }

  def foldRight2[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    foldRightSsafe(as, Eval.now(acc))(f).value
}

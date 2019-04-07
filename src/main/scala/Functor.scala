trait Functor[F[_]] {
  def map[A, B](fa: F[A])(fn: A => B):F[B]
}

object Functor {
  def apply[F[_]](implicit f:Functor[F]): Functor[F] = f

  implicit class FunctorOps[F[_], A](fa: F[A]) {
    def map[B](fn: A => B)(implicit functor: Functor[F]): F[B] = functor.map(fa)(fn)
  }
}
object Schemes {
  import Functor._

  def ana[F[_] : Functor, A](coAlgebra: A => F[A]): A => Fix[F] =
    a => Fix(coAlgebra(a).map(ana(coAlgebra)))

}
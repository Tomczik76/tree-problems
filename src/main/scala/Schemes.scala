object Schemes {
  import Functor._

  def ana[F[_] : Functor, A](coAlgebra: A => F[A]): A => Fix[F] =
    a => Fix(coAlgebra(a).map(ana(coAlgebra)))

  def cata[F[_] : Functor, A](f: F[A] => A): Fix[F] => A =
    fix => f(fix.unFix.map(cata(f)))

  def para[F[_] : Functor, A](f: F[(Fix[F], A)] => A): Fix[F] => A =
    fix => f(fix.unFix.map(fix => fix -> para(f).apply(fix)))

}
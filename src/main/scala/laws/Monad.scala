package laws

trait Monad[F[_]] {

  def unit[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // laws with unit and flatMap:

  def leftIdentity[A, B](a: A, f: A => F[B]) =
    flatMap(unit(a))(f) == f(a)

  def rightIdentity[A](fa: F[A]) =
    flatMap(fa)(unit) == fa

  // derived functions

  def compose[A, B, C](f: B => F[C], g: A => F[B]): A => F[C] =
    a => flatMap(g(a))(f)

  // this is >=> in Haskell
  def andThen[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // laws with andThen

  def leftIdentityAndThen[A, B](a: A, f: A => F[B]) = {
    andThen(unit[A], f)(a) == f(a) // ->
    flatMap(unit(a))(f)
  }

  def rightIdentityAndThen[A, B](a: A, f: A => F[B]) = {
    andThen(f, unit[B])(a) == f(a) // ->
    flatMap(f(a))(unit)
  }

  // laws with compose

  def leftIdentityCompose[A, B](a: A, f: A => F[B]) = {
    compose(f, unit[A])(a) == f(a) // ->
    flatMap(unit(a))(f)
  }

  def rightIdentityCompose[A, B](a: A, f: A => F[B]) = {
    compose(unit[B], f)(a) == f(a) // ->
    flatMap(f(a))(unit)
  }

}

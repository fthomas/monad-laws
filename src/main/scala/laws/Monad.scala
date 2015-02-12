package laws

trait Monad[F[_]] {

  def unit[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // laws with unit and flatMap:

  def leftIdentity[A, B](a: A, f: A => F[B]) =
    flatMap(unit(a))(f) == f(a)

  def rightIdentity[A](fa: F[A]) =
    flatMap(fa)(unit) == fa

  // composition of Kleisli arrows

  // left-to-right composition
  def andThen[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // right-to-left composition
  def compose[A, B, C](f: B => F[C], g: A => F[B]): A => F[C] =
    a => flatMap(g(a))(f)

  // laws with andThen

  // unit is the left identity element for left-to-right Kleisli composition
  def leftIdentityAndThen[A, B](a: A, f: A => F[B]) = {
    andThen(unit[A], f)(a) == f(a)
    flatMap(unit(a))(f)
  }

  // unit is the right identity element for left-to-right Kleisli composition
  def rightIdentityAndThen[A, B](a: A, f: A => F[B]) = {
    andThen(f, unit[B])(a) == f(a)
    flatMap(f(a))(unit)
  }

  // laws with compose

  // unit is the right identity element for right-to-left Kleisli composition
  def leftIdentityCompose[A, B](a: A, f: A => F[B]) = {
    compose(f, unit[A])(a) == f(a)
    flatMap(unit(a))(f)
  }

  // unit is the left identity element for right-to-left Kleisli composition
  def rightIdentityCompose[A, B](a: A, f: A => F[B]) = {
    compose(unit[B], f)(a) == f(a)
    flatMap(f(a))(unit)
  }
}

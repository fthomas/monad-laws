package laws

trait Comonad[F[_]] {

  def extract[A](fa: F[A]): A

  def extend[A, B](fa: F[A])(f: F[A] => B): F[B]

  // laws with extract and extend

  def leftIdentity[A](fa: F[A]) =
    extend(fa)(extract) == fa

  def rightIdentity[A, B](fa: F[A], f: F[A] => B) =
    extract(extend(fa)(f)) == f(fa)

  // composition of Cokleisli arrows

  // left-to-right composition
  def andThen[A, B, C](f: F[A] => B, g: F[B] => C): F[A] => C =
    fa => g(extend(fa)(f))

  // right-to-left composition
  def compose[A, B, C](f: F[B] => C, g: F[A] => B): F[A] => C =
    fa => f(extend(fa)(g))

  // laws with andThen

  // extract is the left identity element for left-to-right Cokleisli composition
  def leftIdentityAndThen[A, B](fa: F[A], f: F[A] => B) = {
    andThen(extract[A], f)(fa) == f(fa)
    f(extend(fa)(extract)) == f(fa)
    extend(fa)(extract) == fa
  }

  // extract is the right identity element for left-to-right Cokleisli composition
  def rightIdentityAndThen[A, B](fa: F[A], f: F[A] => B) = {
    andThen(f, extract[B])(fa) == f(fa)
    extract(extend(fa)(f))
  }

  // laws with compose

  // extract is the right identity element for right-to-left Cokleisli composition
  def leftIdentityCompose[A, B](fa: F[A], f: F[A] => B) = {
    compose(f, extract[A])(fa) == f(fa)
    f(extend(fa)(extract)) == f(fa)
    extend(fa)(extract) == fa
  }

  // extract is the left identity element for right-to-left Cokleisli composition
  def rightIdentityCompose[A, B](fa: F[A], f: F[A] => B) = {
    compose(extract[B], f)(fa) == f(fa)
    extract(extend(fa)(f))
  }
}

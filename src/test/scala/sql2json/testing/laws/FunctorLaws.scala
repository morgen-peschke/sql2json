package sql2json
package testing
package laws 

import cat.Eq
import cat.Show
import cat.Functor
import cat.Functor.given
import testing.Test.given

object FunctorLaws

  def identity[C[_]: Functor, A](fa: C[A])(given Eq[C[A]], Show[C[A]]): Unit =
    "identity" check {
      fa.map(identity) <-> fa
    }
    
  def composition[F[_]: Functor, A, B, C](fa: F[A], f: A => B, g: B => C)(given Show[F[A]], Show[F[C]], Eq[F[C]]): Unit =
    "composition" using ("fa" -> fa) {
        fa.map(f).map(g) <-> fa.map[C](f andThen g)
    }
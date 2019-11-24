package sql2json
package cat

trait Functor[C[_]]
  def map [A,B] (fa: C[A], f: A => B): C[B]

object Functor 
  def apply[C[_]: Functor] = summon[Functor[C]]

  given [C[_,_],L](given B: Bifunctor[C]): Functor[B.RightBiased[L]] = 
    B.rightFunctor[L]

  trait FunctorOps[C[_],A]
    def [B] (fa: C[A]) map (f: A => B)(given F: Functor[C]): C[B] = F.map(fa, f)

  given [C[_],A]: FunctorOps[C,A]
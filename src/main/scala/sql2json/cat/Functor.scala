package sql2json
package cat

trait Functor[C[_]]
  def map [A,B] (fa: C[A], f: A => B): C[B]

trait BifunctorCanBeFunctor
  given [C[_,_],L](given B: Bifunctor[C]): Functor[Bifunctor.RightBiased[C][L]] = 
    B.rightFunctor[L]

  given [C[_,_],R](given B: Bifunctor[C]): Functor[Bifunctor.LeftBiased[C][R]] = 
      B.leftFunctor[R]

object Functor extends BifunctorCanBeFunctor 
  trait FunctorOps[C[_],A]
    def [B] (fa: C[A]) map (f: A => B)(given F: Functor[C]): C[B] = F.map(fa, f)

  given [C[_],A]: FunctorOps[C,A]
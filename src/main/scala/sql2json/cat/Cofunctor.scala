package sql2json
package cat

trait Cofunctor[C[_]]
  def comap [A,B] (fa: C[A], f: B => A): C[B]

object Cofunctor  
  trait CofunctorOps[C[_],A]
    def [B] (fa: C[A]) comap (f: B => A)(given F: Cofunctor[C]): C[B] = F.comap(fa, f)

  given [C[_],A]: CofunctorOps[C,A]
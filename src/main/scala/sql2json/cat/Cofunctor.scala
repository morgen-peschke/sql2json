package sql2json
package cat

trait Cofunctor[C[_]]
  def comap [A,B] (fa: C[A], f: B => A): C[B]

object Cofunctor  
  given ops[C[_],A]: AnyRef
    def [B] (fa: C[A]) comap (f: B => A)(given F: Cofunctor[C]): C[B] = F.comap(fa, f)
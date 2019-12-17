package sql2json
package cat

trait Functor[C[_]]
  def map [A,B] (fa: C[A], f: A => B): C[B]

trait ApplicativeIsFunctor
  given [C[_]](given A: Applicative[C]): Functor[C] = A.functor

object Functor extends ApplicativeIsFunctor 
  given ops[C[_],A]: AnyRef
    def [B] (fa: C[A]) map (f: A => B)(given F: Functor[C]): C[B] = F.map(fa, f)

  given Functor[List]
    def map [A,B] (fa: List[A], f: A => B): List[B] = fa.map(f)
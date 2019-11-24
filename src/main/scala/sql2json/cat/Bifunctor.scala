package sql2json
package cat

trait Bifunctor[C[_,_]]
  def bimap[L0,R0,L1,R1](clr: C[L0,R0], fl: L0 => L1, fr: R0 => R1): C[L1,R1]

  def leftMap[L0,L1,R](clr: C[L0, R], fl: L0 => L1): C[L1, R] = bimap(clr, fl,identity)

  def rightMap[L,R0,R1](clr: C[L, R0], fr: R0 => R1): C[L, R1] = bimap(clr, identity,fr)

  type LeftBiased[R] = [L] =>> C[L,R]
  type RightBiased[L] = [R] =>> C[L,R]
  def leftFunctor[R]: Functor[LeftBiased[R]] = 
    new Functor[LeftBiased[R]] with
      def map[A,B] (fa: LeftBiased[R][A], f: A => B): LeftBiased[R][B] = leftMap(fa, f)
    

  def rightFunctor[L]: Functor[RightBiased[L]] = 
    new Functor[RightBiased[L]] with
      def map [A,B] (fa: RightBiased[L][A], f: A => B): RightBiased[L][B] = rightMap(fa, f)

object Bifunctor
  def apply[C[_,_]](given Bifunctor[C]) = summon[Bifunctor[C]]

  trait BifunctorOps[C[_,_],L0,R0]
    def[L1,R1] (clr: C[L0,R0]) bimap (fl: L0 => L1, fr: R0 => R1)(given B: Bifunctor[C]): C[L1,R1] =
      B.bimap(clr, fl, fr)

    def[L1](clr: C[L0, R0]) leftMap (fl: L0 => L1)(given B: Bifunctor[C]): C[L1, R0] = 
      B.leftMap(clr, fl)

    def[R1](clr: C[L0, R0]) rightMap (fr: R0 => R1)(given B: Bifunctor[C]): C[L0, R1] = 
      B.rightMap(clr, fr)

  given syntax[C[_,_],L,R]: BifunctorOps[C,L,R]

  given Bifunctor[Either]
    def bimap[L0,R0,L1,R1](clr: Either[L0,R0], fl: L0 => L1, fr: R0 => R1): Either[L1,R1] = 
      clr match
        case Right(v) => Right(fr(v))
        case Left(v)  => Left(fl(v))
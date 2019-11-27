package sql2json
package cat

import Bifunctor.{LeftBiased, RightBiased}

trait Bifunctor[C[_,_]]
  def bimap[L0,R0,L1,R1](clr: C[L0,R0], fl: L0 => L1, fr: R0 => R1): C[L1,R1]

  def leftMap[L0,L1,R](clr: C[L0, R], fl: L0 => L1): C[L1, R] = bimap(clr, fl,identity)

  def rightMap[L,R0,R1](clr: C[L, R0], fr: R0 => R1): C[L, R1] = bimap(clr, identity,fr)

  def leftFunctor[R]: Functor[LeftBiased[C][R]] = 
    new Functor[LeftBiased[C][R]] with
      def map[A,B] (fa: LeftBiased[C][R][A], f: A => B): LeftBiased[C][R][B] = leftMap(fa, f)
    

  def rightFunctor[L]: Functor[RightBiased[C][L]] = 
    new Functor[RightBiased[C][L]] with
      def map [A,B] (fa: RightBiased[C][L][A], f: A => B): RightBiased[C][L][B] = rightMap(fa, f)

object Bifunctor
  type LeftBiased[C[_,_]] = [R] =>> [L] =>> C[L,R]
  type RightBiased[C[_,_]] = [L] =>> [R] =>> C[L,R]

  trait BifunctorOps[C[_,_],L0,R0]
    def[L1,R1] (clr: C[L0,R0]) bimap (fl: L0 => L1, fr: R0 => R1)(given B: Bifunctor[C]): C[L1,R1] =
      B.bimap(clr, fl, fr)

    def[L1](clr: C[L0, R0]) leftMap (fl: L0 => L1)(given B: Bifunctor[C]): C[L1, R0] = 
      B.leftMap(clr, fl)

    def[R1](clr: C[L0, R0]) rightMap (fr: R0 => R1)(given B: Bifunctor[C]): C[L0, R1] = 
      B.rightMap(clr, fr)

  given[C[_,_],L,R]: BifunctorOps[C,L,R]

  given Bifunctor[Either]
    def bimap[L0,R0,L1,R1](clr: Either[L0,R0], fl: L0 => L1, fr: R0 => R1): Either[L1,R1] = 
      clr match
        case Right(v) => Right(fr(v))
        case Left(v)  => Left(fl(v))
package sql2json
package cat

import Applicative.given
import ApplicativeError.given
import Functor.given
import Semigroup.given
import Monad.given
import MonadError.given
import org.junit.Test
import org.junit.Assert._
import testing.laws.{ApplicativeLaws, BifunctorLaws, EqLaws, FunctorLaws}

import EitherBifunctorLaws.given

class EitherBifunctorLaws extends BifunctorLaws[Either, Int, String, Long, String, Long, Int]

class EitherLeftFunctorLaws extends FunctorLaws[Bifunctor.LeftBiased[Either][String], Int, String, Long]

object EitherBifunctorLaws

  given bifunctorGivens: BifunctorLaws.Givens[Either, Int, String, Long, String, Long, Int] =
    BifunctorLaws.Givens[Either, Int, String, Long, String, Long, Int]

  given functorGivens: FunctorLaws.Givens[Bifunctor.LeftBiased[Either][String], Int, String, Long] =
    FunctorLaws.Givens[Bifunctor.LeftBiased[Either][String], Int, String, Long]
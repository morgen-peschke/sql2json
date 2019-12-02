package sql2json
package types

import testing.laws.{ApplicativeLaws, EqLaws, FunctorLaws, MonadLaws, SemigroupLaws}

final class NonEmptyListEqLaws extends EqLaws[NonEmptyList[Long]]
final class NonEmptyListFunctorLaws extends FunctorLaws[NonEmptyList, Int, String, Long]
final class NonEmptyListApplicativeLaws extends ApplicativeLaws[NonEmptyList, Int, String, Long]
final class NonEmptyListMonadLaws extends MonadLaws[NonEmptyList, Int, String]
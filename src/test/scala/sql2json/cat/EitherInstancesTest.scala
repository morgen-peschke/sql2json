package sql2json
package cat

import testing.laws.{BifunctorLaws, FunctorLaws}

class EitherBifunctorLaws extends BifunctorLaws[Either, Int, String, Long, String, Long, Int]
class EitherRightFunctorLaws extends FunctorLaws[Bifunctor.RightBiased[Either][String], Int, String, Long]
class EitherLeftFunctorLaws extends FunctorLaws[Bifunctor.LeftBiased[Either][String], Int, String, Long]

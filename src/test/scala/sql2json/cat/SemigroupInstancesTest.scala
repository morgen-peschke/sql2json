package sql2json
package cat

import testing.laws.SemigroupLaws

final class SemigroupInstanceForIntTest extends SemigroupLaws[Int]
final class SemigroupInstanceForLongTest extends SemigroupLaws[Long]
final class SemigroupInstanceForListTest extends SemigroupLaws[List[Boolean]]

given semigroupLawsGivensForInt: SemigroupLaws.Givens[Int]
given semigroupLawsGivensForLong: SemigroupLaws.Givens[Long]
given semigroupLawsGivensForList: SemigroupLaws.Givens[List[Boolean]]
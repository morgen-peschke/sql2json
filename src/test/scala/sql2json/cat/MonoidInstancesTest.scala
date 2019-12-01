package sql2json
package cat

import testing.laws.MonoidLaws

final class MonoidInstanceForLongTest extends MonoidLaws[Long]
final class MonoidInstanceForListTest extends MonoidLaws[List[Boolean]]

given MonoidLawsGivensForLong: MonoidLaws.Givens[Long]
given MonoidLawsGivensForList: MonoidLaws.Givens[List[Boolean]]
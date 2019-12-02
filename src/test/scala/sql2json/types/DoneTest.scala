package sql2json
package types

import testing.laws.{EqLaws, SemigroupLaws, MonoidLaws}

final class DoneEqLaws extends EqLaws[Done]
final class DoneSemigroupLaws extends SemigroupLaws[Done]
final class DoneMonoidLaws extends MonoidLaws[Done]
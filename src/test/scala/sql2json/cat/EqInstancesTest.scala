package sql2json
package cat

import testing.laws.EqLaws

final class EqStringInstancesTest extends EqLaws[String]
final class EqIntInstancesTest extends EqLaws[Int]
final class EqLongInstancesTest extends EqLaws[Long]
final class EqBooleanInstancesTest extends EqLaws[Boolean]
final class EqEitherInstancesTest extends EqLaws[Either[Int,String]]
final class EqListInstancesTest extends EqLaws[List[Boolean]]

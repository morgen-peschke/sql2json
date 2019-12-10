package sql2json
package types
package validation

/**
 * We'll use [[NonEmptyList]] to aggregate errors, it's pulled 
 * out into it's own file for visibility, as it'll be used by
 * both [[Validated]] and [[FailFastValidated]]
 */
type Errors = NonEmptyList[String]
object Errors
  given Convertible[String, Errors] = NonEmptyList.one(_)
  given[T <: Throwable]: Convertible[T, Errors] = 
    t => NonEmptyList(
      t.getMessage,
      (Option(t.getCause).toList ::: t.getSuppressed.toList).map(_.getMessage)
    )
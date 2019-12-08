package sql2json
package types

import cat.{Applicative, Show, Eq, SemigroupK, Functor, Monoid, Monad}
import cat.Show.given
import cat.Eq.given
import cat.Monoid.given
import cat.Semigroup.given
import scala.annotation.tailrec

/**
 * Yet another bit I grabbed from [Cats](https://typelevel.org/cats/)
 */
case class NonEmptyList[A](head: A, tail: List[A])
  def toList: List[A] = head :: tail

  def fold(given M: Monoid[A]): A = tail.foldLeft(M.empty)(_ combine _)

  def map[B] (f: A => B): NonEmptyList[B] = NonEmptyList(f(head), tail.map(f))

  def flatMap[B](fc: A => NonEmptyList[B]): NonEmptyList[B] =
    val processedHead = fc(head)
    NonEmptyList(
      processedHead.head,
      processedHead.tail ::: tail.flatMap(a => fc(a).toList)
    )

object NonEmptyList
  def one[A](head: A): NonEmptyList[A] = NonEmptyList(head, Nil)
  def of[A](head: A, tail0: A, tailN: A*): NonEmptyList[A] = NonEmptyList(head, tail0 :: tailN.toList)

  given[A] (given Show[A]): Show[NonEmptyList[A]] = _.toList.show
  
  given[A] (given Eq[A]): Eq[NonEmptyList[A]]
    def equiv(nelA: NonEmptyList[A], nelB: NonEmptyList[A]): Boolean = 
      @tailrec
      def loop(restA: List[A], restB: List[A]): Boolean = 
        (restA, restB) match
          case (Nil, Nil) => true
          case (_ :: _, Nil) | (Nil, _ :: _) => false
          case (a :: nextA, b :: nextB) => a === b && loop(nextA, nextB)

      nelA.head === nelB.head && loop(nelA.tail, nelB.tail)

  given SemigroupK[NonEmptyList]
    def combineK[A](a: NonEmptyList[A], b: NonEmptyList[A]): NonEmptyList[A] = NonEmptyList(a.head, a.tail ::: b.toList)

  given Functor[NonEmptyList]
    def map[A,B] (fa: NonEmptyList[A], f: A => B): NonEmptyList[B] = fa.map(f)

  given Applicative[NonEmptyList]
    def pure[A](a: A): NonEmptyList[A] = one(a)

    def ap[A, B](cf: NonEmptyList[A => B], ca: NonEmptyList[A]): NonEmptyList[B] = cf.flatMap(ca.map(_))

  given Monad[NonEmptyList]
    def flatMap[A,B](ca: NonEmptyList[A], fc: A => NonEmptyList[B]): NonEmptyList[B] = ca.flatMap(fc)
  
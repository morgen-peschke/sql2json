package sql2json
package types

import cat.Show
import cat.Show.given
import cat.Eq
import cat.Eq.given
import scala.annotation.tailrec

/**
 * Yet another bit I grabbed from [Cats](https://typelevel.org/cats/)
 */
case class NonEmptyList[A](head: A, tail: List[A])
  def toList: List[A] = head :: tail

object NonEmptyList
  def one[A](head: A): NonEmptyList[A] = NonEmptyList(head, Nil)
  def of[A](head: A, tail0: A, tailN: A*): NonEmptyList[A] = NonEmptyList(head, tail0 :: tailN.toList)

  given[A] (given Show[A]): Show[NonEmptyList[A]] = _.toList.map(_.show).mkString("[", ",", "]")
  
  given[A] (given Eq[A]): Eq[NonEmptyList[A]]
    def equiv(nelA: NonEmptyList[A], nelB: NonEmptyList[A]): Boolean = 
      @tailrec
      def loop(restA: List[A], restB: List[A]): Boolean = 
        (restA, restB) match
          case (Nil, Nil) => true
          case (_ :: _, Nil) | (Nil, _ :: _) => false
          case (a :: nextA, b :: nextB) => a === b && loop(nextA, nextB)

      nelA.head === nelB.head && loop(nelA.tail, nelB.tail)

  given cat.SemigroupK[NonEmptyList]
    def combineK[A](a: NonEmptyList[A], b: NonEmptyList[A]): NonEmptyList[A] = NonEmptyList(a.head, a.tail ::: b.toList)

  given cat.Functor[NonEmptyList]
    def map[A,B] (fa: NonEmptyList[A], f: A => B): NonEmptyList[B] = NonEmptyList(f(fa.head), fa.tail.map(f))

  
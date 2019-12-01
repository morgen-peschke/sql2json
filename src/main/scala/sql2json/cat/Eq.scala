package sql2json
package cat

import scala.annotation.tailrec

trait Eq[A]
  def equiv(a: A, b: A): Boolean
  
object Eq
  trait EqOps[A]
    def (a: A) === (b: A)(given E: Eq[A]): Boolean = E.equiv(a, b)

    def (a: A) =!= (b: A)(given E: Eq[A]): Boolean = !E.equiv(a, b)

  given[A]: EqOps[A]

  given Eq[String]  = _ == _
  given Eq[Int]     = _ == _
  given Eq[Long]    = _ == _
  given Eq[Boolean] = _ == _
  given [L: Eq, R: Eq]: Eq[Either[L,R]] = 
    (_, _) match
      case (Left(l1), Left(l2)) => l1 === l2
      case (Right(r1), Right(r2)) => r1 === r2
      case _ => false

  given[A] (given EA: Eq[A]): Eq[List[A]] = 
    @tailrec
    def loop(l: List[A], r: List[A]): Boolean  =
      l match
        case Nil => r.isEmpty
        case lh :: lt =>
          r match
            case Nil => false
            case rh :: rt =>
              lh === rh && loop(lt, rt)
    
    loop(_, _)
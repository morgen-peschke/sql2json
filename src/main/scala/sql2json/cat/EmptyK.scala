package sql2json
package cat

trait EmptyK[C[_]]
  def emptyK[A]: C[A]

  def empty[A]: Empty[C[A]] = 
    Empty.instance[C[A]](emptyK[A])

object EmptyK
  given EmptyK[List]
    def emptyK[A]: List[A] = Nil
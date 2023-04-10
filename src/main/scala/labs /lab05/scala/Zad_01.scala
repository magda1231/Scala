package lab05

def isOrdered[A](leq: (A, A) => Boolean)(l: List[A]): Boolean = {

  def helper(l: List[A], acc: Boolean): Boolean = {
    l match {
      case Nil => acc
      case _ :: Nil => acc
      case x :: y :: z => helper(y :: z, acc && leq(x, y))
    }
  }
  helper(l, true)  
}

@main 
def zadanie_01: Unit = {
  val lt = (m: Int, n: Int) => m < n
  // val lte = (m: Int, n: Int) => m <= n
  val lista = List(1, 2, 3,3, 5)
  println(isOrdered(lt)(lista)) // ==> false
  // Program powinien umożliwić „sprawdzenie” działania
  // funkcji „isOrdered”.
}


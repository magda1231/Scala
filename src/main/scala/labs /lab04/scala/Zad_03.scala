package lab04

def sumuj(l: List[Option[Double]]): Option[Double] = {
   @annotation.tailrec
    def sumujHelper(l: List[Option[Double]], acc: Double): Option[Double] = 
        l match {
            case Nil => Some(acc)
            case Some(head) :: tail if head > 0 => sumujHelper(tail,  acc + head)
            case head :: tail => sumujHelper(tail, acc )
        }
    sumujHelper(l,0)
}
@main
 def zadanie_03: Unit = {
  // Program powinien umożliwić „sprawdzenie” działania
  // funkcji „sumuj”.
//   println(Some(5) + Some(5))
    val lista = List(Some(4.0), Some(-3.0), None, Some(1.0), Some(0.0))
    println(sumuj(lista))


    


}


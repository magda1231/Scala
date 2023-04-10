package lab04
import annotation.tailrec


val lista1 = List(2, 4, 3, 5)
val lista2 = List(1, 2, 2, 3, 1, 5)

// see
def tasuj(l1: List[Int], l2: List[Int]): List[Int] = {

  def merge(l1: List[Int], l2: List[Int], acc: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => acc ++ l2
    case (_, Nil) => acc ++ l1
    case (h1 :: t1, h2 :: t2) if h1 < h2 => merge(t1, l2, acc :+ h1)
    case (h1 :: t1, h2 :: t2) if h1 > h2 => merge(l1, t2, acc :+ h2)
    case (h1 :: t1, h2 :: t2) => merge(t1, t2, acc :+ h1)
  }

  def removeDuplicates(list: List[Int], prev: Option[Int]): List[Int] = list match {
    case Nil => Nil
    case h :: t =>
        if prev == Some(h) then removeDuplicates(t, prev)
        else h :: removeDuplicates(t, Some(h))
  }

  removeDuplicates(merge(l1, l2, Nil), None)
}



@main 
def zadanie_02: Unit = {
  // Program powinien umożliwić „sprawdzenie” działania
  // funkcji „tasuj”.
  println(tasuj(lista1,lista2))
}

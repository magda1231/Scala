def wystąpienia(arg: List[Int]):  List[(Int,Int)] = {

  @annotation.tailrec
  def reverse2(arg: List[(Int,Int)], acc: List[(Int,Int)] = Nil): List[(Int,Int)] = {
    arg match {
      case Nil => acc
      case head :: tail => reverse2(tail, head :: acc)
    }
  }

  @annotation.tailrec
  def reverse(arg: List[Int], acc: List[Int] = Nil): List[Int] = {
    arg match {
      case Nil => acc
      case head :: tail => reverse(tail, head :: acc)
    }
  }
    
  @annotation.tailrec
  def filter(list: List[Int], elem: Int, acc: List[Int] = Nil): List[Int] = {
    list match {
      case Nil => reverse(acc)
      case head :: tail if head != elem => filter(tail, elem, head :: acc)
      case head :: tail => filter(tail, elem, acc)
    }
  } 

  @annotation.tailrec
  def count(list: List[Int], elem: Int, acc: Int = 0): Int = {
    list match {
      case Nil => acc
      case head :: tail if head == elem => count(tail, elem, acc + 1)
      case head :: tail => count(tail, elem, acc)
    }
  }
   
  @annotation.tailrec
  def wystąpieniaHelper(list: List[Int], acc: List[(Int,Int)] = Nil): List[(Int,Int)] = {
    list match {
      case Nil => reverse2(acc)
      case head :: tail => {
        val filtered = filter(tail, head)
        val counthead: Int = count(list, head)
        wystąpieniaHelper(filtered, (head, counthead) :: acc)
      }
    }
  }

  wystąpieniaHelper(arg)

}

/*
  Poprawność rozwiązania należy testować (z poziomu SBT) poleceniem:
  testOnly Test1
*/


@main def zad_1: Unit = {
  // „program główny” ma znaczenie czysto pomocnicze
  val arg = List(1,2,3)
  val wyn = wystąpienia(arg)
  println(wyn)

}


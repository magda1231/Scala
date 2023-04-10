package lab05

def deStutter[A](list: List[A]): List[A] = {
   list.foldLeft(List[A]())((l,n)=> {
    l match {
      case Nil => l :+ n
      case x :: y if l.last == n => l
      case x :: y if l.last!= n =>  l :+ n
      case _ :: Nil if l.last == n => l
      case _ :: Nil if l.last != n => l :+ n
      case _ => l 
    }
    })
  // list.foldLeft(List[A]())((acc, elem) => {
  //   if (acc.isEmpty || acc.last != elem) {
  //     acc :+ elem
  //   } else {
  //     acc
  //   }
  // })

}
@main 
def zadanie_02: Unit = {
  // Program powinien umożliwić „sprawdzenie” działania
  // funkcji „deStutter”.
  println(deStutter(List(1, 1, 2,2, 4, 4, 4, 1, 3,3)))

}

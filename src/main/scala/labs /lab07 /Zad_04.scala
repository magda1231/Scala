package lab07

def przestaw[A](l: List[A]): Any = {

  l.zipWithIndex.map((elem) => if (elem._2 % 2 == 0 && elem._2 != l.length - 1) l(elem._2 + 1) else if (elem._2 % 2 != 0) l(elem._2 - 1) else elem._1)
  l.grouped(2).toList.map(_.reverse).flatten
  l.grouped(2).toList.flatMap( _.reverse)

  
}

@main def zad_04: Unit = {
  println("Hello World!")
  val lista = List(1, 2, 3, 4, 5)
  println(przestaw(lista))
}

// val lista = List(1, 2, 3, 4, 5)
// assert( przestaw(List(1, 2, 3, 4, 5)) List(2, 1, 4, 3, 5) ) /

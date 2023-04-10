package lab07

def usuń[A](lista: List[A], k: Int): List[A] = {
  lista.zipWithIndex.filter((elem) => elem._2 != k).map(x=> x._1)
}

@main 
def zad_01: Unit = {
  println("Hello World!")
  val lista = List(1,2,3,4,5,6)
  println(usuń(lista,5))
}



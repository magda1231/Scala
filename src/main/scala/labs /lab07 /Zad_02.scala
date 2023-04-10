package lab07

def indeksy[A](lista: List[A], el: A): Set[Int] = {
  lista.zipWithIndex.filter(x=> x._1 == el).map(x=> x._2).toSet
}

@main def zad_02: Unit = {
  println("Hello World!")
  val lista = List(1,2,3,1,3)
  println(indeksy(lista,1))
}

// Korzystając z metod oferowanych przez kolekcje zdefiniuj funkcję:

// def indeksy[A](lista: List[A], el: A): Set[Int] = {
//   Set()
// }
// zwracającą wszystkie indeksy w liście lista, na których znajduje się element el.

// Przykłady:

// val lista = List(1, 2, 1, 1, 5)
// assert( indeksy(lista, 1) == Set(0, 2, 3) ) // ==> true
// assert( indeksy(lista, 7) == Set() )        // ==> true

// case clasy 
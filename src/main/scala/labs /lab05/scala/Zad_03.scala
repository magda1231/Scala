package lab05

def chessboard: Unit = {
  val letters = List("a", "b", "c", "d", "e", "f", "g", "h")
  val numbers = List(1, 2, 3, 4, 5, 6, 7, 8).reverse
  val mapped = numbers.flatMap((n) => (letters.map(l => s"($l,$n)")))
  val grouped = mapped.grouped(8).toList
  val result = grouped.map((l) => l.mkString(" ")).mkString("\n")
  println(result)
}

@main
 def zadanie_03: Unit = {
  // Program powinien umożliwić „sprawdzenie” działania
  // funkcji „szachownica”.
  val chess = chessboard
  chess
 
}


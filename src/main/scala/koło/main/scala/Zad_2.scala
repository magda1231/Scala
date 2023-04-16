//==========================================================================
// Metoda porównuje napisy zgodnie z polskim porządkiem alfabetycznym
// Jedyna zmiana jaka może być tutaj potrzebna to „zamiana komentarzy”
// w linijkach 9 oraz 10.
//--------------------------------------------------------------------------
def ltePL(s1: String, s2: String) = {
  import java.util.Locale
  import java.text.Collator
  // val locale = new Locale("pl", "PL") // dla starszych wersji JRE/JDK
  val locale = Locale.of("pl", "PL") // dla nowszych wersji JRE/JDK
  Collator.getInstance(locale).compare(s1, s2) <= 0
}

// Metoda nie wymaga zmian. Wczytuje dane z pliku i zwraca listę linii
def dane: List[String] = {
  import scala.io.Source
  val plik = Source.fromFile("src/main/resources/machiavelli.txt", "UTF-8")
  plik.getLines.toList
}
//==========================================================================
// List[(String, List[Int])]
// Jedyna rzecz do zaimplementowania, czyli metoda „wynik”:
def wynik: List[(String, List[Int])] = {
  
    // działamy z listą zwracaną przez metodę „wynik”
   val length = dane.length
   
   val groupedWithIndex = dane.flatMap(_.split("\n"))
                    .toList.zipWithIndex
                    .map(x=> (x._1,x._2+1))
   val filteredToLetters = groupedWithIndex
                    .map(x => (x._1.split(" ")
                    .toList.map(y => y.filter(_.isLetter).toLowerCase()),x._2))
   val groupedByAmount = filteredToLetters
                    .flatMap(x => x._1.map(y => (y, x._2)))
                    .groupBy(_._1)

   val sorted = groupedByAmount
              .toList
              .map(x => (x._1, x._2.map(_._2).sorted.distinct))
              .sortBy(_._1)
              .sortWith((x,y) => ltePL(x._1, y._1))
              .filter( x => x._1 != "")

    sorted
    

    
}
/*
  Poprawność rozwiązania należy testować (z poziomu SBT) poleceniem:
  testOnly Test2
*/

@main def zad_2: Unit = {
  // „program główny” ma znaczenie czysto pomocnicze
  if ltePL("a", "ą") then println("OK")
  else println("to nie powinno się zdarzyć…")
  println(wynik)
  // val napis = "Ala ma kota, a kot ma Alę."
  // // delete no letter but no space
  // val wynikl = napis.split(" ").map(x => x.filter(_.isLetter)).mkString(" ")
  // println(wynikl)
  // println(dane)
}

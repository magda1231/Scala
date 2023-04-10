package lab07.próba2 
import scala.math.BigDecimal

case class Ocena(
  imię: String,
  nazwisko: String,
  wdzięk: Int,
  spryt: Int
) {
  require(
    // upewniamy się, że składowe Oceny są sensowne
    imię.strip() != "" &&
    nazwisko.strip() != "" &&
    (0 to 20).contains(wdzięk) &&
    (0 to 20).contains(spryt)
  )
}
case class Wynik(
  miejsce: Int,
  imię: String,
  nazwisko: String,
  średniWdzięk: Double,
  średniSpryt: Double,
  suma: Double
) {
  // upewniamy się, że składowe Wyniku są „sensowne”
  require(
    miejsce >= 0 &&
    imię.strip() != "" &&
    nazwisko.strip() != "" &&
    średniWdzięk >= 0 && średniWdzięk <= 20 &&
    średniSpryt >= 0 && średniSpryt <= 20 &&
    suma == średniWdzięk + średniSpryt
  )
}

def klasyfikacja2(oceny: List[Ocena]): Any = {
  val pogrupowane = oceny.groupBy(elem => (elem.imię, elem.nazwisko))
  .map(elem => (elem._2))
  val wyniki = pogrupowane.map(elem => {
    val imie = elem(0).imię
    val nazwisko = elem(0).nazwisko
    val wdzięk: List[Double]= elem.map(x => x.wdzięk)
    val spryt : List[Double]= elem.map(x => x.spryt)
    val średniWdzięk  = BigDecimal(wdzięk.sum / wdzięk.length).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    val średniSpryt  = BigDecimal(spryt.sum / spryt.length).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    val suma  = średniWdzięk + średniSpryt
    Wynik(0,imie,nazwisko, średniWdzięk, średniSpryt, suma)
  }).toList

   val klasyfikacja = wyniki.sortBy(_.nazwisko).sortBy(_.suma).reverse

   klasyfikacja
   .foldLeft(List[Wynik]())((acc, elem) => {
     if (acc.isEmpty) {
       Wynik(1, elem.imię, elem.nazwisko, elem.średniWdzięk, elem.średniSpryt, elem.suma) :: acc
     } else if (elem.suma == acc.head.suma) {
       Wynik(acc.head.miejsce, elem.imię, elem.nazwisko, elem.średniWdzięk, elem.średniSpryt, elem.suma) :: acc
     } else {
       Wynik(acc.head.miejsce + 1, elem.imię, elem.nazwisko, elem.średniWdzięk, elem.średniSpryt, elem.suma) :: acc
     }
   }).reverse
}

@main
def zad_03_2: Unit = {
  // Uwaga! Poniższy przykład to jedynie „inspiracja”

  val oceny = List(  Ocena("Jan", "Kowalski", 19, 17),
                      Ocena("Jan", "Kowalski", 18, 18),
                      Ocena("Sara", "Bloaa", 4, 2),
                      Ocena("Ala", "Ale", 4, 2),
                      Ocena("Ala", "Ala", 1, 2),
                      Ocena("Joseph", "Konrad", 16, 19),
                       Ocena("Jan", "Kowalski", 10, 15),
                      Ocena("Anna", "Kowalska", 19, 17),
                      Ocena("Joanna", "Alojza", 20, 20),  
                      Ocena("Anna", "Kowalska", 18, 18))

  println(klasyfikacja2(oceny))
}
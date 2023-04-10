package lab07
// import scala.math._
// import BigDecimal.double2bigDecimal
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

def klasyfikacja(oceny: List[Ocena]): Any = {
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
    (imie,nazwisko, średniWdzięk, średniSpryt, suma)
  }).toList

  val klasyfikacja = wyniki.sortBy(_._2).sortBy(-_._5)
  
  val zindeksowane = klasyfikacja.zipWithIndex.map(elem => {
    (elem._2 + 1, elem._1._1, elem._1._2, elem._1._3, elem._1._4, elem._1._5)
  
  })
  def sameSumSamePlace(list: List[(Int, String, String, Double, Double,Double)]): List[Wynik] = {
    def sameSumSamePlaceHelper(list: List[(Int, String, String, Double, Double,Double)], acc: List[Wynik]): List[Wynik] = {
      list match {
        case Nil => acc 
        case head :: tail if head._1 == 1 => sameSumSamePlaceHelper(tail, Wynik(head._1, head._2, head._3, head._4, head._5, head._6) :: acc )
        case head :: tail if head._6 == zindeksowane(head._1 -2)._6 => sameSumSamePlaceHelper(tail, Wynik(head._1-1, head._2, head._3, head._4, head._5, head._6) :: acc )
        case head :: tail => sameSumSamePlaceHelper(tail, Wynik(head._1, head._2, head._3, head._4, head._5, head._6) :: acc )     
        }
      }
    sameSumSamePlaceHelper(list, Nil).reverse
  }
  val exaquo = sameSumSamePlace(zindeksowane)

  def moveIf2PlaceDifference(list: List[Wynik]): List[Wynik] = {
    def moveIf2PlaceDifferenceHelper(list: List[Wynik], acc: List[Wynik]): List[Wynik] = {
      list match {
        case Nil => acc
        case head :: tail if head.miejsce == 1 => moveIf2PlaceDifferenceHelper(tail, Wynik(head.miejsce, head.imię, head.nazwisko, head.średniWdzięk, head.średniSpryt, head.suma) :: acc)
        case head :: tail if head.miejsce - exaquo(head.miejsce -2).miejsce > 1 => moveIf2PlaceDifferenceHelper(tail, Wynik(head.miejsce-1, head.imię, head.nazwisko, head.średniWdzięk, head.średniSpryt, head.suma) :: acc)
        case head :: tail => moveIf2PlaceDifferenceHelper(tail, Wynik(head.miejsce, head.imię, head.nazwisko, head.średniWdzięk, head.średniSpryt, head.suma) :: acc)
      }
    }
    moveIf2PlaceDifferenceHelper(list, Nil).reverse
  }
  val moved = moveIf2PlaceDifference(exaquo)

  moved
}
@main
def zad_03: Unit = {
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

  println(klasyfikacja(oceny))
}
package lab01
@main 
def mainPro(): Unit = {
   println("Hello World")
}


def prog2(n:Int): Unit = {
   println(s"Podana liczba: $n")
}


def prog3(imie: String,wiek: Int,czy_gra:Boolean): Unit = {
   val granie = if czy_gra then "gra" else "nie gra"
    println(s"$imie ma $wiek lat i $granie w piłkę")
  }


def prog4(n:Int):Unit = { 
   val rand = scala.util.Random()
   val liczba = rand.nextInt(100)
   if(n<100 && n>0){
     if(n<liczba) println("Podana liczba jest mniejsza od wylosowanej")
     else if(n==liczba) println("Podana liczba jest równa wylosowanej")
     else println("Podana liczba jest większa od wylosowanej")
   }
    else println("Podana liczba nie jest z zakresu 0-100")
   println(s"Wylosowana liczb to $liczba")
}










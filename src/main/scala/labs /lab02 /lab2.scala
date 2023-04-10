package lab02
def obramuj(napis: String): String = {
  val linie = napis.split("\n").map(_.trim).filter(x=> x.length != 0)
  val max_dlugosc = linie.map(x=>x.length).max
  return "*"* (max_dlugosc+4) + "\n" +
      s"${linie.map(x=> {
        val przerwa = " "*((max_dlugosc - x.length))
        "* " + x + przerwa + " *"
      }
      ).mkString("\n")}"
      + "\n" + "*"* (max_dlugosc+4)
}
@main
def zad_01: Unit = {
  var argument = """
        Ala
        ma
        kota i psa
    """

  var wynik = obramuj(argument)
  argument = "Ala\nma\nkota i psa"

  println(wynik)
  return
}

// def zaszyfruj(slowoklucz: String, zdanie: String): String = {
  
//   val slowoklucz2 = slowoklucz.replaceAll("\\s", "")
//   val zdanie2 = zdanie.replaceAll("\\s", "")
//   val slowokluczdlugosc = slowoklucz2.length
//   val zdaniedlugosc = zdanie2.length
//   val ilosc = zdaniedlugosc/slowokluczdlugosc
//   val reszta = zdaniedlugosc%slowokluczdlugosc
//   val slowoklucz3 = slowoklucz2*ilosc + slowoklucz2.take(reszta)
  
//   return slowoklucz3
  

// }

// @main 
// def print: Unit = {
//   val wynik = zaszyfruj("KATAR", "ZASZYFRUJ MNIE")
//   println(wynik)
// }

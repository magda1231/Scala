package lab03
import scala.annotation.tailrec

def reverse(napis: String): String = {
   @annotation.tailrec
    def pomocnicza(napis:String, acc:String): String = {
        if (napis.length == 0) acc 
        else pomocnicza(napis.tail, s"${napis.head}$acc")
    }
    pomocnicza(napis, "")
}
@main 
def zad_03(napis: String): Unit = {
    //Napisz funkcje reverse, która dla podanego napisu zwraca odwrócony napis
    //Wykorzystaj operacje head i tail na napisach
    val odwrócony = reverse(napis)
    println(s"$napis od końca to $odwrócony")
}


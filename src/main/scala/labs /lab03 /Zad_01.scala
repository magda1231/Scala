package lab03
import scala.annotation.tailrec

def ciągGeometryczny(n: Int, iloraz: Double, początek: Double): Double = {
    // wyraz poczatkowy jako akumulator
    // przez q przemnazac akumulator
    if (n == 0) return początek
    @annotation.tailrec
    def acc_helper(n: Int, acc: Double): Double = {
        if (n == 1) acc
        else acc_helper(n-1, acc*iloraz)
    }
    acc_helper(n, początek)
}
@main
 def zad_01(n: Int, iloraz: Double, początek: Double): Unit = {
    require(n>=0)
    //Zdefiniuj funkcję ciągGeometryczny tak, aby zwracała
    //n-ty wyraz ciągu geometrycznego dla zadanego ilorazu i wyrazu początkowego
    val wynik = ciągGeometryczny(n, iloraz, początek)
    println(s"a_$n dla ciągu a_n=$początek*($iloraz^n) wynosi:$wynik")
}




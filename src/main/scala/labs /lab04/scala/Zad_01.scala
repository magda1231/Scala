package lab04
import annotation.tailrec

def ciąg(n: Int): Int = {
  // C(0) == 2
// C(1) == 1
// C(n) == C(n - 1) + C(n - 2) dla n > 1

  @annotation.tailrec
  def helper(n:Int, a:Int, b:Int): Int = {
    if (n == 0) a
    else helper(n-1, b, a+b)
  }
  helper(n, 2, 1)
}

@main 
def zadanie_01: Unit = {
  // Program powinien umożliwić „sprawdzenie” działania
  // funkcji „ciąg”.
  println( ciąg(5))


}

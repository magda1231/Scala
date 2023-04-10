package lab06

def subseq[A](list: List[A], begIdx: Int, endIdx: Int): List[A] = {
  return list.take(endIdx+1).drop(begIdx)

 
}

@main 
def zadanie_01: Unit = {
 
  println(subseq(List(1, 2, 3, 4, 5), 1, 3))
}

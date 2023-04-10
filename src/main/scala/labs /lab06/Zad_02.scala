package lab06

def freqMax[A](list: List[A]): (Set[A],Int)  = {
 val maxlength = list.groupBy(x=> x).map(x=> (x._1,x._2.length)).toList.maxBy(x=>(x._2))._2
 val set = list.groupBy(x=> x).map(x=> (x._1,x._2.length)).toList.filter(x=>x._2 == maxlength)
      .foldLeft(Set.empty[A]) { (z, f) =>
        z + f._1
        }
 (set,maxlength)
}

@main def zadanie_02: Unit = {
  val l = List(1, 1, 2, 4, 4, 3, 4, 1, 3,3)
  println(freqMax(l))

}


package lab09

sealed trait MyList[+A] // definujemy typ (typ mozna zmieniac )
case object Empty extends MyList[Nothing] // definicja pustej listy 
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def head[A](list: MyList[A]): A = list match {
    case Empty => throw IllegalArgumentException("Head of an empty MyList")
    case Cons(h, tl) => h
  }
  // wynik: MyList-a zawierająca wszystkie elementy poza pierwszym
  def tail[A](list: MyList[A]): MyList[A] = list match {
    case Empty => throw IllegalArgumentException("Tail of an empty MyList")
    case Cons(h, tl) => tl
  }

  // wynik: długość MyList-y będącej argumentem
  def length[A](list: MyList[A]): Int = list match {
    case Empty => 0
    case Cons(h, tl) => 1 + length(tl)
  }

  // wynik: MyList-a zawierająca wszystkie elementy poz n początkowymi
  @annotation.tailrec
  def drop[A](list: MyList[A], n: Int): MyList[A] = list match {
    case Empty => Empty
    case Cons(h, tl) if n > 0 => drop(tl, n - 1)
    case _ => list
  }
  // wynik: „odwrócony” argument

  def reverse[A](list: MyList[A]): MyList[A] = {
    @annotation.tailrec
    def reverseHelper[A](list: MyList[A], acc: MyList[A]): MyList[A] = list match {
      case Empty => acc
      case Cons(h, tl) => reverseHelper(tl, Cons(h, acc))
    }

    reverseHelper(list, Empty)
  }

  // wynik: argument po odrzuceniu początkowych elementów spełniających p
  @annotation.tailrec
  def dropWhile[A](l: MyList[A])(p: A => Boolean): MyList[A] = l match {
    case Empty => Empty
    case Cons(h, t) if p(h) => dropWhile(t)(p)
    case Cons(h, t) => l
  }
  // wynik: połączone MyList-y list1 oraz list2

  def append[A](list1: MyList[A], list2: MyList[A]): MyList[A] = {
    @annotation.tailrec
    def appendHelper(list1: MyList[A], list2: MyList[A]): MyList[A] = list1 match {
      case Empty => list2
      case Cons(h, tail) => appendHelper(tail, Cons(h, list2))
    }

    appendHelper(reverse(list1), list2)
  }
  //   wynik: MyList-a składająca się ze wszystkich alementów argumentu, poza ostatnim
//  @annotation.tailrec
  def allButLast[A](list: MyList[A]): MyList[A] = list match {
    case Empty => Empty
    case Cons(h, Empty) => Empty
    case Cons(h,tail) => Cons(h, allButLast(tail))
  }
}

@main def listy: Unit = {
  val l1 = Cons(1, Cons(2, Cons(3, Empty)))
  val l2 = Cons(4, Cons(5, Cons(6, Empty)))
  val res = MyList.head(l1)
  println(s"MyList.head($l1) == $res")

//  println(MyList.dropWhile(l1)(_ == 2))
//  println(MyList.reverse(l1))
//  println(MyList.append(l1,l2))
  println(MyList.allButLast(l1))
  // println(MyList.head(Empty)) // spowoduje „wyjątek”

}
// creating methods 

import scala.annotation.tailrec
import scala.compiletime.ops.boolean


// method: sort list 
def sort(list: List[Int]): List[Int] = list match {
  case List() => List()
  case y :: ys => insert(sort(ys),y)
}
// method: insert list 
def insert( list: List[Int], x: Int): List[Int] = list match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: list else y :: insert(ys, x)
}

//  Find the last element of a list.
def last[A](list: List[A]): A = list match {
  case List() => null.asInstanceOf[A]
  case List(x) => x
  case x :: xs => last(xs)
}
// find last element of sequence
def last2[A](seq: Seq[A]): A = seq match {
  case Seq() => null.asInstanceOf[A]
  case Seq(x) => x
  case x +: xs => last2(xs)
}

def penultimate[A](l:List[A]):A = l match {
    case h :: List(t) => h
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
}

//find the number of elements of a list
def length[A](list: List[A]): Int = list match {
  case List() => 0
  case x :: xs => 1 + length(xs)
}
// // length with tail recursion
def length_tail_call[A](list: List[A]): Int = {
    @tailrec
    def lengthHelper[A](list:List[A],acc: Int): Int = list match {
        case List() => acc
        case h :: t => lengthHelper(t,acc+1)
    }
    lengthHelper(list,0)   
}

// // reverse list
def reverse[A](list: List[A]): List[A] = {
   @tailrec
   def reverseHelper(list: List[A], acc: List[A]): List[A] = {
        list match {
            case Nil => acc 
            case head :: tail => reverseHelper(tail, head :: acc)
        }   
   }
   reverseHelper(list,Nil)
}

// find the kth elememt of list 

def nth[A](list: List[A], nth: Int): A = {
    @tailrec
    def nthHelper[A](list: List[A], nth: Int, acc: Int): A = {
        list match {
            case Nil => throw new NoSuchElementException
            case h :: t if acc == nth => h 
            case h :: t => nthHelper(t, nth, acc + 1)
        }
    }
    nthHelper(list,nth,0)
}

// find wheather list is palindrome 

def isPalindrome[A](list: List[A]): Boolean = {
    def isPalindromeHelper[A](list: List[A], listReversed: List[A], acc: Boolean): Boolean = {
        (list, listReversed) match {
            case (Nil, Nil) => acc 
            case (Nil, _) => false 
            case (_, Nil) => false 
            case (h1 :: t1, h2 :: t2) if (h1 == h2) =>  return isPalindromeHelper(t1,t2,true)
            case (h1 :: t1, h2 :: t2) =>  return false
            
        }
    }
    isPalindromeHelper(list, reverse(list), false)

}

// flatten bez nested structure 

// flatten(List(List(1, 1), 2, List(3, List(5, 8))))
// res0: List[Any] = List(1, 1, 2, 3, 5, 8)
def flatten[A](lista: List[List[A]]): List[A] = {
    def flattenHelper[A](lista: List[List[A]], acc: List[A]): List[A] = {
        lista match {
            case Nil => acc
            case head :: tail => flattenHelper(tail, acc ::: head)
        }
    }

    flattenHelper(lista, Nil)
}
//Eliminate consecutive duplicates of list elements.


//Pack consecutive duplicates of list elements into sublists.
//  def pack[A](ls: List[A]): List[List[A]] = {
//     if (ls.isEmpty) List(List())
//     else {
//       val (packed, next) = ls span { _ == ls.head }
//       if (next == Nil) List(packed)
//       else packed :: pack(next)
//     }
//   }
// def pack[A](lista: List[A]): List[List[A]] = {
//     def packHelper[A](lista: List[A], acc: List[List[A]]): List[List[A]] = {
//         lista match {
//             case Nil => acc
//             case head :: tail => {
//                 acc match {
//                     case Nil => packHelper(tail, List(List(head)))
//                     case h :: t => {
//                         if (h.head == head) {
//                             packHelper(tail, (head :: h) :: t)
//                         } else {
//                             packHelper(tail, List(head) :: acc)
//                         }
//                     }
//                 }
//             }
//         }
//     }
//     reverse(packHelper(lista, Nil))
// }

def pack[A](l: List[A]):List[List[A]] = {
    def _pack(res: List[List[A]], rem: List[A]):List[List[A]] = rem match {
        case Nil => res
        case ls => {
            val (s: List[A], r: List[A]) = rem.span{ _ == rem(0) }
            println(s)
            println(r)
            _pack(res:::List(s), r)
        }
    }
    _pack(List(), l)
}

// span method 
def span[A](list: List[A])(predicate: A => Boolean): (List[A], List[A]) = list match {
  case Nil => (Nil, Nil)
  case head :: tail if predicate(head) => {
    val (prefix, suffix) = span(tail)(predicate)
    (head :: prefix, suffix)
  }
  case _ => (Nil, list)
}

// Decode a run-length encoded list.

def decode[A](list: List[(Int, A)]): List[A] = {
    @tailrec
    def decodeHelper[A](list: List[(Int, A)], acc: List[A]): List[A] = {
        list match {
            case Nil => acc
            case (num, elem) :: tail => {
                num match {
                    case 0 => decodeHelper(tail, acc)
                    case _ => decodeHelper((num - 1, elem) :: tail, elem :: acc)
                }
            }
        }
    }
    decodeHelper(list, Nil)
}

def dencode[A](list: List[A] ): List[(Int, A)]= {
    @tailrec
    def dencodeHelper[A](list: List[A], acc: List[(Int, A)]): List[(Int, A)] = {
        list match {
            case Nil => acc
            case h :: tail if acc == Nil => dencodeHelper(tail, (1, h) :: acc)
            case h :: t => acc match {
                case Nil => dencodeHelper(t, (1, h) :: acc)
                case (num, elem) :: tail if elem == h => dencodeHelper(t, (num + 1, elem) :: tail)
                case (num, elem) :: Nil if elem == h => dencodeHelper(t, (num + 1, elem) :: acc)
                case (num, elem) :: tail if elem != h => dencodeHelper(t, (1, h) :: acc)
                case _ => {
                    println("---" + acc + "--------" + list)
                    acc
                }
            }
        }
    }
    dencodeHelper(list, Nil)
}

  
// encode 
// scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

def encode[A](list: List[A]): List[(Int,A)] = {
    @tailrec
    def encodeHelper[A](list: List[A], acc: List[(Int,A)]):  List[(Int,A)] = {
        list match {
            case Nil => acc 
            case h :: tail if acc == Nil => encodeHelper(tail, (1, h) :: acc)
            case h :: tail => {
                val newAcc = accMatch(h, acc)
                encodeHelper(tail, newAcc)
            }
        }
    }
  
    def accMatch[A](elem: A, acc: List[(Int,A)]): List[(Int,A)] = {
        acc match {
            case Nil => (1, elem) :: acc
            case (num, el) :: tail if el == elem => (num + 1, el)  :: tail 
            case (num, el) :: tail =>  (1, elem) :: acc
        }
    }
    encodeHelper(list, Nil)
}

//check if list is empty


@main 
def print: Unit = {
  println("Hello World!")
//   val lista = List(7,2,3,4,2,6)
  val seq = Seq(7,2,3,4,5,6)
    val set = Set(7,2,3,5,6)
    // println(set(4)) // shows if set contains element
    // print first element of set
    //  println(set.head)
    // // print last element of set
    // println(set.last)
    // val hashmaps = Map(1 -> "a", 2 -> "b", 3 -> "c")
    // println(hashmaps(1)) // shows value of key 
    // println(length_tail_call(lista))
    // println(reverse(lista))
    // println(nth(lista,0))
    val lista = List(1,2,2,1,1)
    val letters = List('a','a','a','b','c','c','a')
    val listy = List(lista,lista)

    // println(pack(letters))
    // val p =  pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e', "a"))
    // span
    // val (a,b) = span(letters)(x => x == 'a')
    val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    // val sublists = span(numbers)(_ % 2 == 0)
    val x = List(15, 10, 5, 8, 20, 12)
    val y =  x.span(_ < 20)
    // println(y)
    // println(span(x)(_ < 20))
    val b = decode(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')))
    println(b)
     val l= List('a','a','a','b', null ,'c','c','a')
    //  println(encode(l))
     println(dencode(l))

   
  


    // println(a)

    // println(p)

    // println(listy)
    // println(isPalindrome(lista))
    // println(flatten(listy))





}
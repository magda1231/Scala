package lab06
// def difficult[A](list: List[A])(len: Int, shift: Int = 1): List[List[A]] 
//   = {
//     // def helper_split(list: List[A], len: Int,  acc: List[List[A]], len_acc: Int): List[List[A]] = {
//     //   list match {
//     //     case Nil => acc
//     //     case head :: tail if len_acc % len == 0 => helper_split(tail, len, (head :: List[A]()) :: acc, len_acc + 1)
//     //     case head :: tail if len_acc % len != 0 => {
//     //       acc match{
//     //         case Nil => helper_split(tail, len, (head :: List[A]()) :: acc, len_acc + 1)
//     //         case head_acc :: tail_acc if len_acc % len == 2 => helper_split(tail, len, (head :: head_acc).reverse :: tail_acc, len_acc + 1)
//     //         case head_acc :: tail_acc => helper_split(tail, len, (head :: head_acc) :: tail_acc, len_acc + 1)
//     //       }
//     //     }
//     //     case _ => acc      
//     //   }
//     // }
//     // helper_split(list, len,  Nil ,0).reverse    
//     //}

def difficult[A](list: List[A])(len: Int, shift: Int): List[List[A]] = {

    def drop[A](list: List[A], n: Int): List[A] = {
      list match {
          case Nil => Nil
          case head :: tail if n > 0 => drop(tail, n - 1)
          case _ => list
      }
    }
    def take[A](list: List[A], n: Int): List[A] = {
        def helper(list: List[A], acc: List[A], n: Int): List[A] = {
            list match {
                case Nil => acc.reverse
                case head :: tail if n > 0 => helper(tail, head :: acc, n - 1)
                case _ => acc.reverse
            }
        }
        helper(list, Nil, n)
    }

    def helper[A](list: List[A], acc: List[List[A]], len: Int, shift: Int): List[List[A]] = {
        list match {
          case Nil => acc
          case _ => {
            helper(drop(list,shift), (take(list,len) :: acc), len, shift)
          }
        }
     
    }
    helper(list, Nil, len, shift).reverse
}

@main
def zadanie_03: Unit = {
  println("Testujemy zadanie 3")
  val l = List(1, 2, 3, 4, 5)
  println(difficult(l)(2,1))
}



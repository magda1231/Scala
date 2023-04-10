// def drop[A](list: List[A], n: Int): List[A] = {
//     list match {
//         case Nil => Nil
//         case head :: tail if n > 0 => drop(tail, n - 1)
//         case _ => list

//     }
// }
// def take[A](list: List[A], n: Int): List[A] = {
//     def helper(list: List[A], acc: List[A], n: Int): List[A] = {
//         list match {
//             case Nil => acc.reverse
//             case head :: tail if n > 0 => helper(tail, head :: acc, n - 1)
//             case _ => acc.reverse
//         }
//     }
//     helper(list, Nil, n)

// }

// def reverse[A](list: List[A]): List[A] = {
//     def helper(list: List[A], acc: List[A]): List[A] = {
//         list match {
//             case Nil => acc
//             case head :: tail => helper(tail, head :: acc)
//         }
//     }
//     helper(list, Nil)
    
// }

// def last[A](list: List[A]): A = {
//     list match {
//         case Nil => null
//         case head :: Nil => head
//         case head :: tail => last(tail)
//     }
// }

// def init[A](list: List[A]): List[A] = {
//     list match {
//         case Nil => Nil
//         case head :: Nil => Nil
//         case head :: tail => head :: init(tail)
//     }
// }

// def concat[A](list1: List[A], list2: List[A]): List[A] = {
//     list1 match {
//         case Nil => list2
//         case head :: tail => head :: concat(tail, list2)
//     }
// }

// @main 
// def testdrop: Unit = {
//   val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
// //   println(drop(3, l))
//   println(drop(l,3))
//   println(take(l,3))
//   println(reverse(l))
// }





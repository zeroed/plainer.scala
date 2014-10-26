/**
 *
 */
package plainer.utils {
  
  /**
   * @author eddie
   *
   */
  package sorter {
    
    object MergeSort {
      def sort[T](less: (T, T)=> Boolean)(xs: List[T]): List[T] = { 
        def merge(xs: List[T], ys: List[T]): List[T] = (xs,ys) match { 
          case (Nil, _) => ys 
          case (_, Nil) => xs 
          case (x :: xs1, y :: ys1) => if (less(x,y)) x :: merge (xs1, ys) 
                                       else y :: merge(xs,ys1) 
        } 
        val n = xs.length / 2 
        if (n == 0) xs 
        else { 
          val (ys, zs) = xs splitAt n 
          merge(sort(less)(ys), sort(less)(zs)) 
        } 
      }
    }
    
    object InsertionSort {
      def sort(xs: List[Int]): List[Int] = {
        if (xs.isEmpty) Nil
        else insert(xs.head, sort(xs.tail)) 
      }
      
      def insert(x: Int, xs: List[Int]): List[Int] = {
        if (xs.isEmpty || x <= xs.head) x :: xs
        else xs.head :: insert(x, xs.tail)
      }
    }
    
    package test {
      
    }
  }
}

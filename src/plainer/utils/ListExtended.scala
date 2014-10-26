/**
 *
 */
package plainer.utils

/**
 * @author eddie
 *
 */
class ListExtended(val l: List[Int]) {
  def hasNegatives: Boolean = l.exists(_ < 0)
  implicit def listToListExtended(list: List[Int]) = { new ListExtended(list) }
}
/**
 *
 */
package plainer.utils

/**
 * @author eddie
 *
 */
class IntExtended(val i: Int) {
  def isModuleOf(m: Int) = if (i % m == 0) true else false
  implicit def intToInt(i: Int) = new IntExtended(i)
}
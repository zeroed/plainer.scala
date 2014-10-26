/**
 *
 */
package plainer.items

/**
 * @author eddie
 *
 */
class Stamp(val content: String) {
  def apply(times: Int):String = List.fill(times){ content }.mkString(" ")
  def concat(other: String):String = content + other
}
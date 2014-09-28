/**
 *
 */
package plainer.items

/**
 * @author eddie
 *
 */
class Stamp(content: String) {
  def apply(times: Int) = List.fill(times){ content }.mkString(" ")
}
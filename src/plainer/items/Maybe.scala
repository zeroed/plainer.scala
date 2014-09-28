/**
 *
 */
package plainer.items

import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._


/**
 * @author eddie
 *
 */
class Maybe(n: String, v: Option[Double], d: Date = new Date) {
  val name: String = n
  var value: Option[Double] = v
  val date: Date = d
  def nullame = value = None: Option[Double]
}
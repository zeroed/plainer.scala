/**
 *
 */
package plainer.items

import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._

/**
  Declaration    Getter?    Setter?
  -----------    -------    -------
  var            yes        yes
  val            yes        no
  default        no         no

 */

/**
 * @author eddie
 *
 */
class Maybe(
  val name: String, 
  var value: Option[Double], 
  val date: Date = new Date) {
  
  def nullame() = value = None: Option[Double]
}
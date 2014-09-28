/**
 *
 */
package plainer.items

import plainer.traits.Cache

/**
 * @author eddie
 *
 */
class Box(key: String, paramValue: Int) extends Cache[String, Int] {
  private var value: Option[Int] = Some(paramValue)

  def get(paramKey: String): Option[Int] = {
    if (key.equals(paramKey)) value 
    else None
  }

  def put(paramKey: String, paramValue: Int) = {
    println("putting %d in %s".format(paramValue, paramKey))
    if (key.equals(paramKey)) {
      value = Some(paramValue)
      true
    } else false
  }

  def delete(paramKey: String) = {
    println("nulling %s".format(paramKey))
    if (key.equals(paramKey)) {
      value = None: Option[Int]
      true
    } else false
  }

  override def toString: String = {
    "I'm a Box: key %s, value %s".format(key, value)
  }
}
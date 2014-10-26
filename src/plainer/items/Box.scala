/**
 *
 */
package plainer.items

import plainer.traits.Cache

/**
 * @author eddie
 *
 */
class Box(val key: String, var value: Option[Int]) extends Cache[String, Int] {
  //private var value: Option[Int] = Some(paramValue)

  def this(key: String, value: Int) {
    this(key, Option[Int](value))
  }
  
  def get(paramKey: String): Option[Int] = {
    if (key.equals(paramKey)) value
    else None
  }

  def put(key: String, paramValue: Int) = {
    println("I have (this) %s and your are putting %d in %s".format(this.key, paramValue, key))
    if (this.key.equals(key)) {
      value = Option[Int](paramValue)
      true
    } else false
  }

  def delete(key: String) = {
    println("nulling %s".format(key))
    if (this.key.equals(key)) {
      value = None: Option[Int]
      true
    } else false
  }

  def unary_! : String = { "Not!" }
  
  override def toString: String = {
    "I'm a Box: key %s, value %s".format(key, value)
  }
}
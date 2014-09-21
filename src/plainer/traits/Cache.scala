/**
 *
 */
package plainer.traits

/**
 * @author eddie
 *
 */
trait Cache[K, V] {
  def get(key: K): Option[V]
  def put(key: K, value: V): Boolean
  def delete(key: K): Boolean
}
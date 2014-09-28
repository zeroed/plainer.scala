/**
 *
 */
package plainer.items

/**
 * @author eddie
 *
 */
object SetCreator {

  def getMutable(args: String*) = scala.collection.mutable.Set(args)
  def getImmutable(args: String*) = scala.collection.immutable.Set(args)
  
}
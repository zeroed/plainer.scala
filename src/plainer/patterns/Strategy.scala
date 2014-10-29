package plainer.patterns

package Strategy {

  trait Animal {
    def makeNoise: Unit
  }
  
  trait Cat extends Animal {
    override def makeNoise { println("Meow") }
  }
  
  trait Dog extends Animal {
    override def makeNoise { println("Woof") }
  }
  
  class AnimalContainer {
    self: Animal =>
  
    def doAnimalStuff {
      makeNoise
    }
  }
  
  object StrategyExample extends App {
      val ex1 = new AnimalContainer with Dog
      val ex2 = new AnimalContainer with Cat
  
      ex1.doAnimalStuff
      ex2.doAnimalStuff
  }
  
  
  /**
   * Another example
   */
  
  package object AnotherExample {
    trait TaxPayer
    case class Employee(sal: Long) extends TaxPayer
    case class NonProfitOrg(funds: BigInt) extends TaxPayer
  
    //Consider a generic tax calculation function. (It can be in TaxPayer also).
    def calculateTax[T <: TaxPayer](victim: T, taxingStrategy: (T => Long)) = {
      taxingStrategy(victim)
    }
  
    val employee = new Employee(1000)
    //A strategy to calculate tax for employees
    def empStrategy(e: Employee): Long = { Math.ceil(e.sal * .3) toLong }
    calculateTax(employee, empStrategy)
    
    val npo = new NonProfitOrg(100000000)
    //The tax calculation strategy for npo is trivial, so we can inline it
    calculateTax(npo, ((t: TaxPayer) => 0))
  }
}
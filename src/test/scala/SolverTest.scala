
import org.scalatest.funsuite.AnyFunSuite
import ru.dm4x.evopoker.Solver


class SolverTest extends AnyFunSuite {
  private val testingClass = Solver
  private val testingClassName: String = testingClass.getClass.getName

  test(s"$testingClassName: fiveCard method") {
    assert(testingClass.fiveCard(List("6s6h6dKcKh"))
      equals Option("List(Hand(List(Card(6,s), Card(6,h), Card(6,d), Card(K,c), Card(K,h)),34000000))"))
  }

  test(s"$testingClassName: process method") {
    assert(
      testingClass.process("texas-holdem 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d")
        equals "Error: not created yet")

    assert(
      testingClass.process("texas-holdem 2h3h4h5d8d KdKs 9hJh")
        equals "Error: not created yet")

    assert(
      testingClass.process("omaha-holdem 3d3s4d6hJc Js2dKd8c KsAsTcTs Jh2h3c9c Qc8dAd6c 7dQsAc5d")
        equals "Error: not created yet")

    assert(
      testingClass.process("five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c")
        equals "4c8h2h6c9c Ah9d6s2cKh Kd9sAs3cQs 7h4s4h8c9h Tc5h6dAc5c")

  }
}

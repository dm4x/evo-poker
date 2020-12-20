
import org.scalatest.funsuite.AnyFunSuite
import ru.dm4x.evopoker.Solver


class SolverTest extends AnyFunSuite {
  private val testingClass = Solver
  private val testingClassName: String = testingClass.getClass.getName

  test(s"$testingClassName: process: texas holdem") {
    assert(
      testingClass.process("texas-holdem 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d")
        equals "Error: not created yet")
  }

  test(s"$testingClassName: process: omaha holdem") {
    assert(
      testingClass.process("omaha-holdem 3d3s4d6hJc Js2dKd8c KsAsTcTs Jh2h3c9c Qc8dAd6c 7dQsAc5d")
        equals "Error: not created yet")
  }

  test(s"$testingClassName: process: high card with Ace > high card with King") {
    assert(
      testingClass.process("five-card-draw Ah3s4d5c8h Kh4s3d2cJh")
        equals "Kh4s3d2cJh Ah3s4d5c8h")
  }

  test(s"$testingClassName: process: one pair > high card") {
    assert(
      testingClass.process("five-card-draw 7h4s4h8c9h Tc5h6dAc7c")
        equals "Tc5h6dAc7c 7h4s4h8c9h")
  }

  test(s"$testingClassName: process: two pairs > one pair") {
    assert(
      testingClass.process("five-card-draw 7h4s4h8c8h 7h4s4h8c9h")
        equals "7h4s4h8c9h 7h4s4h8c8h")
  }

  test(s"$testingClassName: process: three > two pairs") {
    assert(
      testingClass.process("five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c")
        equals "")
  }

  test(s"$testingClassName: process: straight > three") {
    assert(
      testingClass.process("five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c")
        equals "")
  }

  test(s"$testingClassName: process: flush > straight") {
    assert(
      testingClass.process("five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c")
        equals "")
  }

  test(s"$testingClassName: process: fh > flush") {
    assert(
      testingClass.process("five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c")
        equals "")
  }

  test(s"$testingClassName: process: four > fh") {
    assert(
      testingClass.process("five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c")
        equals "")
  }

  test(s"$testingClassName: process: sf > four") {
    assert(
      testingClass.process("five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c")
        equals "")
  }
}

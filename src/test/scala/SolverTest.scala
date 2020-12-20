
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
      testingClass.process("five-card-draw QhQdQs2d3c 7h4s4h8c8h")
        equals "7h4s4h8c8h QhQdQs2d3c")
  }

  test(s"$testingClassName: process: straight > three") {
    assert(
      testingClass.process("five-card-draw 2s3d4h5c6s QhQdQs2d3c")
        equals "QhQdQs2d3c 2s3d4h5c6s")
  }

  test(s"$testingClassName: process: flush > straight") {
    assert(
      testingClass.process("five-card-draw 2s4s6s8sTs 2s3d4h5c6s")
        equals "2s3d4h5c6s 2s4s6s8sTs")
  }

  test(s"$testingClassName: process: fh > flush") {
    assert(
      testingClass.process("five-card-draw 4d4s4c2h2c 2s4s6s8sTs")
        equals "2s4s6s8sTs 4d4s4c2h2c")
  }

  test(s"$testingClassName: process: four > fh") {
    assert(
      testingClass.process("five-card-draw 2d2s2c2hAh 4d4s4c2h2c")
        equals "4d4s4c2h2c 2d2s2c2hAh")
  }

  test(s"$testingClassName: process: sf > four") {
    assert(
      testingClass.process("five-card-draw 2h3h4h5h6h 4d4s4c4hAh")
        equals "4d4s4c4hAh 2h3h4h5h6h")
  }

  test(s"$testingClassName: process: one pair, different high card") {
    assert(
      testingClass.process("five-card-draw 7h5s5h8c9h 7h4s4h8c9h")
        equals "7h4s4h8c9h 7h5s5h8c9h")
  }

  test(s"$testingClassName: process: two pairs, different high card") {
    assert(
      testingClass.process("five-card-draw Ah4s4h8c8h 7h4s4h8c8h")
        equals "7h4s4h8c8h Ah4s4h8c8h")
  }

  test(s"$testingClassName: process: three, different rank") {
    assert(
      testingClass.process("five-card-draw QhQdQs2dTc 7h7d7s2d3c")
        equals "7h7d7s2d3c QhQdQs2dTc")
  }

  test(s"$testingClassName: process: four, different rank") {
    assert(
      testingClass.process("five-card-draw QdQsQcQhAh 4d4s4c4hAh")
        equals "4d4s4c4hAh QdQsQcQhAh")
  }

  test(s"$testingClassName: process: different flush rank") {
    assert(
      testingClass.process("five-card-draw 2s4s6s8sAs 2s4s6s8sTs")
        equals "2s4s6s8sTs 2s4s6s8sAs")
  }

  test(s"$testingClassName: process: different straight rank") {
    assert(
      testingClass.process("five-card-draw 9sTdJhQcKs 2s3d4h5c6s")
        equals "2s3d4h5c6s 9sTdJhQcKs")
  }

  test(s"$testingClassName: process: different fullhouse rank") {
    assert(
      testingClass.process("five-card-draw 8d8s8c2h2c 4d4s4c2s2d")
        equals "4d4s4c2s2d 8d8s8c2h2c")
  }

  test(s"$testingClassName: process: some random board") {
    assert(
      testingClass.process("five-card-draw 7h4s4h8c9h Tc5h6dAc5c Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c")
        equals "4c8h2h6c9c Ah9d6s2cKh Kd9sAs3cQs 7h4s4h8c9h Tc5h6dAc5c")
  }

  test(s"$testingClassName: process: some random board with equity") {
    assert(
      testingClass.process("five-card-draw 7h4s4h8c9h 7c4d4c8h9d Kd9sAs3cQs Ah9d6s2cKh 4c8h2h6c9c")
        equals "4c8h2h6c9c Ah9d6s2cKh Kd9sAs3cQs 7c4d4c8h9d 7h4s4h8c9h")
  }

}


import org.scalatest.funsuite.AnyFunSuite
import ru.dm4x.evopoker.Solver
import ru.dm4x.evopoker.entity.{Card, Hand}

class SolverTest extends AnyFunSuite {
  private val testingClass = Solver
  private val testingClassName: String = testingClass.getClass.getName

  test(s"$testingClassName: fiveCard method") {
    assert(testingClass.fiveCard(List("6s6h6dKcKh")) == Option(""))
  }

  test(s"$testingClassName: splitHand method") {
    assert(testingClass.splitHand("6s6h6dKcKh")
      ==
      Hand(List(Card('6','s'),Card('6','h'),Card('6','d'),Card('K','c'),Card('K','h')) )
    )
  }

  test(s"$testingClassName: splitHandRec method") {
    assert(
      testingClass.splitHandRec("6s6h6dKcKh", List.empty)
        ==
        List(Card('6','s'),Card('6','h'),Card('6','d'),Card('K','c'),Card('K','h'))
    )
  }

}

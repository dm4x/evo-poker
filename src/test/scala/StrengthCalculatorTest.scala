import org.scalatest.funsuite.AnyFunSuite
import ru.dm4x.evopoker.StrengthCalculator
import ru.dm4x.evopoker.entity.{Card, Hand}

class StrengthCalculatorTest extends AnyFunSuite {

  private val testingClassName: String = "StrengthCalculator"

  test(s"$testingClassName: one pair") {
    val calculator = new StrengthCalculator
    val onePair = Hand("6h6dThAhKh",
      List(Card('6', 'h'), Card('6', 'd'), Card('T', 'h'), Card('A', 'h'), Card('K', 'h') ) )
    val calculatedHand = calculator.evaluate(onePair)
    assert(calculator.haveOnePair)
    assert(calculatedHand.isInstanceOf[Hand])
    assert(calculatedHand.strength == 390)
  }

  test(s"$testingClassName: two pair") {
    val calculator = new StrengthCalculator
    val twoPair = Hand("6h6dThThKh",
      List(Card('6', 'h'), Card('6', 'd'), Card('T', 'h'), Card('T', 'h'), Card('K', 'h') ) )
    val calculatedHand = calculator.evaluate(twoPair)
    assert(calculator.haveTwoPair)
    assert(calculatedHand.isInstanceOf[Hand])
    assert(calculatedHand.strength == 3500)
  }

  test(s"$testingClassName: three of a kind") {
    val calculator = new StrengthCalculator
    val three = Hand("9h9s9dAhKh",
      List(Card('9', 'h'), Card('9', 's'), Card('9', 'd'), Card('A', 'h'), Card('K', 'h') ) )
    val calculatedHand = calculator.evaluate(three)
    assert(calculator.haveThreeOfAKind)
    assert(calculatedHand.isInstanceOf[Hand])
    assert(calculatedHand.strength == 44000)
  }

  test(s"$testingClassName: four of a kind") {
    val calculator = new StrengthCalculator
    val four = Hand("ThTdTcTsKh",
      List(Card('T', 'h'), Card('T', 'd'), Card('T', 'c'), Card('T', 's'), Card('K', 'h') ) )
    val calculatedHand = calculator.evaluate(four)
    assert(calculator.haveFourOfAKind)
    assert(calculatedHand.isInstanceOf[Hand])
    assert(calculatedHand.strength == 430000000)
  }

  test(s"$testingClassName: straight") {
    val calculator = new StrengthCalculator
    val straight = Hand("2h3d4c5s6h",
      List(Card('2', 'h'), Card('3', 'd'), Card('4', 'c'), Card('5', 's'), Card('6', 'h') ) )
    val calculatedHand = calculator.evaluate(straight)
    assert(calculator.haveStraight)
    assert(calculatedHand.isInstanceOf[Hand])
    assert(calculatedHand.strength == 100000)
  }

  test(s"$testingClassName: low straight") {
    val calculator = new StrengthCalculator
    val straight = Hand("Ah2d3c4s5h",
      List(Card('A', 'h'), Card('2', 'd'), Card('3', 'c'), Card('4', 's'), Card('5', 'h') ) )
    val calculatedHand = calculator.evaluate(straight)
    assert(calculator.haveStraight)
    assert(calculatedHand.isInstanceOf[Hand])
    assert(calculatedHand.strength == 180000)
  }

  test(s"$testingClassName: high straight") {
    val calculator = new StrengthCalculator
    val straight = Hand("ThJdQcKsAh",
      List(Card('T', 'h'), Card('J', 'd'), Card('Q', 'c'), Card('K', 's'), Card('A', 'h') ) )
    val calculatedHand = calculator.evaluate(straight)
    assert(calculator.haveStraight)
    assert(calculatedHand.isInstanceOf[Hand])
    assert(calculatedHand.strength == 500000)
  }

  test(s"$testingClassName: flush") {
    val calculator = new StrengthCalculator
    val flush = Hand("6h7hThAhKh",
      List(Card('6', 'h'), Card('7', 'h'), Card('T', 'h'), Card('A', 'h'), Card('K', 'h') ) )
    val calculatedHand = calculator.evaluate(flush)
    assert(calculator.haveFlush)
    assert(calculatedHand.isInstanceOf[Hand])
    assert(calculatedHand.strength == 4000000)
  }

  test(s"$testingClassName: full house") {
    val calculator = new StrengthCalculator
    val fullhouse = Hand("6s6h6dKcKh",
      List(Card('6', 's'), Card('6', 'h'), Card('6', 'd'), Card('K', 'c'), Card('K', 'h')) )
    val calculatedHand = calculator.evaluate(fullhouse)
    assert(calculator.haveFullHouse)
    assert(calculatedHand.isInstanceOf[Hand])
    assert(calculatedHand.strength == 34000000)
  }

  test(s"$testingClassName: straight flush") {
    val calculator = new StrengthCalculator
    val straightFlush = Hand("Ah2h3h4h5h",
      List(Card('A', 'h'), Card('2', 'h'), Card('3', 'h'), Card('4', 'h'), Card('5', 'h') ) )
    val calculatedHand = calculator.evaluate(straightFlush)
    assert(calculator.haveStraightFlush)
    assert(calculatedHand.isInstanceOf[Hand])
    assert(calculatedHand.strength == 1800000000)
  }
}

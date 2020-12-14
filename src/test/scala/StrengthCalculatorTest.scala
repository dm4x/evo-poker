import org.scalatest.funsuite.AnyFunSuite
import ru.dm4x.evopoker.StrengthCalculator
import ru.dm4x.evopoker.entity.{Card, Hand}

class StrengthCalculatorTest extends AnyFunSuite {

  private val testingClassName: String = "StrengthCalculator"

  test(s"$testingClassName: one pair") {
    val calculator = new StrengthCalculator
    val onePair = Hand(
      List(Card('6', 'h'), Card('6', 'd'), Card('T', 'h'), Card('A', 'h'), Card('K', 'h') ) )
    val handStrength = calculator.evaluate(onePair)
    assert(calculator.haveOnePair)
    assert(handStrength == 390)
  }

  test(s"$testingClassName: two pair") {
    val calculator = new StrengthCalculator
    val twoPair = Hand(
      List(Card('6', 'h'), Card('6', 'd'), Card('T', 'h'), Card('T', 'h'), Card('K', 'h') ) )
    val handStrength = calculator.evaluate(twoPair)
    assert(calculator.haveTwoPair)
    assert(handStrength == 3500)
  }

  test(s"$testingClassName: three of a kind") {
    val calculator = new StrengthCalculator
    val three = Hand(
      List(Card('9', 'h'), Card('9', 'h'), Card('9', 'd'), Card('A', 'h'), Card('K', 'h') ) )
    val handStrength = calculator.evaluate(three)
    assert(calculator.haveThreeOfAKind)
    assert(handStrength == 44000)
  }

  test(s"$testingClassName: four of a kind") {
    val calculator = new StrengthCalculator
    val four = Hand(
      List(Card('T', 'h'), Card('T', 'd'), Card('T', 'c'), Card('T', 's'), Card('K', 'h') ) )
    val handStrength = calculator.evaluate(four)
    assert(calculator.haveFourOfAKind)
    assert(handStrength == 430000000)
  }

  test(s"$testingClassName: straight") {
    val calculator = new StrengthCalculator
    val straight = Hand(
      List(Card('2', 'h'), Card('3', 'd'), Card('4', 'c'), Card('5', 's'), Card('6', 'h') ) )
    val handStrength = calculator.evaluate(straight)
    assert(calculator.haveStraight)
    assert(handStrength == 100000)
  }

  test(s"$testingClassName: low straight") {
    val calculator = new StrengthCalculator
    val straight = Hand(
      List(Card('A', 'h'), Card('2', 'd'), Card('3', 'c'), Card('4', 's'), Card('5', 'h') ) )
    val handStrength = calculator.evaluate(straight)
    assert(calculator.haveStraight)
    assert(handStrength == 180000)
  }

  test(s"$testingClassName: high straight") {
    val calculator = new StrengthCalculator
    val straight = Hand(
      List(Card('T', 'h'), Card('J', 'd'), Card('Q', 'c'), Card('K', 's'), Card('A', 'h') ) )
    val handStrength = calculator.evaluate(straight)
    assert(calculator.haveStraight)
    assert(handStrength == 500000)
  }

  test(s"$testingClassName: flush") {
    val calculator = new StrengthCalculator
    val flush = Hand(
      List(Card('6', 'h'), Card('7', 'h'), Card('T', 'h'), Card('A', 'h'), Card('K', 'h') ) )
    val handStrength = calculator.evaluate(flush)
    assert(calculator.haveFlush)
    assert(handStrength == 4000000)
  }

  test(s"$testingClassName: full house") {
    val calculator = new StrengthCalculator
    val fullhouse = Hand(
      List(Card('6', 's'), Card('6', 'h'), Card('6', 'd'), Card('K', 'c'), Card('K', 'h')) )
    val handStrength = calculator.evaluate(fullhouse)
    assert(calculator.haveFullHouse)
    assert(handStrength == 34000000)
  }

  test(s"$testingClassName: straight flush") {
    val calculator = new StrengthCalculator
    val straightFlush = Hand(
      List(Card('A', 'h'), Card('2', 'h'), Card('3', 'h'), Card('4', 'h'), Card('5', 'h') ) )
    val handStrength = calculator.evaluate(straightFlush)
    assert(calculator.haveStraightFlush)
    assert(handStrength == 1800000000)
  }
}

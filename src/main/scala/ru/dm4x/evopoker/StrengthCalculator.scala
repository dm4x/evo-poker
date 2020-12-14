package ru.dm4x.evopoker

import ru.dm4x.evopoker.entity.{Card, Hand}

import scala.collection.mutable.ArrayBuffer

class StrengthCalculator {

  private val diamonds = ArrayBuffer(0,0,0,0,0,0,0,0,0,0,0,0,0)
  private val hearts = ArrayBuffer(0,0,0,0,0,0,0,0,0,0,0,0,0)
  private val spades = ArrayBuffer(0,0,0,0,0,0,0,0,0,0,0,0,0)
  private val clubs = ArrayBuffer(0,0,0,0,0,0,0,0,0,0,0,0,0)

  private def setDiamond(cardRank: Int): Unit = addOne(diamonds, cardRank)
  private def setHeart(cardRank: Int): Unit = addOne(hearts, cardRank)
  private def setSpade(cardRank: Int): Unit = addOne(spades, cardRank)
  private def setClub(cardRank: Int): Unit = addOne(clubs, cardRank)

  def getDiamonds: ArrayBuffer[Int] = diamonds
  def getHearts: ArrayBuffer[Int] = hearts
  def getSpades: ArrayBuffer[Int] = spades
  def getClubs: ArrayBuffer[Int] = clubs


  private var isFourOfAKind: Boolean = false
  private var isThreeOfAKind: Boolean = false
  private var isOnePair: Boolean = false
  private var isTwoPair: Boolean = false
  private var isFlush: Boolean = false
  private var isStraight: Boolean = false
  private var lowStraight = false

  private var threeRank: Int = 0
  private var pairRank: Int = 0
  private var secondPairRank: Int = 0
  private var handRanksSum: Int = 0

  private var counter = 0
  private var prevFilledPosition = 0


  /**
   *
   * @param hand a list of `Card` in hand
   * @return total strength of cards in hand
   */
  def evaluate(hand: Hand): Long = {
    hand.cards.foreach(fillSuits)
    checkCombo()
    handStrength(handRanksSum)
  }

  def haveStraightFlush: Boolean = isStraight && isFlush
  def haveFullHouse: Boolean = isOnePair && isThreeOfAKind && notTheSameRank
  def haveFlush: Boolean = isFlush
  def haveTwoPair: Boolean = isTwoPair
  def haveOnePair: Boolean = isOnePair
  def haveThreeOfAKind: Boolean = isThreeOfAKind
  def haveFourOfAKind: Boolean = isFourOfAKind
  def haveStraight: Boolean = isStraight

  private def checkCombo(): Unit = {
    counter = 0
    prevFilledPosition = -1

    isFlush = checkFlush()

    /*
     * lengths of arrays are equal
     */
    for (position <- diamonds.indices) {
      val rankSum = sumOfRank(position)

      if (rankSum > 1) checkPairThreeAndFour(rankSum, position)
      else if (rankSum == 1) {
        checkStraight(prevFilledPosition, position)
      }

      handRanksSum = handRanksSum + (rankSum * position)
    }
  }

  private def checkStraight(prevPosition: Int, position: Int): Unit = {
    if (position - prevPosition == 1 || (position - prevPosition != 1 && counter == 0) ) {
      counter = counter + 1
      prevFilledPosition = position
    } else counter = 0
    if (position == 3 && counter == 4) lowStraight = true
    if (position == 12 && lowStraight) isStraight = true
    if (counter == 5) isStraight = true
  }

  private def checkPairThreeAndFour(rankSum: Int, position: Int): Unit = {
    if (rankSum == 4) isFourOfAKind = true
    else if (rankSum == 3) {isThreeOfAKind = true; threeRank = position}
    else if (rankSum == 2 && pairRank > 0) {isTwoPair = true; secondPairRank = position}
    else if (rankSum == 2) {isOnePair = true; pairRank = position}
  }

  private def notTheSameRank:Boolean = pairRank != threeRank

  private def sumOfRank(position: Int):Int = diamonds(position) + hearts(position) + spades(position) + clubs(position)

  private def checkFlush(): Boolean = {
    diamonds.count(_ == 1) == 5 || hearts.count(_ == 1) == 5 || spades.count(_ == 1) == 5 || clubs.count(_ == 1) == 5
  }

  private def fillSuits(card: Card): Unit = card match {
    case Card(_, 'h') => fillConcreteSuit(card, setHeart)
    case Card(_, 'd') => fillConcreteSuit(card, setDiamond)
    case Card(_, 's') => fillConcreteSuit(card, setSpade)
    case Card(_, 'c') => fillConcreteSuit(card, setClub)
  }

  private def fillConcreteSuit(card: Card, fillFunction: Int => Unit): Unit = card match {
    case Card('A', _) => fillFunction(12)
    case Card('K', _) => fillFunction(11)
    case Card('Q', _) => fillFunction(10)
    case Card('J', _) => fillFunction(9)
    case Card('T', _) => fillFunction(8)
    case Card('9', _) => fillFunction(7)
    case Card('8', _) => fillFunction(6)
    case Card('7', _) => fillFunction(5)
    case Card('6', _) => fillFunction(4)
    case Card('5', _) => fillFunction(3)
    case Card('4', _) => fillFunction(2)
    case Card('3', _) => fillFunction(1)
    case Card('2', _) => fillFunction(0)
  }

  private def handStrength (handRanksSum: Long): Long = {
    if (haveStraightFlush) {100_000_000 * handRanksSum}
    else if (haveFourOfAKind) {10_000_000 * handRanksSum}
    else if (haveFullHouse) {1_000_000 * handRanksSum}
    else if (haveFlush) {100_000 * handRanksSum}
    else if (haveStraight) {10_000 * handRanksSum}
    else if (haveThreeOfAKind) {1000 * handRanksSum}
    else if (haveTwoPair) {100 * handRanksSum}
    else if (haveOnePair) {10 * handRanksSum}
    else 1
  }

  /**
   * add card to array by its rank as position
   * */
  private def addOne(array: ArrayBuffer[Int], position: Int): Unit = array(position) = array(position) + 1

}

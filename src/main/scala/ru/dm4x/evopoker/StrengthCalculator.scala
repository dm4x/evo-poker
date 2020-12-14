package ru.dm4x.evopoker

import ru.dm4x.evopoker.entity.{Card, Hand}

import scala.collection.mutable.ArrayBuffer

class StrengthCalculator {

  /**
   *
   * @param hand a list of `Card` in hand
   * @return total strength of cards in hand
   */
  def evaluate(hands: List[Hand]): List[Hand] = {
    hands
      .map(calcHandStrength)
      .map(checkFlush)
      .map(checkThreeOfAKind)
      .map(checkTwoPair)
      .map(checkOnePair)

  }

  private def calcHandStrength(hand: Hand): Hand = {
    Hand(hand.inHand, hand.cards.map(calcCardStrength))
  }

  private def calcCardStrength(card: Card): Card = card.rank match {
    case 'A' => Card(card.rank, card.suit, strength = 14)
    case 'K' => Card(card.rank, card.suit, strength = 13)
    case 'Q' => Card(card.rank, card.suit, strength = 12)
    case 'J' => Card(card.rank, card.suit, strength = 11)
    case 'T' => Card(card.rank, card.suit, strength = 10)
    case '9' => Card(card.rank, card.suit, strength = 9)
    case '8' => Card(card.rank, card.suit, strength = 8)
    case '7' => Card(card.rank, card.suit, strength = 7)
    case '6' => Card(card.rank, card.suit, strength = 6)
    case '5' => Card(card.rank, card.suit, strength = 5)
    case '4' => Card(card.rank, card.suit, strength = 4)
    case '3' => Card(card.rank, card.suit, strength = 3)
    case '2' => Card(card.rank, card.suit, strength = 2)
    case _ => Card(card.rank, card.suit)
  }

  private def checkFlush(hand: Hand): Hand = {
    if (hand.cards.count(_.suit == 'h') == 5
      ||hand.cards.count(_.suit == 'd') == 5
      ||hand.cards.count(_.suit == 'c') == 5
      ||hand.cards.count(_.suit == 's') == 5) {
      Hand(hand.inHand, hand.cards, hasFlush = true)
    } else Hand(hand.inHand, hand.cards)
  }

  private def checkThreeOfAKind(hand: Hand): Hand = {
    hand.cards.count(_.rank == rank).equals(3)
  }

  private def checkOnePair(hand: Hand): Hand = hand.cards.count(_.rank == rank).equals(2)

  private def checkTwoPair(hand: Hand): Hand = {
    hand.cards.count(_.rank == rank).equals(4)

  }



  def haveStraightFlush: Boolean = isStraight && isFlush

  def haveFullHouse: Boolean = isOnePair && isThreeOfAKind && notTheSameRank

  def haveFourOfAKind: Boolean = isTwoPair && notTheSameRank

  private def notTheSameRank: Boolean = pairRank != threeRank

  private def handStrength(handRanksSum: Long): Long = {
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

}

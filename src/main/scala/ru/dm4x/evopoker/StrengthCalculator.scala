package ru.dm4x.evopoker

import ru.dm4x.evopoker.entity.{Card, Combo, Empty, Flush, FourOfAKind, FullHouse, Hand, OnePair, Straight, StraightFlush, ThreeOfAKind, TwoPair}

import scala.annotation.tailrec

class StrengthCalculator {

  /**
   *
   * @param hand a list of `Card` in hand
   * @return total strength of cards in hand
   */
  def evaluate(hands: List[Hand]): List[Hand] = {
    hands.sortBy(_.strength)
      .map(flush)
      .map(onePairTwoPairFourOfAKindFullHouse(_, 0))
      .map(threeOfAKindFullHouse(_, 0))
      .map(straight)
      .map(calcHandStrength)
  }

  private def calcHandStrength(hand: Hand): Hand = {
    // считаем силу руки, сила карт уже посчитана

  }



  private def flush(hand: Hand): Hand = {
    val maxRank = hand.cards.maxBy(_.rank).rank
    val isFlush = hand.cards.map(_.suit).map(isFlushSuit(hand, _)).find(_ == true).getOrElse(false)
    (hand.combo, isFlush) match {
      case (Straight(_), true) => Hand(hand.inHand, hand.cards, hand.strength, StraightFlush(maxRank))
      case (Straight(_), false) => Hand(hand.inHand, hand.cards, hand.strength, Straight(maxRank))
      case (_, true) => Hand(hand.inHand, hand.cards, hand.strength, Flush(maxRank))
      case _ => hand
    }
  }

  private def isFlushSuit(hand: Hand, suit: Char): Boolean = {
    if (hand.cards.count(_.suit == suit) == 5) true
    else false
  }

  private def straight(hand: Hand): Hand = {
    val maxRank = hand.cards.maxBy(_.rank).rank
    val isStraight = isStraightSuit(0, hand.cards.map(_.rank), maxRank, result = false)
    (hand.combo, isStraight) match {
      case (Flush(_), true) => Hand(hand.inHand, hand.cards, hand.strength, StraightFlush(maxRank))
      case (Flush(_), false) => Hand(hand.inHand, hand.cards, hand.strength, Flush(maxRank))
      case (_, true) => Hand(hand.inHand, hand.cards, hand.strength, Straight(maxRank))
      case _ => hand
    }
  }

  @tailrec
  private def isStraightSuit(prev: Int, ranks: List[Int], highRank: Int, result: Boolean): Boolean = {
    if (ranks.head == highRank) result
    else if (ranks.head - prev == 1) isStraightSuit(ranks.head, ranks.tail, highRank, result = true)
    else false
  }

  @tailrec
  private def threeOfAKindFullHouse(hand: Hand, rank: Int): Hand = {
      if (rank == 15) hand
      else if (hand.cards.count(_.rank == rank).equals(3)) {
        threeOfAKindFullHouse(Hand(hand.inHand, hand.cards, hand.strength, fullHouse(hand, rank)), rank + 1)
      }
      else threeOfAKindFullHouse(hand, rank + 1)
  }

  @tailrec
  private def onePairTwoPairFourOfAKindFullHouse(hand: Hand, rank: Int): Hand = {
    if (rank == 15) hand
    else if (hand.cards.count(_.rank == rank).equals(2)) {
      onePairTwoPairFourOfAKindFullHouse(Hand(hand.inHand, hand.cards, hand.strength, fullHouse(hand, rank)), rank + 1)
    }
    else onePairTwoPairFourOfAKindFullHouse(hand, rank + 1)
  }

  private def fullHouse (hand: Hand, rank: Int): Combo = hand.combo match {
    case ThreeOfAKind(_) => FullHouse(hand.combo.rank, rank)
    case OnePair(_) if hand.combo.rank == rank => FourOfAKind(rank)
    case OnePair(_) => TwoPair(hand.combo.rank, rank)
    case _ => hand.combo
  }

}

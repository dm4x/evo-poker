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
    hands
      .map(flush)
      .map(hand => onePairTwoPairFourOfAKindFullHouse(hand, hand.cards.map(_.rank).distinct.sorted))
      .map(hand => threeOfAKindFullHouse(hand, hand.cards.map(_.rank).distinct.sorted))
      .map(straight)
      .map(createBackHand)
      .map(calcHandStrength)
      .sortWith(compareHands)
  }

  private def createBackHand(hand: Hand): Hand = hand.combo match {
    case FourOfAKind(_, _) => hand.copy(backHand = hand.cards.filterNot(_.rank == hand.combo.rank))
    case ThreeOfAKind(_, _) => hand.copy(backHand = hand.cards.filterNot(_.rank == hand.combo.rank))
    case TwoPair(firstRank, secondRank, _) => hand.copy(backHand = hand.cards.filter(card => card.rank != firstRank && card.rank != secondRank))
    case OnePair(_, _) => hand.copy(backHand = hand.cards.filterNot(_.rank == hand.combo.rank))
    case Flush(_, _) => hand.copy(backHand = hand.cards.filterNot(_.rank == hand.combo.rank))
    case Empty(_, _) => hand.copy(backHand = hand.cards)
    case _ => hand
  }

  private def compareHands(left: Hand, right: Hand): Boolean = {
    if (left.strength < right.strength) true
    else if (left.strength == right.strength) compareBackHands(left.backHand.sortBy(_.rank), right.backHand.sortBy(_.rank))
    else false
  }

  @tailrec
  private def compareBackHands(left: List[Card], right: List[Card]): Boolean = (left, right) match {
    case (left, right) if left.isEmpty || right.isEmpty => true
    case (left, right) if left.head.rank < right.head.rank => true
    case (left, right) if left.head.rank == right.head.rank => compareBackHands(left.tail, right.tail)
    case _ => false
  }

  private def calcHandStrength(hand: Hand): Hand = hand.combo match {
    case Empty(_,_) => hand.copy(strength = hand.cards.map(_.rank).sum)
    case _ => hand.copy(strength = hand.combo.rank * hand.combo.multiplier + hand.cards.map(_.rank).sum)
  }

  private def flush(hand: Hand): Hand = {
    val maxRank = hand.cards.maxBy(_.rank).rank
    val isFlush = hand.cards.map(_.suit).map(isFlushSuit(hand, _)).find(_ == true).getOrElse(false)
    (hand.combo, isFlush) match {
      case (Straight(_, _), true) => hand.copy(combo = StraightFlush(maxRank))
      case (Straight(_, _), false) => hand.copy(combo = Straight(maxRank))
      case (_, true) => hand.copy(combo = Flush(maxRank))
      case _ => hand
    }
  }

  private def isFlushSuit(hand: Hand, suit: Char): Boolean = {
    if (hand.cards.count(_.suit == suit) == 5) true
    else false
  }

  private def straight(hand: Hand): Hand = {
    val maxRank = hand.cards.maxBy(_.rank).rank
    val isStraight = isStraightSuit(0, hand.cards.sortBy(_.rank).map(_.rank), maxRank, result = false)
    (hand.combo, isStraight) match {
      case (Flush(_, _), true) => hand.copy(combo = StraightFlush(maxRank))
      case (Flush(_, _), false) => hand.copy(combo = Flush(maxRank))
      case (_, true) => hand.copy(combo = Straight(maxRank))
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
  private def threeOfAKindFullHouse(hand: Hand, ranks: List[Int]): Hand = {
      if (ranks.isEmpty) hand
      else if (hand.cards.count(_.rank == ranks.head).equals(3)) {
        threeOfAKindFullHouse(hand.copy(combo = fullHouse(hand, ranks)), ranks.tail)
      }
      else threeOfAKindFullHouse(hand, ranks.tail)
  }

  @tailrec
  private def onePairTwoPairFourOfAKindFullHouse(hand: Hand, ranks: List[Int]): Hand = {
    if (ranks.isEmpty) hand
    else if (hand.cards.count(_.rank == ranks.head).equals(2)) {
      onePairTwoPairFourOfAKindFullHouse(hand.copy(combo = fullHouse(hand, ranks)), ranks.tail)
    }
    else onePairTwoPairFourOfAKindFullHouse(hand, ranks.tail)
  }

  private def fullHouse (hand: Hand, ranks: List[Int]): Combo = hand.combo match {
    case ThreeOfAKind(_, _) => FullHouse(hand.combo.rank, ranks.head)
    case OnePair(_, _) if hand.combo.rank == ranks.head => FourOfAKind(ranks.head)
    case OnePair(_, _) if hand.combo.rank != ranks.head => TwoPair(hand.combo.rank, ranks.head)
    case _ => OnePair(ranks.head)
  }

}

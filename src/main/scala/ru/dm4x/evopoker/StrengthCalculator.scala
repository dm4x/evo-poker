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
      .map(hand => onePair(hand, hand.cards.map(_.rank).distinct.sorted))
      .map(hand => threeOfAKind(hand, hand.cards.map(_.rank).distinct.sorted))
      .map(hand => four(hand, hand.cards.map(_.rank).distinct.sorted))
      .map(hand => twoPairs(hand, hand.backHand.map(_.rank).distinct.sorted))
      .map(hand => fullHouse(hand, hand.backHand.map(_.rank).distinct.sorted))
      .map(straightWithBothAces)
      .map(createBackHand)
      .map(calcHandStrength)
      .sortBy(_.strength)
      .sortWith(compareBackHands)
  }

  /**
   * filling a Card.backHand
   * @param hand base hand with combo
   * @return base hand with other cards (not used in combo) in Card.backHand
   */
  private def createBackHand(hand: Hand): Hand = hand.combo match {
    case FourOfAKind(_, _) => hand.copy(backHand = hand.cards.filterNot(_.rank == hand.combo.rank))
    case ThreeOfAKind(_, _) => hand.copy(backHand = hand.cards.filterNot(_.rank == hand.combo.rank))
    case TwoPair(firstRank, secondRank, _) => hand.copy(backHand = hand.cards.filter(card => card.rank != firstRank && card.rank != secondRank))
    case OnePair(_, _) => hand.copy(backHand = hand.cards.filterNot(_.rank == hand.combo.rank))
    case Flush(_, _) => hand.copy(backHand = hand.cards.filterNot(_.rank == hand.combo.rank))
    case Empty(_, _) => hand.copy(backHand = hand.cards)
    case _ => hand
  }

  /**
   * comparing two sorted hands like "Ah Tc 2s" and "Kc 8h 9d"
   * comparing backHands only when combos of hand are equal
   * @param left first hand
   * @param right second hand
   * @return true if left lesser than right
   */
  private def compareBackHands(left: Hand, right: Hand): Boolean = {
    if (left.strength == right.strength) compareBackHands(left.backHand.sortBy(_.rank), right.backHand.sortBy(_.rank))
    else false
  }

  /**
   *
   * @param left a list of cards, sorted by rank
   * @param right  a list of cards, sorted by rank
   * @return true if left lesser than right
   */
  @tailrec
  private def compareBackHands(left: List[Card], right: List[Card]): Boolean = (left, right) match {
    case (left, right) if left.isEmpty || right.isEmpty => true
    case (left, right) if left.head.rank < right.head.rank => true
    case (left, right) if left.head.rank == right.head.rank => compareBackHands(left.tail, right.tail)
    case _ => false
  }

  /**
   * calculate strength of Hand (card's strengths calculated already)
   * @param hand
   * @return Hand with filled strength param
   */
  private def calcHandStrength(hand: Hand): Hand = hand.combo match {
    case Empty(_,_) => hand.copy(strength = hand.cards.map(_.rank).sum)
    case _ => hand.copy(strength = hand.combo.rank * hand.combo.multiplier + hand.cards.map(_.rank).sum)
  }

  /**
   * check for Flush combo
   * @param hand
   * @return Hand with combo filled with Flush
   */
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

  /**
   * that code check a low straight with ace like As2h3d4c5s and than do the same check like other straights
   * @param hand
   * @return Hand with combo filled with straight
   */
  private def straightWithBothAces(hand: Hand): Hand = {
    val ranks = hand.cards.sortBy(_.rank).map(_.rank)
    val ace = hand.cards.filter(_.rank == 14)
    val restCards = hand.cards.filter(_.rank != 14)

    val innerHand = if (ranks.contains(2)
                     && ranks.contains(3)
                     && ranks.contains(4)
                     && ranks.contains(5)
                     && ranks.contains(14)
    ) hand.copy(cards = ace.head.copy(rank = 1) :: restCards)
    else hand
    straight(innerHand)
  }


  /**
   * filler for straight and straight flush
   * @param hand
   * @return Hand with filled combo
   */
  private def straight(hand: Hand): Hand = {
    val maxRank = hand.cards.maxBy(_.rank).rank
    val ranks = hand.cards.sortBy(_.rank).map(_.rank)

    val isStraight = isStraightSuit(ranks.head - 1, ranks, maxRank, 0)

    (hand.combo, isStraight) match {
      case (Flush(_, _), true) => hand.copy(combo = StraightFlush(maxRank))
      case (Flush(_, _), false) => hand.copy(combo = Flush(maxRank))
      case (_, true) => hand.copy(combo = Straight(maxRank))
      case _ => hand
    }
  }

  /**
   * main straight check
   * @param prev previous rank
   * @param ranks ranks of cards in hand, sorted
   * @param highRank of hand to know when need to stop
   * @param counter to accumulate positive check
   * @return
   */
  @tailrec
  private def isStraightSuit(prev: Int, ranks: List[Int], highRank: Int, counter: Int): Boolean = {
    if (ranks.isEmpty && counter == 5) true
    else if (ranks.isEmpty && counter != 5) false
    else if (ranks.head - prev == 1) isStraightSuit(ranks.head, ranks.tail, highRank, counter + 1)
    else isStraightSuit(ranks.head, ranks.tail, highRank, 0)
  }

  /**
   * check for ThreeOfAKind combo
   * @param hand
   * @param ranks of hand to iterate on them
   * @return Hand with combo filled with ThreeOfAKind
   */
  @tailrec
  private def threeOfAKind(hand: Hand, ranks: List[Int]): Hand = {
      if (ranks.isEmpty) hand
      else if (hand.cards.count(_.rank == ranks.head).equals(3)) {
        threeOfAKind(
          hand.copy(
            combo = ThreeOfAKind(ranks.head),
            backHand = hand.cards.filter(_.rank != ranks.head)),
          ranks.tail
        )
      }
      else threeOfAKind(hand, ranks.tail)
  }

  /**
   * check for OnePair combo
   * @param hand
   * @param ranks of hand to iterate on them
   * @return Hand with combo filled with OnePair
   */
  @tailrec
  private def onePair(hand: Hand, ranks: List[Int]): Hand = {
    if (ranks.isEmpty) hand
    else if (hand.cards.count(_.rank == ranks.head).equals(2)) {
      onePair(
        hand.copy(
          combo = OnePair(ranks.head),
          backHand = hand.cards.filter(_.rank != ranks.head)),
        ranks.tail)
    }
    else onePair(hand, ranks.tail)
  }

  /**
   * check for TwoPair combo
   * @param hand
   * @param ranks of hand to iterate on them
   * @return Hand with combo filled with TwoPair
   */
  private def twoPairs(hand: Hand, ranks: List[Int]): Hand = hand.combo match {
    case OnePair(_, _) if hand.combo.rank != ranks.head && hand.backHand.count(_.rank == ranks.head).equals(2) => hand.copy(
      combo = TwoPair(hand.combo.rank, ranks.head),
      backHand = hand.backHand.filter(_.rank != ranks.head))
    case _ => hand
  }

  /**
   * check for FourOfAKind combo
   * @param hand
   * @param ranks of hand to iterate on them
   * @return Hand with combo filled with FourOfAKind
   */
  @tailrec
  private def four(hand: Hand, ranks: List[Int]): Hand = {
    if (ranks.isEmpty) hand
    else if (hand.cards.count(_.rank == ranks.head).equals(4)) {
      four(
        hand.copy(
          combo = FourOfAKind(ranks.head),
          backHand = hand.cards.filter(_.rank != ranks.head)),
        ranks.tail)
    }
    else four(hand, ranks.tail)
  }

  /**
   * check for FullHouse combo
   * @param hand
   * @param ranks of hand to iterate on them
   * @return Hand with combo filled with Fullhouse
   */
  @tailrec
  private def fullHouse(hand: Hand, ranks: List[Int]): Hand = hand.combo match {
    case _ if ranks.isEmpty => hand
    case ThreeOfAKind(_, _) if hand.backHand.count(_.rank == ranks.head).equals(2) => hand.copy(combo = FullHouse(hand.combo.rank, ranks.head))
    case OnePair(_, _) if hand.backHand.count(_.rank == ranks.head).equals(3) => hand.copy(combo = FullHouse(ranks.head, hand.combo.rank))
    case _ => fullHouse(hand, ranks.tail)
  }

}

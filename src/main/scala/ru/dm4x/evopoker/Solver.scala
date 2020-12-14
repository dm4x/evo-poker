package ru.dm4x.evopoker

import ru.dm4x.evopoker.entity.{Card, Hand}

import scala.annotation.tailrec

object Solver {
  // TODO: implement solution logic
  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.toLowerCase.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands => ErrorPrefix + "not created yet"
      case "omaha-holdem" :: board :: hands => ErrorPrefix + "not created yet"
      case "five-card-draw" :: hands => fiveCard(hands).getOrElse(ErrorPrefix + "wrong data in line")
      case x :: _ => ErrorPrefix + "Unrecognized game type"
      case _ => ErrorPrefix + "Invalid input"
    }
  }

  def fiveCard(inHands: List[String]): Option[String] = {
    val cards: List[Hand] = inHands.map(splitHand)
    val calculator = new StrengthCalculator
    cards.map(calculator.evaluate)
    Option("")
  }

  def splitHand(input: String): Hand = {
    Hand(splitHandRec(input, List.empty))
  }

  @tailrec
  def splitHandRec(string: String, hand: List[Card]): List[Card] = {
    if (string.isEmpty) hand
    else {
      val (firstTwo, theRest) = string.splitAt(2)
      val card: Card = Card(firstTwo.charAt(0), firstTwo.charAt(1))
      splitHandRec(theRest, hand :+ card)
    }
  }
}





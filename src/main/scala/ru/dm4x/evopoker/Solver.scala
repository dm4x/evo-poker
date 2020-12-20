package ru.dm4x.evopoker

import ru.dm4x.evopoker.entity.{Card, Hand}

import scala.annotation.tailrec
import scala.util.matching.Regex

object Solver {
  // TODO: implement solution logic
  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands => ErrorPrefix + "not created yet"
      case "omaha-holdem" :: board :: hands => ErrorPrefix + "not created yet"
      case "five-card-draw" :: hands => fiveCard(hands).getOrElse(ErrorPrefix + "wrong data in line")
      case x :: _ => ErrorPrefix + "Unrecognized game type"
      case _ => ErrorPrefix + "Invalid input"
    }
  }

  def fiveCard(inHands: List[String]): Option[String] = {
    val hands: List[Hand] = inHands.map(splitHand)
    val s = new StrengthCalculator().evaluate(hands).map(_.inHand).mkString(" ")
    Option(s)
  }

  private def splitHand(input: String): Hand = {
    Hand(input, splitHandRec(input, List.empty))
  }

  @tailrec
  private def splitHandRec(string: String, hand: List[Card]): List[Card] = {
    if (string.isEmpty) hand
    else {
      val (firstTwo, theRest) = string.splitAt(2)
      val card: Card = Card(cardScore(firstTwo.charAt(0)), firstTwo.charAt(1))
      splitHandRec(theRest, hand :+ card)
    }
  }

  private def cardScore(cardRank: Char): Int = {
    val digits: Regex = "\\d".r
    cardRank match {
      case 'A' => 14
      case 'K' => 13
      case 'Q' => 12
      case 'J' => 11
      case 'T' => 10
      case digits() => cardRank.toInt - 48
      case _ => 0
    }
  }
}





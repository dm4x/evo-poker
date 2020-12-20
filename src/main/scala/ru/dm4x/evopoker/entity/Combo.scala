package ru.dm4x.evopoker.entity

trait Combo {
  val rank: Int
  val multiplier: Long
}

case class Empty(rank: Int = 1, multiplier: Long = 1) extends Combo
case class OnePair(rank: Int, multiplier: Long = 10) extends Combo
case class TwoPair(rank: Int, secondRank: Int, multiplier: Long = 100) extends Combo
case class ThreeOfAKind(rank: Int, multiplier: Long = 1000) extends Combo
case class Straight(rank: Int, multiplier: Long = 10_000) extends Combo
case class Flush(rank: Int, multiplier: Long = 100_000) extends Combo
case class FullHouse(rank: Int, lowRank: Int, multiplier: Long = 1_000_000) extends Combo
case class FourOfAKind(rank: Int, multiplier: Long = 10_000_000) extends Combo
case class StraightFlush(rank: Int, multiplier: Long = 100_000_000) extends Combo


package ru.dm4x.evopoker.entity

trait Combo {
  val rank: Int
}

case class Empty(rank: Int = 1) extends Combo { val multiplier: Long = 1 }
case class OnePair(rank: Int) extends Combo { val multiplier: Long = 10 }
case class TwoPair(rank: Int, secondRank: Int) extends Combo { val multiplier: Long = 100 }
case class ThreeOfAKind(rank: Int) extends Combo { val multiplier: Long = 1000 }
case class Straight(rank: Int) extends Combo { val multiplier: Long = 10_000 }
case class Flush(rank: Int) extends Combo { val multiplier: Long = 100_000 }
case class FullHouse(rank: Int, lowRank: Int) extends Combo { val multiplier: Long = 1_000_000 }
case class FourOfAKind(rank: Int) extends Combo { val multiplier: Long = 10_000_000 }
case class StraightFlush(rank: Int) extends Combo { val multiplier: Long = 100_000_000 }


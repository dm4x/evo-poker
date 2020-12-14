package ru.dm4x.evopoker.entity

trait Combo {
  val multiplier: Long
}

case class OnePair() extends Combo {override val multiplier: Long = 10}
case class TwoPair() extends Combo {override val multiplier: Long = 100}
case class ThreeOfAKind() extends Combo {override val multiplier: Long = 1000}
case class Straight() extends Combo {override val multiplier: Long = 10_000}
case class Flush() extends Combo {override val multiplier: Long = 100_000}
case class FullHouse() extends Combo {override val multiplier: Long = 1_000_000}
case class FourOfAKind() extends Combo {override val multiplier: Long = 10_000_000}
case class StraightFlush() extends Combo {override val multiplier: Long = 100_000_000}


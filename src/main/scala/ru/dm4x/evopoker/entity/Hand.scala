package ru.dm4x.evopoker.entity

case class Hand(inHand: String = "",
                cards: List[Card],
                strength: Long = 0,
                combo: Combo = Empty(),
                backHand: List[Card] = List.empty)

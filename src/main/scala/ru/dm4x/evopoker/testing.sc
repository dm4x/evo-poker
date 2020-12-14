import ru.dm4x.evopoker.entity.{Card, Hand}

val hand = Hand("", cards = List(Card('7','h'), Card('4','s'), Card('4','h'), Card('8','c'), Card('9','h')))
val mapa = hand.cards.groupBy(identity).view.mapValues(_.size)
mapa.keys


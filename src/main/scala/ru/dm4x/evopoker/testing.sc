
var v = 0

private def checkPairThreeAndFour(rankSum: Int):Unit = rankSum match {
  case 4 => v = 4
  case 3 => v = 3
  case 2 => v = 2
  case _ =>
}

checkPairThreeAndFour(4)
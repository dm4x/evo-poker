package ru.dm4x.evopoker

import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = Iterator.continually(Option(StdIn.readLine()))
    .takeWhile(_.nonEmpty)
    .foreach { x =>
      x map Solver.process foreach println
    }
}

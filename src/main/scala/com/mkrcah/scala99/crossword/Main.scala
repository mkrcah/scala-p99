package com.mkrcah.scala99.crossword



object Main extends App {

  val puzzle = Parser.parse("puzzles/p99d.dat")
  val solution = new Solver(puzzle).solve()

  solution match {
    case Some(s) => Printer.prettyPrint(puzzle, s)
    case None => println("No solution found")
  }
}

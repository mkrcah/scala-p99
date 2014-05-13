package com.mkrcah.scala99.crossword


object Printer {

  type Framework = Array[Array[Char]]


  def prettyPrint(puzzle: Puzzle, partialSolution: Map[Site,String]) = {
    val framework = createEmptyFramework(puzzle)
    addEmptySites(puzzle, framework)
    addFilledSites(partialSolution, framework)
    printFramework(framework)
  }


  private def createEmptyFramework(puzzle: Puzzle): Framework = {
    val rows = puzzle.sites.map(_.end.y).reduceLeft(Math.max) + 1
    val cols = puzzle.sites.map(_.end.x).reduceLeft(Math.max) + 1
    Array.tabulate(rows, cols)((x, y) => ' ')
  }


  private def addFilledSites(partialSolution: Map[Site, String], framework: Framework) {
    for ((site, text) <- partialSolution;
         (Position(x, y), index) <- site.positions.zipWithIndex
    ) framework(y)(x) = text(index)
  }


  private def addEmptySites(puzzle: Puzzle, framework: Framework) {
    for (site <- puzzle.sites; Position(x, y) <- site.positions)
      framework(y)(x) = '.'
  }


  private def printFramework(framework: Framework) {
    val rowsAsStr = framework.map(row => row.mkString(" "))
    val frameAsStr = rowsAsStr.mkString("\n")
    println(frameAsStr)
  }

}

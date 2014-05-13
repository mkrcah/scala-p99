package com.mkrcah.scala99.crossword


/** Parses input text file into a model */
object Parser {


  private val SiteRegex = "\\.{2,}".r   // at least 2 consecutive dots


  def parse(filename: String): Puzzle = {
    val lines:Iterator[String] = scala.io.Source.fromFile(filename).getLines()
    val words = lines.takeWhile( _.length >= 1).toSet
    val sites = parseSites(lines.toList)
    new Puzzle(sites, words)
  }


  private def parseSites(lines: List[String]): Set[Site] =
    parseHorizontalSites(lines).toSet ++ parseVerticalSites(lines).toSet



  private def parseHorizontalSites(lines: List[String]) =
    for ((x, y, text) <- getSites(lines))
      yield new Site(Position(x, y), Horizontal, text)


  private def parseVerticalSites(lines: List[String]) =
    for ((x, y, text) <- getSites(transpose(lines)))
      yield new Site(Position(y, x), Vertical, text)


  private def getSites(lines: List[String]) =
    for ((line, counter) <- lines.zipWithIndex;
         pattern <- SiteRegex.findAllIn(line).matchData)
      yield (pattern.start, counter, pattern.matched)


  private def transpose(lines:List[String]):List[String] = {
    def listTranspose[T](xss: List[List[T]]): List[List[T]] =
      xss.filter(!_.isEmpty) match {
        case Nil => Nil
        case ys: List[List[T]] => ys.map{ _.head }::listTranspose(ys.map{ _.tail })
      }
    listTranspose(lines.map(_.toList)).map(_.mkString)
  }

}

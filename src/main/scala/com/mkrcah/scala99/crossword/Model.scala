package com.mkrcah.scala99.crossword


case class Puzzle(sites:Set[Site], words: Set[String]) {

  val intersections: List[Intersection] =
    for (
      siteA <- sites.toList;
      siteB <- sites if siteA != siteB;
      intersection <- siteA.getIntersection(siteB)
    ) yield intersection

}


object PartialSolution {
  def empty = Map.empty[Site, String]
}

trait PartialSolution extends Map[Site, String]
trait Solution extends PartialSolution


case class Intersection(siteA: Site, indexA: Int, siteB: Site, indexB: Int)

sealed trait Direction
case object Vertical extends Direction
case object Horizontal extends Direction

case class Position(x:Int, y:Int)


/** One horizontal or vertical segment to write words into */
class Site(val start: Position, val dir:Direction, val pattern:String) {

  def end = positionAt(pattern.length - 1)

  def positions: IndexedSeq[Position] =
    for (index <- 0 until pattern.length) yield positionAt(index)


  def positionAt(index: Int) = {
    require(index>=0 && index < pattern.length)
    dir match {
      case Vertical => new Position(start.x, start.y + index)
      case Horizontal => new Position(start.x + index, start.y)
    }
  }

  override def toString = {
    val dirAsChar = dir match {
      case Vertical => 'V'
      case Horizontal => 'H'
    }
    f"Site{x=${start.x}%2d, y=${start.y}%2d, dir=$dirAsChar, len=${pattern.length}%2d}"
  }

  def getIntersection(that:Site): Option[Intersection] = {
    for ((posThis, idxThis) <- positions.zipWithIndex;
         (posThat, idxThat) <- that.positions.zipWithIndex if posThis == posThat
    ) return Some(Intersection(this, idxThis, that, idxThat))
    None
  }


}









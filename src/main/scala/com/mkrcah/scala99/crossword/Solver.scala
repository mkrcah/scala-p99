package com.mkrcah.scala99.crossword


class Solver(puzzle:Puzzle, var partialSolution:PartialSolution) {


  def this(puzzle:Puzzle) = this(puzzle, PartialSolution.empty)


  def solve(): Option[Solution] = {
    fillDeterministically()

    if (isComplete)
      return Some(partialSolution with Solution)

    if (!isSolvable)
      return None

    assert(emptySites.size >= 1)
    def pickEmptySite = emptySites.head
    fillNonDeterministically(pickEmptySite)
  }


  private def fillDeterministically() = {
    def hasOneCandidate(site:Site) = getCandidatesFor(site).size == 1
    while (puzzle.sites.exists(hasOneCandidate)) {
      val site = puzzle.sites.find(hasOneCandidate).get
      partialSolution += (site -> getCandidatesFor(site).toList(0))
    }
  }


  private def fillNonDeterministically(site: Site): Option[Solution] = {
    for (w <- getCandidatesFor(site).toList) {
      val solver = new Solver(puzzle, partialSolution ++ Map(site -> w))
      val solution = solver.solve()
      if (solution.isDefined)
        return solution
    }
    None
  }




  private def emptySites = puzzle.sites -- partialSolution.keys

  private def isComplete = partialSolution.size == puzzle.sites.size

  private def isSolvable = !emptySites.exists(getCandidatesFor(_).size == 0)

  private def unusedWords = puzzle.words -- partialSolution.values


  private def getCandidatesFor(site: Site): Set[String] = {
    if (partialSolution.isDefinedAt(site))
      Set.empty[String]
    else {
      val pattern = getPatternFor(site)
      unusedWords.filter(_.matches(pattern))
    }
  }

  private def getPatternFor(site: Site): String = {

    def replaceCharAt(s: String, idx: Int, char: Char) =
      s.substring(0, idx) + char + s.substring(idx+1)

    var pattern = site.pattern
    for (
      Intersection(siteA, idxA, siteB, idxB) <- puzzle.intersections if siteA == site;
      textB <- partialSolution.get(siteB)
    ) pattern = replaceCharAt(pattern, idxA, textB.charAt(idxB))

    pattern
  }


}




package streams

import common._

/**
  * This component implements the solver for the Bloxorz game
  */
trait Solver extends GameDef {

  /**
    * Returns `true` if the block `b` is at the final position
    */
  def done(b: Block): Boolean = b.isStanding && goal.col == b.b1.col && goal.row == b.b1.row

  /**
    * This function takes two arguments: the current block `b` and
    * a list of moves `history` that was required to reach the
    * position of `b`.
    *
    * The `head` element of the `history` list is the latest move
    * that was executed, i.e. the last move that was performed for
    * the block to end up at position `b`.
    *
    * The function returns a stream of pairs: the first element of
    * the each pair is a neighboring block, and the second element
    * is the augmented history of moves required to reach this block.
    *
    * It should only return valid neighbors, i.e. block positions
    * that are inside the terrain.
    */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] =
    b.legalNeighbors.toStream.map(x => (x._1, x._2 :: history))

  /**
    * This function returns the list of neighbors without the block
    * positions that have already been explored. We will use it to
    * make sure that we don't explore circular paths.
    */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = {
    //println(neighbors.toList, explored)
    val tmp = for {
      n <- neighbors
      if !explored.contains(n._1)
    } yield n
    //println(tmp)
    tmp
  }


  /**
    * The function `from` returns the stream of all possible paths
    * that can be followed, starting at the `head` of the `initial`
    * stream.
    *
    * The blocks in the stream `initial` are sorted by ascending path
    * length: the block positions with the shortest paths (length of
    * move list) are at the head of the stream.
    *
    * The parameter `explored` is a set of block positions that have
    * been visited before, on the path to any of the blocks in the
    * stream `initial`. When search reaches a block that has already
    * been explored before, that position should not be included a
    * second time to avoid cycles.
    *
    * The resulting stream should be sorted by ascending path length,
    * i.e. the block positions that can be reached with the fewest
    * amount of moves should appear first in the stream.
    *
    * Note: the solution should not look at or compare the lengths
    * of different paths - the implementation should naturally
    * construct the correctly sorted stream.
    */
  //  def from(initial: Stream[(Block, List[Move])],
  //           explored: Set[Block]): Stream[(Block, List[Move])] = {
  //    println(initial)
  //    val current = initial(explored.size - 1)
  //    val allNeighbours = neighborsWithHistory(current._1, current._2)
  //    val realNeighbours = newNeighborsOnly(allNeighbours, explored)
  //    if (realNeighbours.isEmpty) initial
  //    else
  //      initial #::: realNeighbours #::: from(initial #::: realNeighbours, explored ++ realNeighbours.map(x => x._1))
  //  }
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    val neighbours = getNeighbours(initial, explored)
    initial #::: neighbours
  }

  def getNeighbours(locations: Stream[(Block, List[Move])],
                    explored: Set[Block]): Stream[(Block, List[Move])] = {
    val neighbours = locations.map(x => newNeighborsOnly(neighborsWithHistory(x._1, x._2), explored)).flatten
    neighbours #::: getNeighbours(neighbours, explored ++ neighbours.map(x => x._1))
  }

  //  def from(initial: Stream[(Block, List[Move])],
  //           explored: Set[Block]): Stream[(Block, List[Move])] = {
  //    val current = initial(explored.size - 1)
  //    val allNeighbours = neighborsWithHistory(current._1, current._2)
  //    val realNeighbours = newNeighborsOnly(allNeighbours, explored)
  //    //println("Initial : " + initial)
  //    //println("Initial : " + initial + "| explored : " + explored)
  //    //println("Current : " + current + ", " + "Initial : " + initial)
  //    //println("N: " + realNeighbours)
  //    if (realNeighbours.isEmpty) initial
  //    else {
  //      val result = initial #::: realNeighbours
  //      val newEx = explored ++ realNeighbours.map(x => x._1)
  //      lazy val more = for {
  //        n <- realNeighbours
  //      } yield from(result #::: Stream(n), newEx)
  //      //realNeighbours.foldRight(Stream[(Block, List[Move])])((m, n) => n)
  //      result #::: more.flatten
  //    }
  //  }

  //  def from(initial: Stream[(Block, List[Move])],
  //           explored: Set[Block]): Stream[(Block, List[Move])] = {
  //    val current = initial.head
  //    val allNeighbours = neighborsWithHistory(current._1, current._2)
  //    val realNeighbours = newNeighborsOnly(allNeighbours, explored).toList
  //    val newInitial = (initial.toList ::: realNeighbours).toStream
  //    println("New Initial => " + newInitial)
  //    //println("Current => " + current + ", " + explored)
  //    //println("All Neighbours => " + allNeighbours.toList)
  //    println("Initial => " + initial)
  //    println("Real Neighbours => " + realNeighbours)
  //    if (realNeighbours.isEmpty) Stream.empty
  //    else {
  //      //      val m = for {
  //      //        neighbour <- realNeighbours.take
  //      //      } yield {
  //      //        println("Neighbour = " + neighbour)
  //      //        //from(neighbour #:: initial, explored + neighbour._1)
  //      //        neighbour
  //      //      }
  //      //      m.take(3).foreach(x => println("m = " + x))
  //      val more = realNeighbours.toList.map(x => {
  //        println("neighbour = " + x)
  //        x
  //      })
  //      //val result = initial #::: realNeighbours
  //      //initial #::: realNeighbours #::: more.flatten
  //      //println("Real Neighbours => " + realNeighbours)
  //      //println("RESULT IS " + result)
  //      //      initial #::: realNeighbours
  ////      initial #::: realNeighbours
  //      newInitial
  //    }
  //  }

  //
  //  def from(initial: Stream[(Block, List[Move])],
  //           explored: Set[Block]): Stream[(Block, List[Move])] = {
  //    val first = initial.head
  //    val allNeighbours = neighborsWithHistory(first._1, first._2)
  //    val realNeighbours = newNeighborsOnly(allNeighbours, explored)
  //    val more = for {
  //      neighbour <- realNeighbours
  //    } yield from(Stream(neighbour), explored + neighbour._1)
  //    first #:: realNeighbours #::: more.flatten
  //  }

  /**
    * The stream of all paths that begin at the starting block.
    */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream((this.startBlock, List())), Set(this.startBlock))

  /**
    * Returns a stream of all possible pairs of the goal block along
    * with the history how it was reached.
    */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = pathsFromStart.filter(p => done(p._1))

  /**
    * The (or one of the) shortest sequence(s) of moves to reach the
    * goal. If the goal cannot be reached, the empty list is returned.
    *
    * Note: the `head` element of the returned list should represent
    * the first move that the player should perform from the starting
    * position.
    */
  lazy val solution: List[Move] = {
    //println(pathsToGoal)
    pathsToGoal(0)._2.reverse
  }
}

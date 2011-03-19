package com.stephentu

trait GameTree {
  import Board._
  val board: Board
  val gamePlayer: Player

  def successors: Seq[GameTree] = board.successors(gamePlayer).map(newChild)
  def value: (Int, GameTree) = value(None) 

  /** returns the value of THIS node + the successor game tree
   * which represents the optimal next move (if one exists) */
  def value(maxDepth: Option[Int]): (Int, GameTree) = {
    maxDepth.foreach(d => require(d >= 0))
    value(Integer.MIN_VALUE, Integer.MAX_VALUE, maxDepth)
  }

  private[stephentu] def value(alpha: Int, beta: Int, depthRemaining: Option[Int]) = board.winner match {
    case Some(Winner(player)) => (valueIfWinner(player), null)
    case Some(Tie) => (0, null)
    case None if depthRemaining.map(_ == 0).getOrElse(false) => 
      println("max depth reached: truncating")
      (0, null)
    case None => reduceValues(successors, alpha, beta, depthRemaining)
  }

  /**
   * Value from the point of view of the Maximizer
   */
  @inline private def valueIfWinner(player: Player) = player match {
    case Minimizer => -1
    case Maximizer => 1
  }

  protected def newChild(successor: Board): GameTree
  protected def reduceValues(values: Seq[GameTree], alpha: Int, beta: Int, depthRemaining: Option[Int]): (Int, GameTree)
}

class MaxGameTree(val board: Board) extends GameTree {
  val gamePlayer = Maximizer
  protected def newChild(successor: Board) = 
    new MinGameTree(successor)
  protected def reduceValues(values: Seq[GameTree], alpha: Int, beta: Int, depthRemaining: Option[Int]) = {
    import scala.math.max
    val (a, _, n) = values.foldLeft((alpha, false, null:GameTree)) {
      case (acc @ (alphaSoFar, break, node), child) =>
        if (break) {
          assert(child ne null)
          acc
        } else {
          val cvalue = child.value(alphaSoFar, beta, depthRemaining.map(_-1))._1
          if (cvalue > alphaSoFar)
            (cvalue, beta <= cvalue, child)
          else
            acc
        }
    }
    (a, n)
  }
}

class MinGameTree(val board: Board) extends GameTree {
  val gamePlayer = Minimizer
  protected def newChild(successor: Board) = 
    new MaxGameTree(successor)
  protected def reduceValues(values: Seq[GameTree], alpha: Int, beta: Int, depthRemaining: Option[Int]) = {
    import scala.math.min
    val (b, _, n) = values.foldLeft((beta, false, null:GameTree)) {
      case (acc @ (betaSoFar, break, node), child) =>
        if (break) {
          assert(child ne null) 
          acc
        } else {
          val cvalue = child.value(alpha, betaSoFar, depthRemaining.map(_-1))._1
          if (cvalue < betaSoFar)
            (cvalue, cvalue <= alpha, child)
          else
            acc
        }
    }
    (b, n)
  }
}

trait ArgMinMaxMixin {

  protected def argp(values: Seq[GameTree])(cmp: (Int, Int) => Boolean): (Int, GameTree) = {
    if (values.isEmpty)
      throw new Exception ("argp on empty array")
    var first = true
    values.foldLeft((values.head.value._1, values.head)) {
      case (acc @ (winnerSoFar, argWinnerSoFar), candidate) =>
        if (first) {
          first = false
          acc
        } else {
          val candidateValue = candidate.value._1
          if (cmp(winnerSoFar, candidateValue)) (candidateValue, candidate)
          else acc
        } 
    }
  }

  protected def argmax(values: Seq[GameTree]) = argp(values) { case (best, cur) => cur > best }

  protected def argmin(values: Seq[GameTree]) = argp(values) { case (best, cur) => cur < best }
}

class NoABMaxGameTree(val board: Board) extends GameTree with ArgMinMaxMixin {
  val gamePlayer = Maximizer
  protected def newChild(successor: Board) = 
    new NoABMinGameTree(successor)
  protected def reduceValues(values: Seq[GameTree], alpha: Int, beta: Int, depthRemaining: Option[Int]) = argmax(values)
}

class NoABMinGameTree(val board: Board) extends GameTree with ArgMinMaxMixin {
  val gamePlayer = Minimizer
  protected def newChild(successor: Board) = 
    new NoABMaxGameTree(successor)
  protected def reduceValues(values: Seq[GameTree], alpha: Int, beta: Int, depthRemaining: Option[Int]) = argmin(values)
}

// vim: set ts=2 sw=2 et:

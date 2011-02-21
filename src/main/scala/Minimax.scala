package com.stephentu

sealed trait GameTree {
  import Board._
  val board: Board
  def gamePlayer: Player
  def value: Int = value(Integer.MIN_VALUE, Integer.MAX_VALUE)

  private[stephentu] def value(alpha: Int, beta: Int): Int = board.winner match {
    case Some(Winner(player)) => valueIfWinner(player)
    case Some(Tie) => 0
    case None =>
      reduceValues(board.successors(gamePlayer).map(newChild), alpha, beta)
  }

  /**
   * Value from the point of view of the Maximizer
   */
  private def valueIfWinner(player: Player) = player match {
    case Minimizer => -1
    case Maximizer => 1
  }

  protected def newChild(successor: Board): GameTree
  protected def reduceValues(values: Seq[GameTree], alpha: Int, beta: Int): Int
}

class MaxGameTree(val board: Board) extends GameTree {
  def gamePlayer = Maximizer
  protected def newChild(successor: Board) = 
    new MinGameTree(successor)
  protected def reduceValues(values: Seq[GameTree], alpha: Int, beta: Int) = {
    import scala.math.max
    values.foldLeft((alpha, false)) {
      case (acc @ (alphaSoFar, break), child) =>
        if (break) acc
        else {
          val alphaNew = max(alphaSoFar, child.value(alphaSoFar, beta)) 
          (alphaNew, beta <= alphaNew)
        }
    }._1
  }
}

class MinGameTree(val board: Board) extends GameTree {
  def gamePlayer = Minimizer
  protected def newChild(successor: Board) = 
    new MaxGameTree(successor)
  protected def reduceValues(values: Seq[GameTree], alpha: Int, beta: Int) = {
    import scala.math.min
    values.foldLeft((beta, false)) {
      case (acc @ (betaSoFar, break), child) =>
        if (break) acc
        else {
          val betaNew = min(betaSoFar, child.value(alpha, betaSoFar)) 
          (betaNew, betaNew <= alpha)
        }
    }._1
  }
}

// vim: set ts=2 sw=2 et:

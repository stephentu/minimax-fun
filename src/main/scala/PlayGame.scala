package com.stephentu

import util.Random

object PlayGame {

  import Board.{ Tie, Winner }

  private val searchDepth = Integer.MAX_VALUE 

  sealed trait GameResult {
    val board: Board
  }
  case class UserWins(board: Board) extends GameResult
  case class ComputerWins(board: Board) extends GameResult
  case class GameTie(board: Board) extends GameResult

  def main(args: Array[String]): Unit = {

    val board = Board.initFourBVTTT()
    val userGoesFirst = Random.nextBoolean()

    val ValidMove = "\\s*(\\d),\\s*(\\d)\\s*".r

    def playUser(board: Board, player: Player, searchDepth: Int): GameResult = {
      // display board to stdout
      println("User (%s)'s turn".format(player.strRepr))
      println(board)

      // ask user where he wants to move next
      @scala.annotation.tailrec def nextMove: (Int, Int) = {
        print("Where do you want to move next (x, y coords please): ")
        Console.readLine() match {
          case ValidMove(x, y) if !board.isOccupied(x.toInt, y.toInt) => 
            println("\nMoving %d, %d".format(x.toInt, y.toInt))
            (x.toInt, y.toInt)
          case ValidMove(x, y) =>
            println("\nSorry, position is occupied already")
            nextMove
          case _ =>
            println("\nPlease enter valid x,y pair")
            nextMove
        }
      }

      val (x, y) = nextMove
      val successor = board.makeMove(x, y, player)
      successor.winner match {
        case Some(Winner(p)) if p == player => UserWins(successor)
        case Some(Winner(p)) => ComputerWins(successor)
        case Some(Tie) => GameTie(successor)
        case None => playComputer(successor, player.next, searchDepth)
      }
    }

    def playComputer(board: Board, player: Player, searchDepth: Int): GameResult = {
      def valueToStringIfMax(v: Int) = v match {
        case -1 => "Computer is going to lose"
        case  0 => "Computer is going to tie"
        case  1 => "Computer is going to win"
        case  _ => error("invalid value")
      }
      def valueToStringIfMin(v: Int) = v match {
        case -1 => "Computer is going to win"
        case  0 => "Computer is going to tie"
        case  1 => "Computer is going to lose"
        case  _ => error("invalid value")
      }
      def valueToString(v: Int) = player match {
        case Maximizer => valueToStringIfMax(v)
        case Minimizer => valueToStringIfMin(v)
      }
      println("Computer (%s)'s turn".format(player.strRepr))
      val startTime = System.currentTimeMillis
      val (value, node) = 
        (player match {
          case Maximizer => new MaxGameTree(board)
          case Minimizer => new MinGameTree(board)
        }).value(Some(searchDepth))
      val endTime = System.currentTimeMillis
      assert(node ne null)
      println("Computation took %d ms".format(endTime - startTime))
      println(valueToString(value))
      val successor = node.board
      successor.winner match {
        case Some(Winner(p)) if p == player => 
          println(successor) 
          ComputerWins(successor)
        case Some(Winner(p)) => 
          println(successor) 
          UserWins(successor)
        case Some(Tie) => GameTie(successor)
        case None => playUser(successor, player.next, searchDepth)
      }
    }

    {
      if (userGoesFirst) playUser(board, Maximizer, searchDepth) 
      else playComputer(board, Maximizer, searchDepth)
    } match {
      case UserWins(b) => 
        println("Good job, you win!")
        println(b)
      case ComputerWins(b) => 
        println("Sorry, computer wins")
        println(b)
      case GameTie(b) => 
        println("Nobody wins")
        println(b)
    }
  }
}
// vim: set ts=2 sw=2 et:

package com.stephentu

sealed trait Player {
  def strRepr: String
}
case object Minimizer extends Player {
  def strRepr = "o"
}
case object Maximizer extends Player {
  def strRepr = "x"
}

object Board {

  sealed trait BoardResult
  case object Tie extends BoardResult
  case class Winner(player: Player) extends BoardResult

  def initFourTTT(): Board =
    new FourTicTacToeBoard(Vector.fill(4)(Vector.fill(4)(None)))
  def initRegTTT(): Board = 
    new RegTicTacToeBoard(Vector.fill(3)(Vector.fill(3)(None)))
}

trait Board {
  import Board._

  def winner: Option[BoardResult]
  def successors(player: Player): Seq[Board]
}

class FourTicTacToeBoard(val config: Vector[Vector[Option[Player]]]) extends Board {

  import Board._

  private def isBoardFull = 
    config.map(_.filter(_.isEmpty).size).sum == 0

  /**
   * Returns the winner of this board config, if one exists 
   */
  def winner: Option[BoardResult] = {
    def colSlice(col: Int): Seq[Option[Player]] =
      (0 until 4).map(row => config(row)(col))

    def sliceWinner(slice: Seq[Option[Player]]): Option[Player] = {
      val (minP, maxP) = slice.foldLeft((0, 0)) {
        case ((minPAcc, maxPAcc), Some(Minimizer)) =>
          (minPAcc + 1, maxPAcc)
        case ((minPAcc, maxPAcc), Some(Maximizer)) =>
          (minPAcc, maxPAcc + 1)
        case (acc, None) => acc
      }
      if (minP == 3 && maxP == 1)
        Some(Minimizer)
      else if (maxP == 3 && minP == 1)
        Some(Maximizer)
      else 
        None
    }

    def sliceFn(acc: Option[Player], slice: Seq[Option[Player]]): Option[Player] = 
      acc.orElse(sliceWinner(slice))

    def leftDiag = 
      Vector(config(0)(0),
             config(1)(1),
             config(2)(2),
             config(3)(3))

    def rightDiag =
      Vector(config(0)(3),
             config(1)(2),
             config(2)(1),
             config(3)(0))

    config.foldLeft(None:Option[Player])(sliceFn) // check if rows have winner
      .orElse((0 until 4).map(colSlice).foldLeft(None:Option[Player])(sliceFn) // check if columns have winner
        .orElse(List(leftDiag, rightDiag).foldLeft(None:Option[Player])(sliceFn))) // check if diag have winner
          .map(Winner(_))
            .orElse(if (isBoardFull) Some(Tie) else None)
  }

  /**
   * Returns the next possible moves, given a player
   */
  def successors(player: Player): Seq[Board] =
    (0 until 4).flatMap(i => (0 until 4).flatMap(j => config(i)(j) match {
        case None => List(new FourTicTacToeBoard(nextConfig(player, i, j)))
        case _    => Nil }))

  override def toString = {
    // ---------
    // |x|o|x| |
    // |x|o|x| |
    // |x|o|x| |
    // |x|o|x| |
    // ---------

    "---------\n" +
    config.map(_.map(_.map(_.strRepr).getOrElse(" ")).mkString("|", "|", "|")).mkString("\n") + "\n" +
    "---------\n"
  }

  private def nextConfig(player: Player, i: Int, j: Int): Vector[Vector[Option[Player]]] = 
    config.updated(i, config(i).updated(j, Some(player)))
}

class RegTicTacToeBoard(val config: Vector[Vector[Option[Player]]]) extends Board {

  import Board._

  private def isBoardFull = 
    config.map(_.filter(_.isEmpty).size).sum == 0

  /**
   * Returns the winner of this board config, if one exists 
   */
  def winner: Option[BoardResult] = {
    def colSlice(col: Int): Seq[Option[Player]] =
      (0 until 3).map(row => config(row)(col))

    def sliceWinner(slice: Seq[Option[Player]]): Option[Player] = {
      val (minP, maxP) = slice.foldLeft((0, 0)) {
        case ((minPAcc, maxPAcc), Some(Minimizer)) =>
          (minPAcc + 1, maxPAcc)
        case ((minPAcc, maxPAcc), Some(Maximizer)) =>
          (minPAcc, maxPAcc + 1)
        case (acc, None) => acc
      }
      if (minP == 3)
        Some(Minimizer)
      else if (maxP == 3)
        Some(Maximizer)
      else 
        None
    }

    def sliceFn(acc: Option[Player], slice: Seq[Option[Player]]): Option[Player] = 
      acc.orElse(sliceWinner(slice))

    def leftDiag = 
      Vector(config(0)(0),
             config(1)(1),
             config(2)(2))

    def rightDiag =
      Vector(config(0)(2),
             config(1)(1),
             config(2)(0))

    config.foldLeft(None:Option[Player])(sliceFn) // check if rows have winner
      .orElse((0 until 3).map(colSlice).foldLeft(None:Option[Player])(sliceFn) // check if columns have winner
        .orElse(List(leftDiag, rightDiag).foldLeft(None:Option[Player])(sliceFn))) // check if diag have winner
          .map(Winner(_))
            .orElse(if (isBoardFull) Some(Tie) else None)
  }

  /**
   * Returns the next possible moves, given a player
   */
  def successors(player: Player): Seq[Board] =
    (0 until 3).flatMap(i => (0 until 3).flatMap(j => config(i)(j) match {
        case None => List(new RegTicTacToeBoard(nextConfig(player, i, j)))
        case _    => Nil }))

  override def toString = {
    // -------
    // |x|o|x|
    // |x|o|x|
    // |x|o|x|
    // -------

    "-------\n" +
    config.map(_.map(_.map(_.strRepr).getOrElse(" ")).mkString("|", "|", "|")).mkString("\n") + "\n" +
    "-------\n"
  }

  private def nextConfig(player: Player, i: Int, j: Int): Vector[Vector[Option[Player]]] = 
    config.updated(i, config(i).updated(j, Some(player)))
}

// vim: set ts=2 sw=2 et:

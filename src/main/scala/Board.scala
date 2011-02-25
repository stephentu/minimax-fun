package com.stephentu

import collection.mutable.ArrayBuffer

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

  def initFourBVTTT(): Board = 
    new FourTicTacToeBoardBitVec(0, 0)
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

class FourTicTacToeBoardBitVec(val minVec: Int, val maxVec: Int) extends Board {
  // lower 16 bits of minVec = [xxxx|xxxx|xxxx|xxxx] (row0|row1|row2|row3)
  // lower 16 bits of maxVec = [xxxx|xxxx|xxxx|xxxx] (row0|row1|row2|row3)

  import Board._

  private val row0_m = 0xf000
  private val row1_m = 0x0f00
  private val row2_m = 0x00f0
  private val row3_m = 0x000f

  private val col0_m = 0x8888
  private val col1_m = 0x4444
  private val col2_m = 0x2222
  private val col3_m = 0x1111

  // takes a row based repr and converts it to a column based, ie
  // [row0|row1|row2|row3] -> [col0|col1|col2|col3]
  @inline private def rot(repr: Int): Int = {
    val col0 = (repr & 0x8000) | ((repr & 0x0800) << 3) | ((repr & 0x0080) << 6) | ((repr & 0x0008) << 9) /* col0 */
    val col1 = ((repr & 0x4000) >> 3) | (repr & 0x0400) | ((repr & 0x0040) << 3) | ((repr & 0x0004) << 6) /* col1 */
    val col2 = ((repr & 0x2000) >> 6) | ((repr & 0x0200) >> 3) | (repr & 0x0020) | ((repr & 0x0002) << 3) /* col2 */
    val col3 = ((repr & 0x1000) >> 9) | ((repr & 0x0100) >> 6) | ((repr & 0x0010) >> 3) | (repr & 0x0001) /* col3 */
    col0 | col1 | col2 | col3
  }

  @inline private def diagLeft(repr: Int): Int = {
    // [x---|-x--|--x-|---x]
    ((repr & 0x8000) >> 12) | ((repr & 0x0400) >> 8) | ((repr & 0x0020) >> 4) | (repr & 0x0001)
  }

  @inline private def diagRight(repr: Int): Int = {
    // [---x|--x-|-x--|x---]
    ((repr & 0x1000) >> 12) | ((repr & 0x0200) >> 8) | ((repr & 0x0040) >> 4) | (repr & 0x0008)
  }

  def winner: Option[BoardResult] = {

    @inline def sliceWinner(slice_min: Int, slice_max: Int): Option[Player] = {
      // slice min winner?
      // min = [1110], max = [0001]
      // min = [1101], max = [0010]
      // min = [1011], max = [0100]
      // min = [0111], max = [1000]
      if (slice_min == 0xe && slice_max == 0x1 ||
          slice_min == 0xd && slice_max == 0x2 ||
          slice_min == 0xb && slice_max == 0x4 ||
          slice_min == 0x7 && slice_max == 0x8) 
        return Some(Minimizer)

      // slice max winner?
      // max = [1110], min = [0001]
      // max = [1101], min = [0010]
      // max = [1011], min = [0100]
      // max = [0111], min = [1000]
      if (slice_max == 0xe && slice_min == 0x1 ||
          slice_max == 0xd && slice_min == 0x2 ||
          slice_max == 0xb && slice_min == 0x4 ||
          slice_max == 0x7 && slice_min == 0x8) 
        return Some(Maximizer)

      return None
    }

    val row0_min = (minVec & row0_m) >> 12
    val row0_max = (maxVec & row0_m) >> 12

    sliceWinner(row0_min, row0_max).orElse({
      val row1_min = (minVec & row1_m) >> 8
      val row1_max = (maxVec & row1_m) >> 8
      sliceWinner(row1_min, row1_max).orElse({
        val row2_min = (minVec & row2_m) >> 4
        val row2_max = (maxVec & row2_m) >> 4
        sliceWinner(row2_min, row2_max).orElse({
          val row3_min = (minVec & row3_m)
          val row3_max = (maxVec & row3_m)
          sliceWinner(row3_min, row3_max).orElse({
            val minVecRot = rot(minVec)
            val maxVecRot = rot(maxVec)
            val col0_min = (minVecRot & row0_m) >> 12
            val col0_max = (maxVecRot & row0_m) >> 12
            sliceWinner(col0_min, col0_max).orElse({
              val col1_min = (minVecRot & row1_m) >> 8
              val col1_max = (maxVecRot & row1_m) >> 8
              sliceWinner(col1_min, col1_max).orElse({
                val col2_min = (minVecRot & row2_m) >> 4
                val col2_max = (maxVecRot & row2_m) >> 4
                sliceWinner(col2_min, col2_max).orElse({
                  val col3_min = (minVecRot & row3_m)
                  val col3_max = (maxVecRot & row3_m)
                  sliceWinner(col3_min, col3_max).orElse({
                    val diagLeft_min = diagLeft(minVec)
                    val diagLeft_max = diagLeft(maxVec)
                    sliceWinner(diagLeft_min, diagLeft_max).orElse({
                      val diagRight_min = diagRight(minVec)
                      val diagRight_max = diagRight(maxVec)
                      sliceWinner(diagRight_min, diagRight_max)
                    })
                  })
                })
              })
            })
          })
        })
      })
    }).map(Winner(_)).orElse(if ((minVec | maxVec) == 0xffff) Some(Tie) else None)
  }

  def successors(player: Player) = {
    val remainingVec = (minVec | maxVec)
    var i = 1
    val buf = new ArrayBuffer[Board]
    while (i < 0x10000) {
      if ((remainingVec & i) == 0) // available move
        player match {
          case Minimizer =>
            buf += new FourTicTacToeBoardBitVec(minVec | i, maxVec)
          case Maximizer =>
            buf += new FourTicTacToeBoardBitVec(minVec, maxVec | i)
        }
      i <<= 1
    }
    buf.toSeq
  }

  override def toString = {
    @inline def sliceToVec(slice: Int, player: Player): Vector[Option[Player]] = {
      Vector(if ((slice & 0x8) != 0) Some(player) else None,
             if ((slice & 0x4) != 0) Some(player) else None,
             if ((slice & 0x2) != 0) Some(player) else None,
             if ((slice & 0x1) != 0) Some(player) else None)
    }

    @inline def mergeVecs(left: Seq[Option[Player]], right: Seq[Option[Player]]) = {
      left.zip(right).map { 
        case (Some(p), None) => Some(p)
        case (None, Some(p)) => Some(p)
        case (None, None) => None
        case _ => error("should never reach")
      }
    }

    val row0_minvec = sliceToVec((minVec & row0_m) >> 12, Minimizer)
    val row0_maxvec = sliceToVec((maxVec & row0_m) >> 12, Maximizer)

    val row1_minvec = sliceToVec((minVec & row1_m) >> 8, Minimizer)
    val row1_maxvec = sliceToVec((maxVec & row1_m) >> 8, Maximizer)

    val row2_minvec = sliceToVec((minVec & row2_m) >> 4, Minimizer)
    val row2_maxvec = sliceToVec((maxVec & row2_m) >> 4, Maximizer)

    val row3_minvec = sliceToVec(minVec & row3_m, Minimizer)
    val row3_maxvec = sliceToVec(maxVec & row3_m, Maximizer)

    val config = Vector(mergeVecs(row0_minvec, row0_maxvec),
           mergeVecs(row1_minvec, row1_maxvec),
           mergeVecs(row2_minvec, row2_maxvec),
           mergeVecs(row3_minvec, row3_maxvec))
    "---------\n" +
    config.map(_.map(_.map(_.strRepr).getOrElse(" ")).mkString("|", "|", "|")).mkString("\n") + "\n" +
    "---------\n"
  }

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

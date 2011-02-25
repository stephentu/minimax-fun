package com.stephentu

object Test {
  def main(args: Array[String]): Unit = {
    // you start first
    val tree = new MaxGameTree(Board.initFourBVTTT())

    val startTime = System.currentTimeMillis
    println("root tree value: " + tree.value)
    val endTime = System.currentTimeMillis
    println("computation took %d ms".format(endTime - startTime))

  }
}

// vim: set ts=2 sw=2 et:

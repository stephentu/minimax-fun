package com.stephentu

object Test {
  def main(args: Array[String]): Unit = {
    // you start first
    val tree = new MaxGameTree(Board.initFourTTT())
    println("root tree value: " + tree.value)
  }
}

// vim: set ts=2 sw=2 et:

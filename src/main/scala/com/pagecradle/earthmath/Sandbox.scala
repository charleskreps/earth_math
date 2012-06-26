package com.pagecradle.earthmath

/**
 * @author ckreps
 */
object Sandbox {

  def main(args: Array[String]) {

    val result = CSquares.CSquare(-42.80, 147.30, 1)
    println(result)
    println(result.toCSquareString)
    println(result.latBoundary)
    println(result.lngBoundary)
    println(result.center)
  }
}

package com.pagecradle.earthmath

/**
 * @author ckreps
 */
object BoolLogic {

  def table[T](p: Boolean, q: Boolean, tt: T, tf: T, ft: T, ff: T): T = {
    if (p) {
      if (q) tt else tf
    } else {
      if (q) ft else ff
    }
  }
}

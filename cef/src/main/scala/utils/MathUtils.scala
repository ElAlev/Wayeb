package utils

import scala.math.log10

object MathUtils {
  /**
    * Estimates logarithm of a number with a given base.
    *
    * @param x The number.
    * @param base The base.
    * @return The logarithm.
    */
  def logbase(
               x: Double,
               base: Double
             ): Double = log10(x) / log10(base)
}

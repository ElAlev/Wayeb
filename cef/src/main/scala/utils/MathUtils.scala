package utils

import breeze.stats.distributions.Uniform

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

  /**
    * Uninormly samples a double from an interval [0,max].
    *
    * @param max The upper limit of the interval.
    * @return the sample
    */
  def sampleUniform(max: Int): Double = {
    val uni = new Uniform(0, max)
    val s = uni.sample()
    s
  }

  /**
    * Uninormly samples an integer from an interval [0,max].
    *
    * @param max The upper limit of the interval.
    * @return the sample
    */
  def sampleIntUniform(max: Int): Int = sampleUniform(max).toInt

  /**
    * Performs simple linear regression with least squares.
    *
    * @param x The vector of x values.
    * @param y The vector of y values.
    * @return The slope and intercept of the resulting line.
    */
  def slopeInterceptLinearLeastSquares(
                                        x: List[Double],
                                        y: List[Double]
                                      ): (Double,Double) = {
    require(x.nonEmpty & x.size == y.size)
    val n = x.size
    val x2 = x.map(e => e * e)
    val xy = x.zip(y).map(e => (e._1 * e._2))
    val sumx = x.sum
    val sumy = y.sum
    val sumx2 = x2.sum
    val sumxy = xy.sum
    val slope = ((n*sumxy) - (sumx * sumy)) / ((n*sumx2) - (sumx * sumx))
    val intercept = (sumy - (slope * sumx)) / n
    (slope, intercept)
  }
}

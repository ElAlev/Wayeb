package Specs.vmm

import breeze.stats.distributions.Uniform
import com.typesafe.scalalogging.LazyLogging
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import model.vmm.Symbol
import model.vmm.pst.psa.PSAUtils
import ui.ConfigUtils

@RunWith(classOf[JUnitRunner])
class PSAGenerator extends FlatSpec with LazyLogging {
  "A randomly generated PSA " should " be valid " in {
    for (m <- 1 to ConfigUtils.maxOrder) testOrder(m)
  }

  def testOrder(maxOrder: Int): Unit = {
    val numberOfPSAs = 10
    logger.debug("Testing generation of PSA @ order " + maxOrder)
    for (i <- 1 to numberOfPSAs) {
      val uniExpansion = new Uniform(0, 1)
      val expansionProb = uniExpansion.sample()
      val symbols = (1 to ConfigUtils.symbolsNo).toSet[Int].map(i => Symbol(i))
      logger.debug("Generating PSA with maxOrder/expansionProb/symbols\n" + maxOrder + "/" + expansionProb + "/" + symbols)
      val psa = PSAUtils.createPSA(symbols, maxOrder, expansionProb)
      logger.debug("PSA generated\n " + psa.toString)
      logger.debug("PSA size/maxOrder: " + psa.size + "/" + psa.maxOrder)
      assert(PSAUtils.isSuffixFree(psa))
      assert(PSAUtils.isSuffixFull(psa))
    }
  }
}

package Specs.vmm

import breeze.stats.distributions.Uniform
import com.typesafe.scalalogging.LazyLogging
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import model.vmm.Symbol
import model.vmm.pst.psa.PSAUtils

@RunWith(classOf[JUnitRunner])
class CompleteProperSuffixSet extends FlatSpec with LazyLogging {
  "A suffix set " should " be complete and proper " in {
    for (m <- 1 to 3) testOrder(m)
  }

  def testOrder(maxOrder: Int): Unit = {
    logger.debug("Testing creation of a complete and proper suffix set @ order " + maxOrder)
    val uniExpansion = new Uniform(0, 1)
    val expansionProb = uniExpansion.sample()
    val symbols = (1 to 5).toSet[Int].map(i => Symbol(i))
    logger.debug("Creating suffix sets with symbols/maxOrder/expansionProb" + symbols + "/" + maxOrder + "/" + expansionProb)
    val suffixes = PSAUtils.createCompleteProperFullSuffixSet(symbols, maxOrder, expansionProb)
    logger.debug("Created complete, proper, full set " + suffixes)
    assert(PSAUtils.isCompleteProperFull(suffixes, symbols, maxOrder))
  }
}

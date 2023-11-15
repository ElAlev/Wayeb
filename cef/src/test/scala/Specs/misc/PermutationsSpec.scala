package Specs.misc

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.BooleanPermutator
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PermutationsSpec extends FlatSpec with LazyLogging {
  "Permutations constructed in different ways " should " still be the same" in {
    val elements = Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val maxLength = 5
    var permsTrad = Map[Int, Set[List[Int]]]()

    var t1 = System.nanoTime()
    for (i <- 1 to maxLength) {
      val perms = utils.SetUtils.permutations(elements, i)
      permsTrad = permsTrad + (i -> perms)
    }
    var t2 = System.nanoTime()
    logger.debug("perms time: " + (t2 - t1) / 1000000.0)

    t1 = System.nanoTime()
    val permsAlt = utils.SetUtils.permutationsAlt(elements, maxLength)
    t2 = System.nanoTime()
    logger.debug("perms alt time: " + (t2 - t1) / 1000000.0)

    checkPerms(permsTrad, permsAlt)

    val truthValues = Set(true, false)
    val permsAltBool = utils.SetUtils.permutationsAlt(truthValues, maxLength)
    val boolPerm = new BooleanPermutator(maxLength)
    val permsPermutatorBool = boolPerm.getPermutationsUpTo(maxLength)
    logger.debug("perms alt bool:" + permsAltBool)
    logger.debug("perms perm bool:" + permsPermutatorBool)
    checkPerms(permsAltBool, permsPermutatorBool)
  }

  private def checkPerms[T](
                             perms1: Map[Int, Set[List[T]]],
                             perms2: Map[Int, Set[List[T]]]
                           ): Unit = {
    assert(perms1.keySet == perms2.keySet)
    val it = perms1.keySet.iterator
    while (it.hasNext) {
      val i = it.next()
      assert(perms1(i) == perms2(i))
    }
  }

}

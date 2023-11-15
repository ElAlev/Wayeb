package workflow.provider

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.{Predicate, Sentence}
import fsm.symbolic.sra.dsra.{DSRAStreaming, DSRAUtils}
import fsm.symbolic.sre.{SREFormula, SREUtils}
import fsm.{DSRAInterface, SPSTmInterface}
import model.vmm.mapper.SymbolExtractorFromDSRA
import stream.source.StreamSource
import workflow.condition.{Condition, FileExistsCondition}
import workflow.provider.source.dsra.DSRASourceDirect
import workflow.provider.source.pst.{PSTSource, PSTSourceDirect, PSTSourceLearnerFromDSRA}
import workflow.provider.source.spstm._

import java.io.{FileInputStream, ObjectInputStream}

object SPSTmProvider {
  /**
   * Constructor for SPSTm provider.
   *
   * @param spstmSource The source for the SPSTm.
   * @return            A SPSTm provider.
   */
  def apply(spstmSource: SPSTmSource): SPSTmProvider = new SPSTmProvider(spstmSource, List.empty)

  /**
   * Constructor for SPSTm provider when spstmSource is workflow.provider.source.spstm.SPSTmSourceSerialized.
   * The source contains the path to the file with the serialized SPSTm.
   *
   * @param spstmSource A workflow.provider.source.spstm.SPSTmSourceSerialized.
   * @return            A SPSTm provider.
   */
  def apply(spstmSource: SPSTmSourceSerialized): SPSTmProvider =
    new SPSTmProvider(spstmSource, List(new FileExistsCondition(spstmSource.fn)))
}

/**
 * According to type of source for SPSTm, builds a provider in the form of a list of SPSTm interfaces.
 *
 * @param spstmSource The source for the SPSTm:
 *                   - SPSTmSourceDirectI when there already exists a SPSTm.
 *                   - SPSTmSourceFromSREM when the SPSTm must be learnt from the given SREM and a training stream.
 *                   - SPSTmSourceFromDSRA when the SPSTm must be learnt from the given dSRA and a training stream.
 *                   - SPSTmSourceSerialized when the SPSTm have been serialized and stored.
 * @param conditions A list of conditions that must be checked and satisfied.
 */
class SPSTmProvider private (
                              spstmSource: SPSTmSource,
                              conditions: List[Condition]
                            ) extends AbstractProvider(conditions) with LazyLogging {

  /**
   * Calling this function actually initiates the construction of the SPSTm.
   * Before calling this, nothing is done.
   * CAUTION: Do not create a provider, then delete its source (e.g. the file the the serialized SPSTm) and then call
   * provide(). Keep the source until you call provide().
   *
   * @return A list of SPSTm interfaces.
   */
  override def provide(): List[SPSTmInterface] = {
    spstmSource match {
      case x: SPSTmSourceDirectI => x.spstmi
      case x: SPSTmSourceFromSREM => srem2spstm(x.patternFile, x.declarationsFile, x.streamSource, x.pMin, x.alpha, x.gammaMin, x.r)
      case x: SPSTmSourceFromDSRA => dsra2spstm(x.dsrap, x.orders, x.pstSource)
      case x: SPSTmSourceSerialized => deserializeFile(x.fn)
      case _ => {
        logger.error("Not valid SPSTmSource")
        throw new Error("Not valid SPSTmSource")
      }
    }
  }

  /**
   * Learns SPSTm from dSRA and a training stream.
   *
   * @param dsrap     The provider for the dSRA.
   * @param orders    The order of the SPSTm (each SPSTm has its own).
   * @param pstSource The pstSource inside the SPSTm.
   * @return          A list of SPSTm interfaces.
   */
  private def dsra2spstm(
                          dsrap: DSRAProvider,
                          orders: List[Int],
                          pstSource: PSTSource
                        ): List[SPSTmInterface] = {
    val pstps = PSTProvider(pstSource).provide().map(pst => PSTSourceDirect(List((pst._1, pst._2.asInstanceOf[SymbolExtractorFromDSRA]))))
    val dsraisPstpsOrders = (dsrap.provide(), pstps, orders).zipped.toList
    dsraisPstpsOrders.map(spo => dsra2spstm(spo._1, spo._3, spo._2, spo._1.id))
  }

  /**
   * Learns a SPSTm from a dSRA and a training stream.
   *
   * @param dsrai     The dSRA interface.
   * @param order     The order of the SPSTm.
   * @param pstSource The pstSource inside the SPSTm.
   * @param id        The unique id for the SPSTm.
   * @return          A SPSTm interface.
   */
  private def dsra2spstm(
                          dsrai: DSRAInterface,
                          order: Int,
                          pstSource: PSTSource,
                          id: Int
                        ): SPSTmInterface = {
    val pstp = PSTProvider(pstSource)
    val pst = pstp.provide().head
    val spstmi = SPSTmInterface(pst._1, dsrai.dsra, pst._2.asInstanceOf[SymbolExtractorFromDSRA], order, id, dsrai.partitionAttribute)
    spstmi
  }

  /**
   * Constructs SPSTm interfaces from a set of patterns and a training stream.
   *
   * @param patternsFile      The path to the file with the patterns.
   * @param declarationsFile  The path to the declarations file.
   * @param streamSource      The stream source.
   * @param pMin              This is the symbol threshold. Symbols with lower probability are discarded
   *                          (same for all SPSTm).
   * @param alpha             Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on
   *                          the expanded context must be greater than this threshold (same for all SPSTm).
   * @param gammaMin          Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on
   *                          the expanded context must be greater than this threshold (same for all SPSTm).
   * @param r                 This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of
   *                          the conditional on the expanded context by the conditional on the original context is
   *                          greater than this threshold (same for all SPSTm).
   * @return                  A list of SPSTm interfaces.
   */
  private def srem2spstm(
                          patternsFile: String,
                          declarationsFile: String,
                          streamSource: StreamSource,
                          pMin: Double,
                          alpha: Double,
                          gammaMin: Double,
                          r: Double
                        ): List[SPSTmInterface] = {
    val (formulas, exclusives, extras) = SREUtils.sre2formulas(patternsFile, declarationsFile, withSelection = false)
    val dsraWithOrdersAttrs = formula2dsra(formulas, exclusives, extras)
    val dsraList = dsraWithOrdersAttrs.map(x => x._1)
    val ordersList = dsraWithOrdersAttrs.map(x => x._2)
    val attrsList = dsraWithOrdersAttrs.map(x => x._3)
    val spstmi = dsraList.indices.map(f => {
      val dsrap = DSRAProvider(DSRASourceDirect(List(dsraList(f)), List(attrsList(f))))
      val pstp = PSTProvider(PSTSourceLearnerFromDSRA(dsrap, streamSource, ordersList(f), pMin, alpha, gammaMin, r))
      val pst = pstp.provide().head
      val spsti = SPSTmInterface(pst._1, dsraList(f), pst._2.asInstanceOf[SymbolExtractorFromDSRA], ordersList(f), f+1, attrsList(f))
      spsti
    }).toList
    spstmi
  }

  /**
   * Given a list of formulas, extras and exclusives, builds the actual streaming DSRA.
   *
   * @param formulas    The list of formulas.
   * @param exclusives  Sets of exclusive predicates.
   * @param extras      Sets of extra predicates.
   * @return            A list of DSRA.
   */
  private def formula2dsra(
                            formulas: List[(SREFormula, Int, String, Int, String)],
                            exclusives: Set[Set[Predicate]],
                            extras: Set[Sentence]
                          ): List[(DSRAStreaming,Int,String)] = {
    logger.info("Building NSRA")
    val dsra = formulas.map(f => (DSRAUtils.buildDSRAForStream(f._1, f._4), f._2, f._3))
    dsra
  }

  /**
   * Actual deserialization from a single file.
   *
   * @param fn  The path to the file containing the serialized list of SPSTm.
   * @return    A list of SPSTm.
   */
  private def deserializeFile(fn: String): List[SPSTmInterface] = {
    val oisSPST = new ObjectInputStream(new FileInputStream(fn))
    val spst = oisSPST.readObject.asInstanceOf[List[SPSTmInterface]]
    oisSPST.close()
    spst
  }

}

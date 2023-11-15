package workflow.provider

import java.io.{FileInputStream, ObjectInputStream}
import com.typesafe.scalalogging.LazyLogging
import fsm.CountPolicy.CountPolicy
import fsm.symbolic.logic.{Predicate, Sentence}
import fsm.symbolic.sfa.SFAUtils
import fsm.symbolic.sfa.sdfa.{SDFA, SDFAUtils}
import fsm.symbolic.sfa.snfa.SNFAUtils
import fsm.symbolic.sre.{SREFormula, SREUtils}
import fsm.{SDFAInterface, SPSTInterface}
import model.vmm.mapper.Isomorphism
import stream.source.StreamSource
import ui.ConfigUtils
import workflow.condition.{Condition, FileExistsCondition}
import workflow.provider.source.pst.{PSTSource, PSTSourceDirect, PSTSourceLearnerFromSDFA}
import workflow.provider.source.sdfa.SDFASourceDirect
import workflow.provider.source.spst.{SPSTSource, SPSTSourceDirectI, SPSTSourceFromSDFA, SPSTSourceFromSRE, SPSTSourceSerialized}

object SPSTProvider {
  /**
    * Constructor for SPST provider.
    *
    * @param spstSource The source for the SPST.
    * @return A SPST provider.
    */
  def apply(spstSource: SPSTSource): SPSTProvider = new SPSTProvider(spstSource, List.empty)

  /**
    * Constructor for SPST provider when spstSource is workflow.provider.source.spst.SPSTSourceSerialized.
    * The source contains the path to the file with the serialized SPST.
    *
    * @param spstSource A workflow.provider.source.spst.SPSTSourceSerialized.
    * @return A SPST provider.
    */
  def apply(spstSource: SPSTSourceSerialized): SPSTProvider =
    new SPSTProvider(spstSource, List(new FileExistsCondition(spstSource.fn)))
}

/**
  * According to type of source for SPST, builds a provider in the form of a list of SPST interfaces.
  *
  * @param spstSource The source for the SPST:
  *                   - SPSTSourceDirectI when there already exists a SPST.
  *                   - SPSTSourceFromSRE when the SPST must be learnt from the given SRE and a training stream.
  *                   - SPSTSourceFromSDFA when the SPST must be learnt from the given SDFA and a training stream.
  *                   - SPSTSourceSerialized when the SPST have been serialized and stored.
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class SPSTProvider private (
                             spstSource: SPSTSource,
                             conditions: List[Condition]
                           ) extends AbstractProvider(conditions) with LazyLogging {

  /**
    * Calling this function actually initiates the construction of the SPSTs.
    * Before calling this, nothing is done.
    * CAUTION: Do not create a provider, then delete its source (e.g. the file the the serialized SPST) and then call
    * provide(). Keep the source until you call provide().
    *
    * @return A list of SPST interfaces.
    */
  override def provide(): List[SPSTInterface] = {
    spstSource match {
      case x: SPSTSourceDirectI => x.spsti
      case x: SPSTSourceFromSRE =>sre2spst(x.patternFile, x.declarationsFile, x.streamSource, x.policy, x.pMin, x.alpha, x.gammaMin, x.r)
      case x: SPSTSourceFromSDFA => sdfa2spst(x.sdfap, x.orders, x.pstSource)
      case x: SPSTSourceSerialized => deserializeFile(x.fn)
      case _ => {
        logger.error("Not valid SPSTSource")
        throw new Error("Not valid SPSTSource")
      }
    }
  }

  /**
    * Given a list of formulas, extras and exclusives, builds the actual SDFA.
    * First creates SNFA (with minor modification for streams), then goes through determinization.
    *
    * @param formulas The list of formulas.
    * @param policy Counting policy.
    * @param exclusives Sets of exclusive predicates.
    * @param extras Sets of extra predicates.
    * @param minTermMethod The method for constructing the minterms, withsat or withoutsat.
    * @return a list of SDFA
    */
  private def formula2sdfa(
                            formulas: List[(SREFormula, Int, String, Int, String)],
                            policy: CountPolicy,
                            exclusives: Set[Set[Predicate]],
                            extras: Set[Sentence],
                            minTermMethod: String
                          ): List[(SDFA,Int,String)] = {
    logger.info("Building SNFA")
    val snfaStream = formulas.map(f => (SNFAUtils.buildSNFAForStream(f._1), f._2, f._3))
    logger.info("Determinizing")
    val sdfa = snfaStream.map(s => (SFAUtils.determinizeI(s._1, exclusives, extras, minTermMethod), s._2, s._3))
    sdfa.foreach(s => SDFAUtils.checkForDead(s._1))
    val sdfap = sdfa.map(s => (SDFAUtils.setPolicy(s._1, policy), s._2, s._3))
    sdfap
  }

  /**
    * Constructs SPST interfaces from a set of patterns and a training stream.
    *
    * @param patternsFile The path to the file with the patterns.
    * @param declarationsFile The path to the declarations file.
    * @param streamSource The stream source.
    * @param policy The counting policy.
    * @param pMin This is the symbol threshold. Symbols with lower probability are discarded (same for all SPST).
    * @param alpha Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
    *              context must be greater than this threshold (same for all SPST).
    * @param gammaMin Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                 expanded context must be greater than this threshold (same for all SPST).
    * @param r This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
    *          on the expanded context by the conditional on the original context is greater than this threshold (same
    *          for all SPST).
    * @return A list of SPST interfaces.
    */
  private def sre2spst(
                        patternsFile: String,
                        declarationsFile: String,
                        streamSource: StreamSource,
                        policy: CountPolicy,
                        pMin: Double,
                        alpha: Double,
                        gammaMin: Double,
                        r: Double
                      ): List[SPSTInterface] = {
    val (formulas, exclusives, extras) = SREUtils.sre2formulas(patternsFile, declarationsFile, withSelection = false)
    val sdfaWithOrdersAttrs = formula2sdfa(formulas, policy, exclusives, extras, ConfigUtils.defaultMinTermMethod)
    val sdfaList = sdfaWithOrdersAttrs.map(x => x._1)
    val ordersList = sdfaWithOrdersAttrs.map(x => x._2)
    val attrsList = sdfaWithOrdersAttrs.map(x => x._3)
    val spsti = sdfaList.indices.map(f => {
      val sdfap = SDFAProvider(SDFASourceDirect(List(sdfaList(f)), List(attrsList(f))))
      val pstp = PSTProvider(PSTSourceLearnerFromSDFA(sdfap, streamSource, ordersList(f), pMin, alpha, gammaMin, r))
      val pst = pstp.provide().head
      val spsti = SPSTInterface(pst._1, sdfaList(f), pst._2.asInstanceOf[Isomorphism], ordersList(f), f+1, attrsList(f))
      spsti
    }).toList
    spsti
  }

  /**
    * Learns SPST from SDFA and a training stream.
    *
    * @param sdfap The provider for the SDFA.
    * @param orders The order of the SPST (each SPST has its own).
    * @param pstSource The pstSource inside the SPST
    * @return A list of SPST interfaces.
    */
  private def sdfa2spst(
                         sdfap: SDFAProvider,
                         orders: List[Int],
                         pstSource: PSTSource
                       ): List[SPSTInterface] = {
    val pstps = PSTProvider(pstSource).provide.map(pst => PSTSourceDirect(List((pst._1,pst._2.asInstanceOf[Isomorphism]))))
    val sdfaisPstpsOrders = (sdfap.provide(), pstps, orders).zipped.toList
    sdfaisPstpsOrders.map(spo => sdfa2spst(spo._1, spo._3, spo._2, spo._1.id))
  }

  /**
    * Learns a SPST from a SDFA and a training stream.
    *
    * @param sdfai The SDFA interface.
    * @param order The order of the SPST.
    * @param pstSource The pstSource inside the SPST
    * @param id The unique id for the SPST.
    * @return A SPST interface.
    */
  private def sdfa2spst(
                         sdfai: SDFAInterface,
                         order: Int,
                         pstSource: PSTSource,
                         id: Int
                       ): SPSTInterface = {
    val pstp = PSTProvider(pstSource)
    val pst = pstp.provide().head
    val spsti = SPSTInterface(pst._1, sdfai.sdfa, pst._2.asInstanceOf[Isomorphism], order, id, sdfai.partitionAttribute)
    spsti
  }

  /**
    * Actual deserialization from a single file.
    *
    * @param fn The path to the file containing the serialized list of SPST.
    * @return a list of SPST
    */
  private def deserializeFile(fn: String): List[SPSTInterface] = {
    val oisSPST = new ObjectInputStream(new FileInputStream(fn))
    val spst = oisSPST.readObject.asInstanceOf[List[SPSTInterface]]
    oisSPST.close()
    spst
  }

}

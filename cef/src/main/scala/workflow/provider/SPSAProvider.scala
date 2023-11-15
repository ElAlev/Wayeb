package workflow.provider

import java.io.{File, FileInputStream, ObjectInputStream}
import com.typesafe.scalalogging.LazyLogging
import fsm.SPSAInterface
import fsm.CountPolicy.CountPolicy
import stream.source.StreamSource
import model.vmm.VMMUtils
import model.vmm.pst.spsa.SymbolicPSA
import workflow.condition.{Condition, FileExistsCondition}
import workflow.provider.source.spsa.{
  SPSASource,
  SPSASourceDirect,
  SPSASourceDirectI,
  SPSASourceFromSRE,
  SPSASourcePSASerialized,
  SPSASourceSerialized
}

object SPSAProvider {
  /**
    * Constructor for SPSA provider when sdfaSource is workflow.provider.source.spsa.SPSASourceDirectI.
    * The source contains already the SPSA interfaces.
    *
    * @param spsaSource A workflow.provider.source.spsa.SPSASourceDirectI.
    * @return A SPSA provider.
    */
  def apply(spsaSource: SPSASourceDirectI): SPSAProvider = new SPSAProvider(spsaSource, List.empty)

  /**
    * Constructor for SPSA provider when sdfaSource is workflow.provider.source.spsa.SPSASourceDirectI.
    * The source contains already the SPSA. We only need to add ids.
    *
    * @param spsaSource A workflow.provider.source.spsa.SPSASourceDirectI.
    * @return A SPSA provider.
    */
  def apply(spsaSource: SPSASourceDirect): SPSAProvider = new SPSAProvider(spsaSource, List.empty)

  /**
    * Constructor for SPSA provider when sdfaSource is workflow.provider.source.spsa.SPSASourceSerialized.
    * The source contains the path to the file with the serialized list of SPSA interfaces.
    *
    * @param spsaSource A workflow.provider.source.spsa.SPSASourceSerialized.
    * @return A SPSA provider.
    */
  def apply(spsaSource: SPSASourceSerialized): SPSAProvider =
    new SPSAProvider(spsaSource, List(new FileExistsCondition(spsaSource.fn)))

  /**
    * Constructor for SPSA provider when sdfaSource is workflow.provider.source.spsa.SPSASourceFromSRE.
    * The source contains the path to the file with the patterns and a training stream.
    *
    * @param spsaSource A workflow.provider.source.spsa.SPSASourceFromSRE.
    * @return A SPSA provider.
    */
  def apply(spsaSource: SPSASourceFromSRE): SPSAProvider =
    new SPSAProvider(spsaSource, List(new FileExistsCondition(spsaSource.patternFile)))

  /**
    * Constructor for SPSA provider when sdfaSource is workflow.provider.source.spsa.SPSASourcePSASerialized.
    * The source contains the path to the file with the patterns and PSA provider.
    *
    * @param spsaSource A workflow.provider.source.spsa.SPSASourcePSASerialized.
    * @return A SPSA provider.
    */
  def apply(spsaSource: SPSASourcePSASerialized): SPSAProvider =
    new SPSAProvider(spsaSource, List(new FileExistsCondition(spsaSource.patternFile)))
}

/**
  * According to type of source for SPST, builds a provider in the form of a list of SPST interfaces.
  *
  * @param spsaSource The source for the SPSA:
  *                   - SPSASourceDirectI/SPSASourceDirect when the SPSA already exists.
  *                   - SPSASourceSerialized when the SPSA has been serialized and stored.
  *                   - SPSASourceFromSRE when the SPSA must be learnt from SDFA and training stream.
  *                   - SPSASourcePSASerialized when the PSA have been serialized and stored and need to be embedded to
  *                   the SDFA.
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class SPSAProvider private (
                             spsaSource: SPSASource,
                             conditions: List[Condition]
                           ) extends AbstractProvider(conditions) with LazyLogging {

  checkConditions()

  /**
    * Calling this function actually initiates the construction of the SPSAs.
    * Before calling this, nothing is done.
    * CAUTION: Do not create a provider, then delete its source (e.g. the file the the serialized SPSA) and then call
    * provide(). Keep the source until you call provide().
    *
    * @return A list of SPSA interfaces.
    */
  override def provide(): List[SPSAInterface] = {
    spsaSource match {
      case x: SPSASourceDirectI => x.spsai
      case x: SPSASourceDirect =>
        if (x.partitionAttributes.isEmpty) addIds(x.spsa)
        else addIdsAttr(x.spsa.zip(x.partitionAttributes))
      case x: SPSASourceSerialized => deserialize(x.fn)
      case x: SPSASourceFromSRE => sre2spsa(x.patternFile, x.declarationsFile, x.streamSource, x.policy, x.maxNoStates)
      case x: SPSASourcePSASerialized => sre2spsaPSASer(x.patternFile, x.declarationsFile, x.psap, x.policy)
      case _ => {
        logger.error("Not valid SPSASource")
        throw new Error("Not valid SPSASource")
      }
    }
  }

  /**
    * Creates SPSA from a set of given patterns and a set of leanrt PSA ny embedding each PSA in its
    * respective SDFA.
    *
    * @param patternsFile The path to the file with the patterns.
    * @param declarationsFile The path to the declarations file.
    * @param psap The provider for the already learnt PSA.
    * @param policy The counting policy.
    * @return A list of SPSA interfaces.
    */
  private def sre2spsaPSASer(
                              patternsFile: String,
                              declarationsFile: String,
                              psap: PSAProvider,
                              policy: CountPolicy
                            ): List[SPSAInterface] = {
    val psa = psap.provide()
    val spsas = VMMUtils.embedPSAsInSDFAs(patternsFile, declarationsFile, policy, psa)
    addIdsAttr(spsas)
  }

  /**
    * Learns a SPSA from a pattern and a training stream.
    *
    * @param patternsFile The path to the file with the pattern.
    * @param declarationsFile The path to the declarations file.
    * @param streamSource The source for the training stream.
    * @param policy The counting policy.
    * @param maxNoStates The maximum number of states for the intermediate PSA.
    * @return A list of learnt SPSA interfaces.
    */
  private def sre2spsa(
                        patternsFile: String,
                        declarationsFile: String,
                        streamSource: StreamSource,
                        policy: CountPolicy,
                        maxNoStates: Int
                      ): List[SPSAInterface] = {
    val spsas = VMMUtils.learnSymbolicPSA(patternsFile, declarationsFile, streamSource, policy, maxNoStates)
    addIdsAttr(spsas)
  }

  /**
    * Just creates and adds an identifier to each SPSA.
    *
    * @param spsas The list of SPSAs.
    * @return list of corresponding SPSA with identifiers.
    */
  private def addIds(spsas: List[SymbolicPSA]): List[SPSAInterface] = {
    val spsai = spsas.zipWithIndex
    val fsm = spsai.map(s => SPSAInterface(s._1, s._2))
    fsm
  }

  /**
    * Just creates and adds an identifier to each SPSA.
    *
    * @param spsas The list of SPSAs/partition attributes.
    * @return list of corresponding SPSA with identifiers.
    */
  private def addIdsAttr(spsas: List[(SymbolicPSA, String)]): List[SPSAInterface] = {
    val spsai = spsas.zipWithIndex
    val fsm = spsai.map(s => SPSAInterface(s._1._1, s._2, s._1._2))
    fsm
  }

  /**
    * Just deserializes a list of serialized SPSA.
    *
    * @param fn The path to the file/directory containing the serialized list of SPSA.
    *           If file, directly deserialize. If directory, gather all files, deserialize them all and give new
    *           identifiers to avoid id conflicts.
    * @return a list of SPSA
    */
  private def deserialize(fn: String): List[SPSAInterface] = {
    val fd = new File(fn)
    if (fd.isFile) deserializeFile(fn)
    else if (fd.isDirectory) {
      val files = fd.list().toList.sorted
      val spsas = files.flatMap(f => deserializeFile(fn + "/" + f))
      resetIds(spsas)
    } else throw new Error("Something wrong with given SPSA file/directory.")
  }

  /**
    * Resets all ids for the SPSAs.
    *
    * @param spsas A list with SPSA interfaces.
    * @return The list with the SPSA interfaces with new ids.
    */
  private def resetIds(spsas: List[SPSAInterface]): List[SPSAInterface] = {
    spsas.zipWithIndex.map(si => SPSAInterface(
      sdfa               = si._1.sdfa,
      label2id           = si._1.label2id,
      id2label           = si._1.id2label,
      state2row          = si._1.state2row,
      transitionMatrix   = si._1.transitionMatrix,
      iso                = si._1.iso,
      startStates        = si._1.startStates,
      maxOrder           = si._1.maxOrder,
      id                 = si._2,
      partitionAttribute = si._1.partitionAttribute
    ))
  }

  /**
    * Actual deserialization from a single file.
    *
    * @param fn The path to the file containing the serialized list of SPSA.
    * @return a list of SPSA
    */
  private def deserializeFile(fn: String): List[SPSAInterface] = {
    val oisSPSA = new ObjectInputStream(new FileInputStream(fn))
    val spsa = oisSPSA.readObject.asInstanceOf[List[SPSAInterface]]
    oisSPSA.close()
    spsa
  }

  /**
    * Checks all conditions.
    * TODO: do the checking in the super-class workflow.provider.AbstractProvider?
    *
    * @return True if all conditions satisfied.
    */
  def checkConditions(): Boolean = {
    val superchecks = super.check()
    if (superchecks.contains(false)) {
      logger.error("Provider conditions unsatisfied")
      throw new Error("Provider conditions unsatisfied")
      false
    } else true
  }
}

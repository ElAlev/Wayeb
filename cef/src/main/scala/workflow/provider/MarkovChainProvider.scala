package workflow.provider

import java.io.{File, FileInputStream, ObjectInputStream}
import model.markov.{MarkovChain, MarkovChainFactory, TransitionProbs}
import stream.source.StreamSource
import workflow.condition.{Condition, FileExistsCondition}
import workflow.task.estimatorTask.MatrixMLETask
import workflow.provider.source.matrix.{
  MatrixSource,
  MCSourceDirect,
  MCSourceMLE,
  MCSourceProbs,
  MCSourceSPSA,
  MCSourceSerialized
}

object MarkovChainProvider {
  /**
    * Constructor for MC provider when sdfaSource is workflow.provider.source.matrix.MCSourceDirect.
    * The source contains already the Markov chains.
    *
    * @param mcSource A workflow.provider.source.matrix.MCSourceDirect.
    * @return A MC provider.
    */
  def apply(mcSource: MCSourceDirect): MarkovChainProvider = new MarkovChainProvider(mcSource, List.empty[Condition])

  /**
    * Constructor for MC provider when sdfaSource is workflow.provider.source.matrix.MCSourceMLE.
    * The source contains the provider for FSMs and a source for the training stream.
    *
    * @param mcSource A workflow.provider.source.matrix.MCSourceMLE.
    * @return A MC provider.
    */
  def apply(mcSource: MCSourceMLE): MarkovChainProvider = new MarkovChainProvider(mcSource, List.empty[Condition])

  /**
    * Constructor for MC provider when sdfaSource is workflow.provider.source.matrix.MCSourceProbs.
    * The source contains a set of conditional probabilities.
    *
    * @param mcSource A workflow.provider.source.matrix.MCSourceProbs.
    * @return A MC provider.
    */
  def apply(mcSource: MCSourceProbs): MarkovChainProvider = new MarkovChainProvider(mcSource, List.empty[Condition])

  /**
    * Constructor for MC provider when sdfaSource is workflow.provider.source.matrix.MCSourceSerialized.
    * The source contains the path to the file with the serialized MCs.
    *
    * @param mcSource A workflow.provider.source.matrix.MCSourceSerialized.
    * @return A MC provider.
    */
  def apply(mcSource: MCSourceSerialized): MarkovChainProvider =
    new MarkovChainProvider(mcSource, List(new FileExistsCondition(mcSource.fn)))

  /**
    * Constructor for MC provider when sdfaSource is workflow.provider.source.matrix.MCSourceSPSA.
    * The source contains SPSAa.
    *
    * @param mcSource A workflow.provider.source.matrix.MCSourceSPSA.
    * @return A MC provider.
    */
  def apply(mcSource: MCSourceSPSA): MarkovChainProvider = new MarkovChainProvider(mcSource, List.empty[Condition])
}

/**
  * According to type of source for the Markov chain, builds a provider in the form of a list of Markov chains.
  *
  * @param mcSource The source for the Markov chain:
  *                 - MCSourceDirect when the MC already exists.
  *                 - MCSourceMLE when the transition matrix must be estimated through MLE.
  *                 - MCSourceProbs when the MC must be constructed from a set of conditional probabilities.
  *                 - MCSourceSerialized when the MC ahs been serialized and saved.
  *                 - MCSourceSPSA when the MC is to be retreived from SPSA.
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class MarkovChainProvider private(
                                   mcSource: MatrixSource,
                                   conditions: List[Condition]
                                 ) extends AbstractProvider(conditions) {

  /**
    * Calling this function actually initiates the construction of the Markov chains.
    *
    * @return A list of Markov chains.
    */
  override def provide(): List[MarkovChain] = {
    mcSource match {
      case x: MCSourceDirect => x.mcs
      case x: MCSourceMLE => estimateMatrix(x.fsmp, x.streamSource)
      case x: MCSourceProbs => createMatrixFromProbs(x.fsmp, x.probs)
      case x: MCSourceSerialized => deserialize(x.fn)
      case x: MCSourceSPSA => getMarkovChainFromSPSA(x.spsa)
      case _ => throw new Error("Not valid MatrixSource")
    }
  }

  /**
    * Estimates the transition matrix of the FSM(s) through maximum likelihood estimation.
    *
    * @param fsmp The provider for FSMs.
    * @param streamSource The source for the training stream.
    * @return The list of Markov chains.
    */
  private def estimateMatrix(
                              fsmp: FSMProvider,
                              streamSource: StreamSource
                            ): List[MarkovChain] = {
    val mlet = MatrixMLETask(fsmp, streamSource)
    mlet.execute()._1
  }

  /**
    * Creates the transition matrix of the FSMs from given conditional probabilities.
    *
    * @param fsmp The provider for the FSMs.
    * @param probs The conditional probabilities.
    * @return The list of Markov chains.
    */
  private def createMatrixFromProbs(
                                     fsmp: FSMProvider,
                                     probs: TransitionProbs
                                   ): List[MarkovChain] = {
    val fsmList = fsmp.provide()
    val mcs = fsmList.map(fsm => MarkovChainFactory.buildMC(fsm, probs))
    mcs
  }

  /**
    * Retrieves the Markov chains from the given SPSAs.
    *
    * @param spsap The provider for the SPSAs.
    * @return The list of Markov chains.
    */
  private def getMarkovChainFromSPSA(spsap: SPSAProvider): List[MarkovChain] = {
    val spsa = spsap.provide()
    spsa.map(x => x.getMarkovChain)
  }

  /**
    * Just deserializes a list of serialized Markov chains.
    *
    * @param fn The path to the file/directory containing the serialized list of MCs.
    *           If file, directly deserialize. If directory, gather all files and deserialize them.
    * @return A list of SDFA
    */
  private def deserialize(fn: String): List[MarkovChain] = {
    val fd = new File(fn)
    if (fd.isFile) deserializeFile(fn)
    else if (fd.isDirectory) {
      val files = fd.list().toList.sorted
      val mcs = files.flatMap(f => deserializeFile(fn + "/" + f))
      mcs
    } else throw new Error("Something wrong with given MCs file/directory.")
  }


  /**
    * Actual deserialization from a single file.
    *
    * @param fn The path to the file containing the serialized list of MCs.
    * @return A list of MCs
    */
  private def deserializeFile(fn: String): List[MarkovChain] = {
    val oisDFA = new ObjectInputStream(new FileInputStream(fn))
    val mc = oisDFA.readObject.asInstanceOf[List[MarkovChain]]
    oisDFA.close()
    mc
  }
}

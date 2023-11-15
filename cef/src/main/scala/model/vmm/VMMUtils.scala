package model.vmm

import com.typesafe.scalalogging.LazyLogging
import fsm.CountPolicy.CountPolicy
import fsm.symbolic.logic.{EpsilonSentence, LogicUtils, Predicate, Sentence}
import fsm.symbolic.sfa.SFAUtils
import fsm.symbolic.sfa.sdfa.{SDFA, SDFAUtils}
import fsm.symbolic.sfa.snfa.{SNFA, SNFAUtils}
import fsm.symbolic.sra.dsra.DSRAStreaming
import fsm.symbolic.sre.{SREFormula, SREUtils}
import model.vmm.mapper.{Isomorphism, SymExBank, SymbolExtractorFromDSRA, SymbolMapper}
import model.vmm.pst.psa.{PSAUtils, ProbSuffixAutomaton}
import model.vmm.pst.spsa.{SPSAUtils, SymbolicPSA}
import model.vmm.pst.{CSTLearner, CounterSuffixTree, PSTLearner, PredictionSuffixTree}
import stream.source.StreamSource

object VMMUtils extends LazyLogging {

  /**
    * Learns a list of symbolic probabilistic suffix automata (SPSA, i.e., an embedding of a PSA in a SDFA) from a list
    * of patterns.
    *
    * @param patternsFile The path to the file with the patterns.
    * @param declarationsFile The path to the file with the declarations.
    * @param streamSource The source for the training stream.
    * @param policy Teh counting policy for all the patterns (common).
    * @param maxNoStates The maximum number of states that the PSA (not the SPSA) can have.
    * @return A list of SPSA, along with the partition attributes.
    */
  def learnSymbolicPSA(
                        patternsFile: String,
                        declarationsFile: String,
                        streamSource: StreamSource,
                        policy: CountPolicy,
                        maxNoStates: Int
                      ): List[(SymbolicPSA, String)] = {
    logger.debug("Parsing formulas and declarations")
    val (formulas, exclusives, extras) = SREUtils.sre2formulas(patternsFile, declarationsFile, withSelection = false)
    val spsas = formulas.map(f => learnSPSAFromSingleFormula(f, exclusives, extras, policy, streamSource, maxNoStates))
    spsas.map(spsa => (spsa._1, spsa._2))
  }

  /**
    * Learns a SPSA from a formula and a training stream.
    *
    * @param formula The formula with its order and partition attribute.
    * @param exclusives The set of exclusive predicates
    * @param extras The set of extra predicates.
    * @param policy Teh counting policy.
    * @param streamSource The source for the training stream.
    * @param maxNoStates The maximum number of states that the intermediate PSA is allowed to have.
    * @return The SPSA, along with the partition attribute and the PSA.
    */
  private def learnSPSAFromSingleFormula(
                                          formula: (SREFormula, Int, String, Int, String),
                                          exclusives: Set[Set[Predicate]],
                                          extras: Set[Sentence],
                                          policy: CountPolicy,
                                          streamSource: StreamSource,
                                          maxNoStates: Int
                                        ): (SymbolicPSA, String, ProbSuffixAutomaton) = {
    val snfaStream = SNFAUtils.buildSNFAForStream(formula._1)
    val sdfa = SFAUtils.determinizeI(snfaStream, exclusives, extras)
    SDFAUtils.checkForDead(sdfa)
    val sdfap = SDFAUtils.setPolicy(sdfa, policy)
    //TODO: why not sdfap?
    val iso = createIsomorphism(snfaStream, exclusives, extras)
    val psa = learnProbSuffixAutomaton(streamSource, formula._2, iso, maxNoStates, formula._3)
    val spsa = SPSAUtils.buildSPSA(sdfap, psa, iso)
    (spsa, formula._3, psa)
  }

  /**
    * Learns a SPSA from a formula and a training stream.
    * Same as model.vmm.VMMUtils#learnSPSAFromSingleFormula(scala.Tuple3, scala.collection.immutable.Set, scala.collection.immutable.Set, scala.Enumeration.Value, stream.source.StreamSource, int, model.vmm.mapper.Isomorphism),
    * but isomorphism is provided directly. Sometimes it's more efficient to manually create an isomorphism instead of
    * constructing it from the formula and the SDFA.
    *
    * @param formula The formula with its order and partition attribute.
    * @param exclusives The set of exclusive predicates
    * @param extras The set of extra predicates.
    * @param policy Teh counting policy.
    * @param streamSource The source for the training stream.
    * @param maxNoStates The maximum number of states that the intermediate PSA is allowed to have.
    * @param iso The isomorphism.
    * @return The SPSA, along with the partition attribute and the PSA.
    */
  def learnSPSAFromSingleFormula(
                                  formula: (SREFormula, Int, String),
                                  exclusives: Set[Set[Predicate]],
                                  extras: Set[Sentence],
                                  policy: CountPolicy,
                                  streamSource: StreamSource,
                                  maxNoStates: Int,
                                  iso: Isomorphism
                                ): (SymbolicPSA, String, ProbSuffixAutomaton) = {
    logger.info("Building SNFA")
    val snfaStream = SNFAUtils.buildSNFAForStream(formula._1)
    logger.info("Determinizing")
    val sdfa = SFAUtils.determinizeI(snfaStream, exclusives, extras)
    SDFAUtils.checkForDead(sdfa)
    val sdfap = SDFAUtils.setPolicy(sdfa, policy)
    //TODO: why not sdfap?
    val (spsa, psa) = learnSPSAFromSingleSDFA(sdfap, formula._2, streamSource, iso, maxNoStates, formula._3)
    (spsa, formula._3, psa)
  }

  /**
    * Learns a SPSA from a SDFA and a training stream.
    *
    * @param sdfa The given SDFA.
    * @param maxOrder The maximum order of the PSA.
    * @param streamSource The source of the training stream.
    * @param iso The isomorphism of the given SDFA.
    * @param maxNoStates The maximum number of states that the intermediate PSA is allowed to have.
    * @param partitionAttribute The partition attribute of the pattern from which the isomorphism was derived.
    * @return The learnt SPSA along with the PSA.
    */
  def learnSPSAFromSingleSDFA(
                               sdfa: SDFA,
                               maxOrder: Int,
                               streamSource: StreamSource,
                               iso: Isomorphism,
                               maxNoStates: Int,
                               partitionAttribute: String
                             ): (SymbolicPSA, ProbSuffixAutomaton) = {
    logger.debug("Learning PSA")
    val psa = learnProbSuffixAutomaton(streamSource, maxOrder, iso, maxNoStates, partitionAttribute)
    logger.debug("Building SPSA")
    val spsa = SPSAUtils.buildSPSA(sdfa, psa, iso)
    logger.debug("SPSA has " + spsa.getSize + " states")
    (spsa, psa)
  }

  /**
    * Learns a SPSA from a SDFA and a training stream.
    * User-provided values for hyper-parameters.
    *
    * @param sdfa The given SDFA.
    * @param maxOrder The maximum order of the PSA.
    * @param streamSource The source of the training stream.
    * @param iso The isomorphism of the given SDFA.
    * @param partitionAttribute The partition attribute of the pattern from which the isomorphism was derived.
    * @param pMin This is the symbol threshold. Symbols with lower probability are discarded.
    * @param alpha Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
    *              context must be greater than this threshold.
    * @param gammaMin Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                 expanded context must be greater than this threshold.
    * @param r This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
    *          on the expanded context by the conditional on the original context is greater than this threshold.
    * @return The learnt SPSA along with the PSA.
    */
  def learnSPSAFromSingleSDFA(
                               sdfa: SDFA,
                               maxOrder: Int,
                               streamSource: StreamSource,
                               iso: Isomorphism,
                               partitionAttribute: String,
                               pMin: Double,
                               alpha: Double,
                               gammaMin: Double,
                               r: Double
                             ): (SymbolicPSA, ProbSuffixAutomaton) = {
    logger.debug("Learning PSA")
    val psa = learnProbSuffixAutomaton(streamSource, maxOrder, iso, partitionAttribute, pMin, alpha, gammaMin, r)
    logger.debug("Building SPSA")
    val spsa = SPSAUtils.buildSPSA(sdfa, psa, iso)
    logger.debug("SPSA has " + spsa.getSize + " states")
    (spsa, psa)
  }

  /**
    * Learns a PSA from a training stream. The symbols for the PSA are provided through a given isomorphism.
    *
    * @param streamSource The source for the training stream.
    * @param maxOrder The maximum order of the PSA.
    * @param iso The isomorphism containing the PSA symbols.
    * @param maxNoStates The maximum number of states the PSA is allowed to have.
    * @param partitionAttribute The partition attribute of the pattern from which the isomorphism was derived.
    * @return The learnt PSA.
    */
  private def learnProbSuffixAutomaton(
                                        streamSource: StreamSource,
                                        maxOrder: Int,
                                        iso: Isomorphism,
                                        maxNoStates: Int,
                                        partitionAttribute: String
                                      ): ProbSuffixAutomaton = {
    logger.debug("Learning probabilistic suffix automaton")
    val pst = learnPredictionSuffixTree(streamSource, maxOrder, iso, maxNoStates, partitionAttribute)
    val psta = pst.makePSACompatible()
    val psa = PSAUtils.buildPSA(psta)
    logger.debug("Done with learning probabilistic suffix automaton")
    logger.debug("PSA has " + psa.size + " states")
    psa
  }

  /**
    * Learns a PSA from a training stream. The symbols for the PSA are provided through a given isomorphism.
    * User-provided values for hyper-parameters.
    *
    * @param streamSource The source for the training stream.
    * @param maxOrder The maximum order of the PSA.
    * @param iso The isomorphism containing the PSA symbols.
    * @param partitionAttribute The partition attribute of the pattern from which the isomorphism was derived.
    * @param pMin This is the symbol threshold. Symbols with lower probability are discarded.
    * @param alpha Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
    *              context must be greater than this threshold.
    * @param gammaMin Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                 expanded context must be greater than this threshold.
    * @param r This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
    *          on the expanded context by the conditional on the original context is greater than this threshold.
    * @return The learnt PSA.
    */
  private def learnProbSuffixAutomaton(
                                        streamSource: StreamSource,
                                        maxOrder: Int,
                                        iso: Isomorphism,
                                        partitionAttribute: String,
                                        pMin: Double,
                                        alpha: Double,
                                        gammaMin: Double,
                                        r: Double
                                      ): ProbSuffixAutomaton = {
    logger.debug("Learning probabilistic suffix automaton")
    val pst = learnPredictionSuffixTree(streamSource, iso, partitionAttribute, maxOrder, pMin, alpha, gammaMin, r)
    val psta = pst.makePSACompatible()
    val psa = PSAUtils.buildPSA(psta)
    logger.debug("Done with learning probabilistic suffix automaton")
    logger.debug("PSA has " + psa.size + " states")
    psa
  }

  /**
    * Learns a prediction suffix tree from a training stream. Symbols provided through an isomorphism.
    *
    * @param streamSource The source of the training stream.
    * @param maxOrder The maximum order of the tree.
    * @param iso The isomorphism from which we get the symbols.
    * @param maxNoStates The maximum number of states that the PSA (after converting from the PST) is allowed to have.
    * @param partitionAttribute The partition attribute of the pattern from which the isomorphism was derived.
    * @return The learnt prediction suffix tree.
    */
  private def learnPredictionSuffixTree(
                                         streamSource: StreamSource,
                                         maxOrder: Int,
                                         iso: Isomorphism,
                                         maxNoStates: Int,
                                         partitionAttribute: String
                                       ): PredictionSuffixTree = {
    logger.debug("Learning prediction suffix tree from iso")
    val cst = learnCounterSuffixTree(streamSource, maxOrder, iso, partitionAttribute)
    val pstl = PSTLearner(iso.getSymbols.toSet, maxOrder, maxNoStates)
    val pst = pstl.learnOriginal(cst, withMissing = true)
    logger.debug("Done with learning prediction suffix tree")
    pst
  }

  /**
    * Learns a prediction suffix tree from a training stream. Symbols provided through an isomorphism.
    * User-provided values for hyper-parameters.
    *
    * @param streamSource The source of the training stream.
    * @param iso The isomorphism from which we get the symbols.
    * @param partitionAttribute The partition attribute of the pattern from which the isomorphism was derived.
    * @param maxOrder The maximum order of the tree.
    * @param pMin This is the symbol threshold. Symbols with lower probability are discarded.
    * @param alpha Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
    *              context must be greater than this threshold.
    * @param gammaMin Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                 expanded context must be greater than this threshold.
    * @param r This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
    *          on the expanded context by the conditional on the original context is greater than this threshold.
    * @return The learnt PST.
    */
  private def learnPredictionSuffixTree(
                                         streamSource: StreamSource,
                                         iso: Isomorphism,
                                         partitionAttribute: String,
                                         maxOrder: Int,
                                         pMin: Double,
                                         alpha: Double,
                                         gammaMin: Double,
                                         r: Double
                                       ): PredictionSuffixTree = {
    logger.debug("Learning prediction suffix tree from iso")
    val cst = learnCounterSuffixTree(streamSource, maxOrder, iso, partitionAttribute)
    learnPredictionSuffixTree(iso, cst, maxOrder, pMin, alpha, gammaMin, r, withMissing = true)._1
  }

  /**
    * Learns a prediction suffix tree from a training stream. Symbols provided through a SDFA.
    * User-provided values for hyper-parameters.
    *
    * @param streamSource The source of the training stream.
    * @param sdfa The given SDFA.
    * @param partitionAttribute The partition attribute of the pattern from which the isomorphism was derived.
    * @param maxOrder The maximum order of the tree.
    * @param pMin This is the symbol threshold. Symbols with lower probability are discarded.
    * @param alpha Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
    *              context must be greater than this threshold.
    * @param gammaMin Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                 expanded context must be greater than this threshold.
    * @param r This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
    *          on the expanded context by the conditional on the original context is greater than this threshold.
    * @return The learnt PST.
    */
  def learnPredictionSuffixTree(
                                 streamSource: StreamSource,
                                 sdfa: SDFA,
                                 partitionAttribute: String,
                                 maxOrder: Int,
                                 pMin: Double,
                                 alpha: Double,
                                 gammaMin: Double,
                                 r: Double
                               ): (PredictionSuffixTree, SymbolMapper) = {
    logger.debug("Learning prediction suffix tree from SDFA")
    val iso = createIsomorphism(sdfa)
    val cst = learnCounterSuffixTree(streamSource, maxOrder, iso, partitionAttribute)
    learnPredictionSuffixTree(iso, cst, maxOrder, pMin, alpha, gammaMin, r, withMissing = false)
  }

  /**
   * Learns a prediction suffix tree from a training stream. Symbols provided through a dSRA.
   * User-provided values for hyper-parameters.
   *
   * @param streamSource       The source of the training stream.
   * @param dsra               The given dSRA.
   * @param partitionAttribute The partition attribute of the pattern from which the isomorphism was derived.
   * @param maxOrder           The maximum order of the tree.
   * @param pMin               This is the symbol threshold. Symbols with lower probability are discarded.
   * @param alpha              Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
   *                           context must be greater than this threshold.
   * @param gammaMin           Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
   *                           expanded context must be greater than this threshold.
   * @param r                  This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
   *                           on the expanded context by the conditional on the original context is greater than this threshold.
   * @return The learnt PST.
   */
  def learnPredictionSuffixTree(
                                 streamSource: StreamSource,
                                 dsra: DSRAStreaming,
                                 partitionAttribute: String,
                                 maxOrder: Int,
                                 pMin: Double,
                                 alpha: Double,
                                 gammaMin: Double,
                                 r: Double
                               ): (PredictionSuffixTree, SymbolMapper) = {
    logger.debug("Learning prediction suffix tree from DSRA")
    val symExBank: SymExBank = SymExBank(dsra, partitionAttribute)
    val cst = learnCounterSuffixTree(streamSource, maxOrder, symExBank, partitionAttribute)
    val symbolExtractor: SymbolExtractorFromDSRA = symExBank.getSymbolExtractor
    learnPredictionSuffixTree(symbolExtractor, cst, maxOrder, pMin, alpha, gammaMin, r, withMissing = false)
  }

  /**
    * Learns a prediction suffix tree from a CounterSuffixTree. Symbols provided through an isomorphism
    * User-provided values for hyper-parameters.
    *
    * @param sm The isomorphism from which we get the symbols.
    * @param cst The learnt CounterSuffixTree.
    * @param maxOrder The maximum order of the tree.
    * @param pMin This is the symbol threshold. Symbols with lower probability are discarded.
    * @param alpha Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the expanded
    *              context must be greater than this threshold.
    * @param gammaMin Used to calculate the conditional threshold = (1 + alpha) * gammaMin. The conditional on the
    *                 expanded context must be greater than this threshold.
    * @param r This is the likelihood ratio threshold. Contexts are expanded if the probability ratio of the conditional
    *          on the expanded context by the conditional on the original context is greater than this threshold.
    * @param withMissing If true, missing children will be added to the pst.
    * @return The learnt PST.
    */
  def learnPredictionSuffixTree(
                                 sm: SymbolMapper,
                                 cst: CounterSuffixTree,
                                 maxOrder: Int,
                                 pMin: Double,
                                 alpha: Double,
                                 gammaMin: Double,
                                 r: Double,
                                 withMissing: Boolean
                               ): (PredictionSuffixTree, SymbolMapper) = {
    val pstl = PSTLearner(sm.getSymbols.toSet, maxOrder, pMin, alpha, gammaMin, r)
    val pst = pstl.learnVariant(cst, withMissing)
    logger.debug("Done with learning prediction suffix tree")
    (pst, sm)
  }

  /**
    * Constructs a counter suffix tree from a stream. Symbols provided through an isomorphism.
    *
    * @param streamSource The stream from which the tree is to be constructed.
    * @param maxOrder The tree's maximum order (depth).
    * @param sm The isomorphism providing the symbols.
    * @param partitionAttribute The partition attribute of the pattern from which the isomorphism was derived.
    * @return The counter suffix tree.
    */
  private def learnCounterSuffixTree(
                                      streamSource: StreamSource,
                                      maxOrder: Int,
                                      sm: SymbolMapper,
                                      partitionAttribute: String
                                    ): CounterSuffixTree = {
    require(maxOrder > 0)
    logger.debug("Learning counter suffix tree")
    val cstLearner = CSTLearner(maxOrder, sm, partitionAttribute)
    streamSource.emitEventsToListener(cstLearner)
    val cst = cstLearner.getCST
    logger.debug("Done with learning counter suffix tree")
    cst
  }

  /**
    * Embeds a set of given PSAs in a set of SDFAs from patterns provided in a file, producing a set of SPSAs.
    * For each pattern, a PSA must be given.
    *
    * @param patternsFile The path to the file with the patterns.
    * @param declarationsFile The path to the declarations file.
    * @param policy The counting policy.
    * @param psas The list of PSAs.
    * @return The list of SPSAs, along with their respective partition attributes.
    */
  def embedPSAsInSDFAs(
                        patternsFile: String,
                        declarationsFile: String,
                        policy: CountPolicy,
                        psas: List[ProbSuffixAutomaton]
                      ): List[(SymbolicPSA, String)] = {
    logger.debug("Parsing formulas and declarations")
    val (formulas, exclusives, extras) = SREUtils.sre2formulas(patternsFile, declarationsFile, withSelection = false)
    require(formulas.length == psas.length)
    val spsas = formulas.zip(psas).map(f => embedPSAinSDFA(f._1, exclusives, extras, policy, f._2))
    spsas
  }

  /**
    * Creates a SDFA from a given formula and then embeds a given PSA, producing a SPSA.
    *
    * @param formula The given formula.
    * @param exclusives The set of exclusives.
    * @param extras The set of extra predicates.
    * @param policy The counting policy.
    * @param psa The given PSA.
    * @return The SPSA, along with the partition attribute.
    */
  private def embedPSAinSDFA(
                              formula: (SREFormula, Int, String, Int, String),
                              exclusives: Set[Set[Predicate]],
                              extras: Set[Sentence],
                              policy: CountPolicy,
                              psa: ProbSuffixAutomaton
                            ): (SymbolicPSA, String) = {
    logger.info("Building SNFA")
    val snfaStream = SNFAUtils.buildSNFAForStream(formula._1)
    logger.info("Determinizing")
    val sdfa = SFAUtils.determinizeI(snfaStream, exclusives, extras)
    SDFAUtils.checkForDead(sdfa)
    val sdfap = SDFAUtils.setPolicy(sdfa, policy)
    logger.debug("Creating isomorphism")
    val iso = createIsomorphism(snfaStream, exclusives, extras)
    /*logger.debug("Building SPSA")
    val spsa = SPSAUtils.buildSPSA(sdfap, psa, iso)
    logger.debug("SPSA has " + spsa.getSize + " states")*/
    val spsa = embedPSAinSDFA(sdfap, psa, iso)
    (spsa, formula._3)
  }

  /**
    * Embeds a given PSA in a given SDFA, producing a SPSA.
    *
    * @param sdfa The given SDFA.
    * @param psa The given PSA.
    * @param iso The isomorphism corresponding to the SDFA and holding the symbols of the PSA.
    * @return The SPSA.
    */
  def embedPSAinSDFA(
                      sdfa: SDFA,
                      psa: ProbSuffixAutomaton,
                      iso: Isomorphism
                    ): SymbolicPSA = {
    logger.debug("Building SPSA")
    val spsa = SPSAUtils.buildSPSA(sdfa, psa, iso)
    logger.debug("SPSA has " + spsa.getSize + " states")
    spsa
  }

  /*
  def mergeSingleSFAPSA(
      formula: (SREFormula, Int, String),
      exclusives: Set[Set[Predicate]],
      extras: Set[Sentence],
      policy: CountPolicy,
      psa: ProbSuffixAutomaton,
      iso: Isomorphism
  ): (SymbolicPSA, String) = {
    logger.info("Building SNFA")
    val snfaStream = SNFAUtils.buildSNFAForStream(formula._1)
    logger.info("Determinizing")
    val sdfa = SFAUtils.determinizeI(snfaStream, exclusives, extras)
    SDFAUtils.checkForDead(sdfa)
    val sdfap = SDFAUtils.setPolicy(sdfa, policy)
    logger.debug("Building SPSA")
    val spsa = SPSAUtils.buildSPSA(sdfap, psa, iso)
    logger.debug("SPSA has " + spsa.getSize + " states")
    (spsa, formula._3)
  }
  */

  /**
    * Constructs an isomorphism (i.e. a mapping of sentences to symbols) appropriate for a SNFA. First constructs the
    * SDFA to find its minterms.
    *
    * @param snfa The given SNFA.
    * @param exclusives The set of exclusives.
    * @param extras The set of extra predicates.
    * @return The isomorphism.
    */
  def createIsomorphism(
                         snfa: SNFA,
                         exclusives: Set[Set[Predicate]],
                         extras: Set[Sentence]
                       ): Isomorphism = {
    val sentences = snfa.getSentences.filter(s => !s.isInstanceOf[EpsilonSentence] & !s.isTrue)
    val sentencesWithExtras = sentences ++ extras
    val minTerms = LogicUtils.buildMinTerms(sentencesWithExtras, exclusives)
    val iso = Isomorphism(minTerms)
    iso
  }

  /**
    * Constructs an isomorphism (i.e. a mapping of minterms to symbols) appropriate for a SDFA.
    *
    * @param sdfa The given SDFA.
    * @return The isomoprhism.
    */
  def createIsomorphism(sdfa: SDFA): Isomorphism = {
    val sentences = sdfa.getSentences
    val iso = Isomorphism(sentences)
    iso
  }

}

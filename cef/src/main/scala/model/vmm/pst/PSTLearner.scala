package model.vmm.pst

import com.typesafe.scalalogging.LazyLogging
import utils.MathUtils
import model.vmm.{Symbol, SymbolWord}
import ui.ConfigUtils

import scala.math.abs

object PSTLearner {
  /**
    * Constructor for PST learner, variant from DBLP:journals/jair/BegleiterEY04.
    *
    * @param symbols The symbols.
    * @param maxLength The maximum order of the PST.
    * @param pMin Symbol threshold.
    * @param alpha Conditional threshold = (1 + alpha) * gammaMin.
    * @param gammaMin Conditional threshold = (1 + alpha) * gammaMin.
    * @param r Likelihood ratio threshold.
    * @return The PST learner.
    */
  def apply(
             symbols: Set[Symbol],
             maxLength: Int,
             pMin: Double,
             alpha: Double,
             gammaMin: Double,
             r: Double
           ): PSTLearner = {
    val ConditionalThreshold = (1 + alpha) * gammaMin
    val LikelihoodRatioThreshold = r
    val SymbolThreshold = pMin
    new PSTLearner(symbols, maxLength, ConditionalThreshold, LikelihoodRatioThreshold, SymbolThreshold, gammaMin)
  }

  /**
    * Constructor for PST learner, original version from DBLP:journals/ml/RonST96.
    *
    * @param symbols The symbols.
    * @param maxLength The maximum order of the PST.
    * @param maxNoStates The maximum number of states the PSA (after the PST) is allowed to have.
    * @return The PST learner.
    */
  def apply(
             symbols: Set[Symbol],
             maxLength: Int,
             maxNoStates: Int
           ): PSTLearner = {
    val Epsilon: Double = ConfigUtils.epsilon
    val Epsilon2: Double = Epsilon / (48 * maxLength)
    val SymbolsNo: Int = symbols.size
    val gammaMin: Double = Epsilon2 / SymbolsNo
    val Epsilon0 = Epsilon / (2 * maxNoStates * maxLength * MathUtils.logbase(1 / gammaMin, 2))
    val Epsilon1 = Epsilon2 / (8 * maxNoStates * Epsilon0 * gammaMin)
    val ConditionalThreshold = (1 + Epsilon2) * gammaMin
    val LikelihoodRatioThreshold = 1 + 3 * Epsilon2
    val SymbolThreshold = abs((1 - Epsilon1) * Epsilon0) //TODO: should it be absolute value?
    new PSTLearner(symbols, maxLength, ConditionalThreshold, LikelihoodRatioThreshold, SymbolThreshold, gammaMin)
  }
}

/**
  * Learner for Prediction Suffix Trees.
  * See
  * @article{DBLP:journals/ml/RonST96,
  *                                   author    = {Dana Ron and
  *                                   Yoram Singer and
  *                                   Naftali Tishby},
  *                                   title     = {The Power of Amnesia: Learning Probabilistic Automata with Variable
  *                                   Memory Length},
  *                                   journal   = {Mach. Learn.},
  *                                   volume    = {25},
  *                                   number    = {2-3},
  *                                   pages     = {117--149},
  *                                   year      = {1996}
  *                                   }
  *
  * 1. Initialize T̄ and S̄:
  *   let T̄ consist of a single root node (corresponding to e),
  *   and let S̄ ← {σ | σ ∈ Σ and P̃ (σ) ≥ (1 − epsilon1)epsilon0 (SymbolThreshold) }.
  *
  * 2. While S̄ 6= ∅, pick any s ∈ S̄ and do:
  *   (A) Remove s from S̄;
  *   (B) If there exists a symbol σ ∈ Σ such that
  *         P̃ (σ|s) ≥ (1 + epsilon2)γmin (ConditionalThreshold) and
  *         P̃ (σ|s)/P̃ (σ|suffix (s)) > 1 + epsilon2 (LikelihoodRatioThreshold),
  *       then add to T̄ the node corresponding to s and all the nodes on the path from th deepest node in T̄ that is a
  *       suffix of s, to s;
  *   (C) If |s| < L then for every σ0 ∈ Σ, if P̃ (σ0 ·s) ≥ (1 − epsilon1 )epsilon0, then add σ 0 ·s to S̄.
  *
  * 3. Initialize T̂ to be T̄ .
  *
  * 4. Extend T̂ by adding all missing sons of internal nodes.
  *
  * 5. For each s labeling a node in T̂ , let γ̂s(σ) = P̃ (σ|s0 )(1 − |Σ|γmin) + γmin,
  *    where s0 is the longest suffix of s in T̄ .
  *
  * @param symbols The symbols.
  * @param maxLength The maximum order of the PST.
  * @param ConditionalThreshold P(σ|s) (for some symbol σ) must be greater than this for s to be added to the tree.
  * @param LikelihoodRatioThreshold P(σ|s) /P(σ|suffix(s)) (for some symbol σ) must be greater than this for s to be
  *                                 added to the tree.
  * @param SymbolThreshold P(σ) must be greater than this for σ to be added as a symbol to the tree.
  * @param gammaMin Distribution smoothing lowest value.
  */
class PSTLearner(
                  symbols: Set[Symbol],
                  maxLength: Int,
                  ConditionalThreshold: Double,
                  LikelihoodRatioThreshold: Double,
                  SymbolThreshold: Double,
                  gammaMin: Double
                ) extends LazyLogging {

  /**
    * Checks if a given symbol is frequent, i.e., its probability is above the given threshold.
    *
    * @param cst The counter suffix tree from which to find the symbol's probability.
    * @param symbol The given symbol.
    * @param pMin The threshold.
    * @return True if the given symbol is frequent.
    */
  private def isSymbolFrequent(
                                cst: CounterSuffixTree,
                                symbol: Symbol,
                                pMin: Double
                              ): Boolean = {
    val prob = cst.getProbFor(List(symbol))
    prob >= pMin
  }

  /**
    * Implementation of the learning algorithm with the hyper-parameters described in DBLP:journals/ml/RonST96.
    * Calls model.vmm.pst.PSTLearner#existsMeaningfulSymbolOriginal(model.vmm.pst.CounterSuffixTree, model.vmm.SymbolWord).
    *
    * @param cst The counter suffix tree from which to estimate required probabilities.
    * @param withMissing If true, step 4 (adding missing children) is executed.
    * @return The prediction suffix tree.
    */
  def learnOriginal(
                     cst: CounterSuffixTree,
                     withMissing: Boolean
                   ): PredictionSuffixTree = {
    val retainedSymbols = symbols.filter(s => isSymbolFrequent(cst, s, SymbolThreshold))
    val frontier: scala.collection.mutable.Set[SymbolWord] =
      scala.collection.mutable.Set(retainedSymbols.map(s => SymbolWord(List(s))).toSeq: _*)
    val root = PredictionSuffixTree(List.empty, cst.getSymbolDistributionFor(List.empty))
    while (frontier.nonEmpty) {
      val s = frontier.head
      frontier -= s
      if (existsMeaningfulSymbolOriginal(cst, s)) root.updateWithNewSuffix(s.word, cst)
      if (s.length < maxLength) {
        for (sigma <- symbols) {
          val suffixExtension = s.word ::: List(sigma)
          val probSuffixExtension = cst.getProbFor(suffixExtension)
          if (probSuffixExtension >= SymbolThreshold) frontier += SymbolWord(suffixExtension)
        }
      }
    }
    if (withMissing) root.addMissingChildren(symbols, root, cst)
    root.smoothDistributions(gammaMin, symbols)
    root
  }

  /**
    * Implementation of the learning algorithm with the hyper-parameters described in
    * @article{DBLP:journals/jair/BegleiterEY04,
    *                                           author    = {Ron Begleiter and
    *                                           Ran El{-}Yaniv and
    *                                           Golan Yona},
    *                                           title     = {On Prediction Using Variable Order Markov Models},
    *                                           journal   = {J. Artif. Intell. Res.},
    *                                           volume    = {22},
    *                                           pages     = {385--421},
    *                                           year      = {2004}
    *                                           }
    *
    * Calls model.vmm.pst.PSTLearner#existsMeaningfulSymbolVariant(model.vmm.pst.CounterSuffixTree, model.vmm.SymbolWord).
    *
    * Source code for implementation used to be here:
    * https://www.cs.technion.ac.il/~ronbeg/vmm/ (no longer works)
    *
    * @param cst The counter suffix tree from which to estimate required probabilities.
    * @param withMissing If true, step 4 (adding missing children) is executed.
    * @return The prediction suffix tree.
    */
  def learnVariant(
                    cst: CounterSuffixTree,
                    withMissing: Boolean
                  ): PredictionSuffixTree = {
    val pMin = SymbolThreshold
    val nextSymProbMin = gammaMin
    val alphabetSize = symbols.size
    require(nextSymProbMin * alphabetSize <= 1)

    val retainedSymbols = symbols.filter(s => isSymbolFrequent(cst, s, pMin))
    val root = PredictionSuffixTree(List.empty, cst.getSymbolDistributionFor(List.empty))
    val frontier: scala.collection.mutable.Set[SymbolWord] =
      scala.collection.mutable.Set(retainedSymbols.map(s => SymbolWord(List(s))).toSeq: _*)
    while (frontier.nonEmpty) {
      val s = frontier.head
      frontier -= s
      if (existsMeaningfulSymbolVariant(cst, s))
        root.updateWithNewSuffix(s.word, cst, withSmoothing = false, gammaMin, symbols)
      if (s.length < maxLength) {
        for (sigma <- symbols) {
          val suffixExtension = s.word ::: List(sigma)
          val probSuffixExtension = cst.getProbFor(suffixExtension)
          if (probSuffixExtension >= pMin) frontier += SymbolWord(suffixExtension)
        }
      }
    }
    if (withMissing) root.addMissingChildren(symbols, root, cst)
    root.smoothDistributions(gammaMin, symbols)
    root
  }

  /**
    * Checks if, for a given context/suffix, there exists a meaningful symbol.
    * Version from DBLP:journals/ml/RonST96.
    *
    * @param cst The counter suffix tree from which to estimate required probabilities.
    * @param context The given context.
    * @return True if there does exist such a symbol.
    */
  private def existsMeaningfulSymbolOriginal(
                                              cst: CounterSuffixTree,
                                              context: SymbolWord
                                            ): Boolean = symbols.exists(sigma => checkSymbolOriginal(cst, sigma, context))

  /**
    * Checks if, for a given context/suffix, the given symbol is meaningful.
    * Version from DBLP:journals/ml/RonST96.
    *
    * @param cst The counter suffix tree from which to estimate required probabilities.
    * @param sigma The given symbol.
    * @param context The given context.
    * @return True if there does exist such a symbol.
    */
  private def checkSymbolOriginal(
                                   cst: CounterSuffixTree,
                                   sigma: Symbol,
                                   context: SymbolWord
                                 ): Boolean = {
    val (probSigmaOnS, ratio): (Double, Double) = computeSigmaOnSANDRatio(cst, sigma, context)
    probSigmaOnS >= ConditionalThreshold & ratio > LikelihoodRatioThreshold
  }

  /**
    * Checks if, for a given context/suffix, there exists a meaningful symbol.
    * Version from DBLP:journals/jair/BegleiterEY04.
    *
    * @param cst The counter suffix tree from which to estimate required probabilities.
    * @param context The given context.
    * @return True if there does exist such a symbol.
    */
  private def existsMeaningfulSymbolVariant(
                                             cst: CounterSuffixTree,
                                             context: SymbolWord
                                           ): Boolean = {
    symbols.exists(sigma => checkSymbolVariant(cst,sigma,context))
    /*(for (sigma <- symbols) {
      if (checkSymbolVariant(cst, sigma, context)) {
        return true
      }
    }
    false*/
  }

  /**
    * Checks if, for a given context/suffix, the given symbol is meaningful.
    * Version from DBLP:journals/jair/BegleiterEY04.
    *
    * @param cst The counter suffix tree from which to estimate required probabilities.
    * @param sigma The given symbol.
    * @param context The given context.
    * @return True if there does exist such a symbol.
    */
  private def checkSymbolVariant(
                                  cst: CounterSuffixTree,
                                  sigma: Symbol,
                                  context: SymbolWord
                                ): Boolean = {
    val (probSigmaOnS, ratio): (Double, Double) = computeSigmaOnSANDRatio(cst, sigma, context)
    probSigmaOnS >= ConditionalThreshold & (ratio >= LikelihoodRatioThreshold | ratio <= (1 / LikelihoodRatioThreshold))
  }

  /**
    * Estimates P̃(σ|s) and P̃(σ|s) / P̃(σ|suffix(s)).
    *
    * @param cst The counter suffix tree from which to estimate required probabilities.
    * @param sigma The symbol σ.
    * @param context The context s.
    * @return P̃(σ|s) and P̃(σ|s) / P̃(σ|suffix(s)).
    */
  private def computeSigmaOnSANDRatio(
                                       cst: CounterSuffixTree,
                                       sigma: Symbol,
                                       context: SymbolWord
                                     ): (Double, Double) = {
    val probSigmaOnS = cst.getConditionalProbFor(sigma, context.word)
    val suffixS = context.getSuffix
    val probSigmaOnSuffixS = if (suffixS.isEmpty) cst.getProbFor(List(sigma)) else cst.getConditionalProbFor(sigma, suffixS.word)
    val ratio = probSigmaOnS / probSigmaOnSuffixS
    (probSigmaOnS, ratio)
  }

}

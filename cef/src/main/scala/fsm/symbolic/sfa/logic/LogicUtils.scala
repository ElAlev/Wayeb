package fsm.symbolic.sfa.logic

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sre.{BooleanOperator, LogicAtomicSentence, LogicComplexSentence, LogicSentence}
import ui.ConfigUtils
import utils.{Progressor, SetUtils}

object LogicUtils extends LazyLogging {

  /**
    * Converts a formula's logic sentence into an actual sentence to be used in an automaton.
    *
    * @param logicSentence The formula's logic sentence.
    * @return The actual sentence.
    */
  def parsed2ActualSentence(logicSentence: LogicSentence): Sentence = {
    logicSentence match {
      case x: LogicAtomicSentence => {
        val pc = PredicateConstructor
        val args = x.terms.map(t => t.toString)
        val pred = pc.getNewPredicateInstance(x.p.name, args)
        AtomicSentence(pred)
      }
      case x: LogicComplexSentence => {
        ComplexSentence(x.op, x.sentences.map(s => parsed2ActualSentence(s)))
      }
    }
  }

  /**
    * Creates a set of minterms from a set of sentences and a set of exclusives, according to the default method.
    *
    * @param originalSentences The set of original sentences, typically the set of sentences from the transitions of a
    *                          SNFA.
    * @param exclusivePredicates The set of exclusives.
    * @return The set of minterms.
    */
  def buildMinTerms(
                     originalSentences: Set[Sentence],
                     exclusivePredicates: Set[Set[Predicate]]
                   ): Set[Sentence] = {
    buildMinTerms(originalSentences, exclusivePredicates, ConfigUtils.defaultMinTermMethod)
  }

  /**
    * Creates a set of minterms from a set of sentences and a set of exclusives, according to the method provided.
    *
    * @param originalSentences The set of original sentences, typically the set of sentences from the transitions of a
    *                          SNFA.
    * @param exclusivePredicates The set of exclusives.
    * @param how If "withsat" the minterms are built with satisfiability checking. Could be slow.
    *            If "withoutsat", satisfiability checking is bypassed (faster) but may result in a larger set of
    *            produced minterms.
    * @return The set of minterms.
    */
  def buildMinTerms(
                     originalSentences: Set[Sentence],
                     exclusivePredicates: Set[Set[Predicate]],
                     how: String
                   ): Set[Sentence] = {
    how match {
      case "withsat" => buildMinTermsWithSat(originalSentences, exclusivePredicates)
      case "withoutsat" => buildMinTermsWithoutSat(originalSentences, exclusivePredicates)
      case _ => throw new Error("Method for building min-terms not valid.")
    }
  }

  /**
    * Builds minterms without satisfiability checking. Faster, but may result in a larger set of minterms.
    * The idea is the following: for sentences that do not belong to any of the exclusives and thus have no constraints,
    * we create all possible sub-minterms. E.g., if originalSentences = (P,Q,R,S) and exclusivePredicates = ((R,S)),
    * then for P, Q we create P AND Q, P AND NOT Q, NOT Q AND P, NOT P AND NOT Q. For every set of exclusives,
    * we create the positive literals and and the conjunction of the negated literals. E.g., we would create,
    * R, S and NOT R AND NOT S. We finally create the cartesian product of the set of non-exclusive minterms with the
    * sets built from the exclusives. E.g., the product of (P AND Q, P AND NOT Q, NOT Q AND P, NOT P AND NOT Q) with
    * (R, S, NOT R AND NOT S). This may result in redundnat minterms being created. For example, if we had (Q,R) as
    * another set of exclusives, R would be created twice.
    *
    * @param originalSentences The set of original sentences.
    * @param exclusivePredicates The set of exclusives.
    * @return The set of minterms.
    */
  private def buildMinTermsWithoutSat(
                                       originalSentences: Set[Sentence],
                                       exclusivePredicates: Set[Set[Predicate]]
                                     ): Set[Sentence] = {
    val exclusivesAsSentences = exclusivePredicates.map(ex => ex.map(p => AtomicSentence(p).asInstanceOf[Sentence]))
    val exclusivesAsSentencesFlattened = exclusivesAsSentences.flatten
    //require(exclusivesAsSentencesFlattened.subsetOf(originalSentences))
    val originalNotExclusive = originalSentences &~ exclusivesAsSentencesFlattened
    // let's first construct the sub-minterms from sentences that do not belong to any set of exclusives
    val originalNotExclusiveMinTerms = buildMinTermsAsConcjuncts(originalNotExclusive.toList, Set.empty[Sentence])
    // now, for every set of exclusives, construct the corresponding sub-minterms
    val exclusivesMinTerms = exclusivesAsSentences.map(e => buildMinTermsForExclusives(e))
    // combine the set of non-exclusive sub-minterms (if any) with the sets of exclusive sub-minterms,
    // by fist taking the cartesian product
    val cartesian = if (originalNotExclusiveMinTerms.nonEmpty) SetUtils.cartesian[Sentence](exclusivesMinTerms + originalNotExclusiveMinTerms)
    else SetUtils.cartesian[Sentence](exclusivesMinTerms)
    // and then connecting each member of this product with an AND operator
    val minTerms = cartesian.map(c => connectWithAND(c))
    minTerms
  }

  /**
    * Connects a set of sentences with an AND operator.
    *
    * @param sentences The sentences to be connected.
    * @return The AND sentence, connecting all given sentences.
    */
  def connectWithAND(sentences: Set[Sentence]): Sentence = {
    val sentencesList = sentences.toList
    connectWithANDAux(sentencesList.tail, sentencesList.head)
  }

  /**
    * Helper recursive function to connect sentences with AND.
    *
    * @param sentences The (remaining) sentences to be connected.
    * @param connected The connected sentence thus far.
    * @return
    */
  @scala.annotation.tailrec
  private def connectWithANDAux(
                                 sentences: List[Sentence],
                                 connected: Sentence
                               ): Sentence = {
    sentences match {
      case Nil => connected
      case head :: tail => connectWithANDAux(tail, ComplexSentence(BooleanOperator.AND, List(connected, head)))
    }
  }

  /**
    * Constructs all possible minterms from a set of sentences, without any simplifications.
    * E.g., if remainingSentences = (P,Q) then we create P AND Q, P AND NOT Q, NOT Q AND P, NOT P AND NOT Q.
    * Works recursively. Start with all sentences, take the first one, add this and its negation as minterms,
    * then take the second sentence, append this and its negations to the already existing minterms, etc.
    *
    * @param remainingSentences The (remaining) sentences from which to create new conjuncts for the minterms (in the
    *                           first call, should contain all sentences).
    * @param currentMinTerms The minterms constructed thus far (in the first call, should be empty).
    * @return The minterms, without any simplifications.
    */
  @scala.annotation.tailrec
  private def buildMinTermsAsConcjuncts(
                                         remainingSentences: List[Sentence],
                                         currentMinTerms: Set[Sentence]
                                       ): Set[Sentence] = {
    remainingSentences match {
      case Nil => currentMinTerms
      case head :: tail => {
        var newMinTerms = Set.empty[Sentence]
        if (currentMinTerms.isEmpty) {
          val nmt1 = head
          val nmt2 = ComplexSentence(BooleanOperator.NOT, List(head))
          newMinTerms = newMinTerms ++ Set(nmt1, nmt2)
        } else {
          for (mt <- currentMinTerms) {
            val nmt1 = ComplexSentence(BooleanOperator.AND, List(mt, head))
            val nmt2 = ComplexSentence(BooleanOperator.AND, List(mt, ComplexSentence(BooleanOperator.NOT, List(head))))
            newMinTerms = newMinTerms ++ Set(nmt1, nmt2)
          }
        }
        buildMinTermsAsConcjuncts(tail, newMinTerms)
      }
    }
  }

  /**
    * Builds minterms from a set of exclusives. E.g., if sentences=(R,S), then we create R, S and NOT R AND NOT S,
    * i.e., the sentences themselves and then the conjunction of their negations.
    *
    * @param sentences The original sentences.
    * @return The minterms.
    */
  private def buildMinTermsForExclusives(sentences: Set[Sentence]): Set[Sentence] = {
    sentences + createConjunctOfNegations(sentences)
  }

  /**
    * Constructs the conjunction of the negations of the given sentences.
    *
    * @param sentences The given sentences.
    * @return The conjunction of the negated given sentences.
    */
  private def createConjunctOfNegations(sentences: Set[Sentence]): Sentence = {
    require(sentences.nonEmpty)
    val sentencesList = sentences.toList
    val initConjunct = ComplexSentence(BooleanOperator.NOT, List(sentencesList.head))
    createConjunctOfNegationsAux(sentencesList.tail, initConjunct)
  }

  /**
    * Helper, recursive function to construct the conjunction of negated sentences. We incrementally negate each given
    * sentence and add the negation as conjunct.
    *
    * @param sentences The original sentences.
    * @param currentConjunct The constructed conjunct thus far.
    * @return The final conjunct.
    */
  @scala.annotation.tailrec
  private def createConjunctOfNegationsAux(
                                            sentences: List[Sentence],
                                            currentConjunct: Sentence
                                          ): Sentence = {
    sentences match {
      case Nil => currentConjunct
      case head :: tail => {
        val newConjunct = ComplexSentence(BooleanOperator.AND, List(currentConjunct, ComplexSentence(BooleanOperator.NOT, List(head))))
        createConjunctOfNegationsAux(tail, newConjunct)
      }
    }
  }

  /**
    * Given a set of sentences and a set of exclusives, creates a (possibly simplified) set of min-terms.
    * The creation of min-terms is done incrementally. We first pick one of the original sentences and create its
    * positive and negative version as candidate conjuncts. We then try to add new conjuncts by appending the positive
    * and negatives version of a second original sentence and then of a third etc. At each step, we try to simplify the
    * intermediate min-terms. For example, if P is one the current min-terms and we try to add Q as a conjunct but (P,Q)
    * are exclusive, we will discard (P AND Q). (P AND Q) is discarded for ever and we do not need to
    * keep it an intermediate min-term since it will always lead to unsatisfiable min-terms regardless of what other
    * conjunct we might to append. For example, (P AND Q AND R) will again be unsatisfiable.
    *
    * @param originalSentences The set of original sentences.
    * @param exclusivePredicates The set of exclusives.
    * @return The set of (possibly simplified) min-terms.
    */
  private def buildMinTermsWithSat(
                                    originalSentences: Set[Sentence],
                                    exclusivePredicates: Set[Set[Predicate]]
                                  ): Set[Sentence] = {
    val assProd = new AssignmentProducer(exclusivePredicates)
    var minTerms = scala.collection.mutable.Set.empty[(Sentence, List[Assignment])]
    val slit = originalSentences.iterator
    //val progressor = Progressor("Min-terms", originalSentences.size)
    while (slit.hasNext) {
      val newMinTerms = scala.collection.mutable.Set.empty[(Sentence, List[Assignment])]
      // let's start with the sentence itself
      val candidateConjunct = slit.next()
      // and then create its negation
      val candidateConjunctNeg = ComplexSentence(BooleanOperator.NOT, List(candidateConjunct))
      if (minTerms.isEmpty) {
        if (exclusivePredicates.isEmpty) {
          // without any exclusives, we just add the sentence and its negation to the minterms
          // CAUTION: if no exclusives, empty list of assignments means satisfiable for all assignments
          newMinTerms += Tuple2(candidateConjunct, List.empty)
          newMinTerms += Tuple2(candidateConjunctNeg, List.empty)
        } else {
          // if exclusives exist, we add the sentence and its negation, but we must also find the assignments for which
          // they are satisfiable
          val ccass = candidateConjunct.isSatisfiableFor(List.empty[Assignment], assProd)
          newMinTerms += Tuple2(candidateConjunct, ccass)
          val ccnass = candidateConjunctNeg.isSatisfiableFor(List.empty[Assignment], assProd)
          newMinTerms += Tuple2(candidateConjunctNeg, ccnass)
        }
      } else {
        // if there are already existing minterms
        val mtit = minTerms.iterator
        while (mtit.hasNext) {
          val minTerm = mtit.next()
          val check = simplifyCandidate(minTerm, candidateConjunct, exclusivePredicates, assProd)
          if (check._1) newMinTerms += Tuple2(check._2, check._3)
          val checkNeg = simplifyCandidate(minTerm, candidateConjunctNeg, exclusivePredicates, assProd)
          if (checkNeg._1) newMinTerms += Tuple2(checkNeg._2, checkNeg._3)
        }
      }
      minTerms = newMinTerms
      //logger.whenDebugEnabled {
      //progressor.tick
      //}
    }
    minTerms.map(mt => mt._1).toSet
  }

  /**
    * Assuming we have an already existing minterm (e.g. P) with its valid assignments and we are given a new conjunct
    * to add (e.g. Q), we try to create a new minterm (e.g. P AND Q). We first check whether this new minterm is valid
    * (i.e., satisfiable according to the exclusives) and, if yes, we then try to simplify it. E.g., if minterm=P,
    * conjunct = NOT Q and P and Q are mutually exclusive, then P AND NOT Q can be simplified to just P.
    *
    * @param minTerm The already existing minterm, along with its valid assignments.
    * @param conjunct The new conjunct to append.
    * @param exclusivePredicates The set of exclusives.
    * @param assignmentProducer An assignment producer that must have been constructed according to the exclusives.
    * @return If the new minterm is valid, then return (true, its simplified form, its list of valid assignments).
    *         Otherwise, return (false, rest does not matter).
    */
  private def simplifyCandidate(
                                 minTerm: (Sentence, List[Assignment]),
                                 conjunct: Sentence,
                                 exclusivePredicates: Set[Set[Predicate]],
                                 assignmentProducer: AssignmentProducer
                               ): (Boolean, Sentence, List[Assignment]) = {
    // first create a candidate new term by appending a new conjunct,
    // e.g., if P is already a minterm and Q the new conjunct, we create P AND Q
    val candidateTerm = ComplexSentence(BooleanOperator.AND, List(minTerm._1, conjunct))
    if (exclusivePredicates.isEmpty) {
      // if there are no exclusives, there is nothing to be simplified
      // CAUTION: if no exclusives, empty list of assignments means satisfiable for all assignments
      (true, candidateTerm, List.empty)
    } else {
      // if there are exclusives,
      // we must check that the new candidate term is satisfiable for the valid assignment of the minterm,
      // e.g., we check that P AND Q is satisfiable for the valid assignments of P
      // NOTE: We do not need to check all assignments, but only those that we already know they are valid, since the
      // old minterm (e.g., P) must still be satisfiable in P and Q. As we add more conjuncts, the set of valid
      // assignments can only shrink (or remain the same).
      val satAss = candidateTerm.isSatisfiableFor(minTerm._2, assignmentProducer)
      if (satAss.nonEmpty) {
        // if there are still valid assignments for the new term (e.g., P AND Q), we can then try to simplify it
        val simplified = simplify(candidateTerm, exclusivePredicates)
        // we return true (i.e., this is a valid, satisfiable new term), the term in its (possibly) simplified form and
        // the assignment for which it is satisfiable
        (true, simplified, satAss)
      } else {
        // if there are exclusives but no valid assignments, this means the minterm is also not valid and we return
        // false (rest of returned objects do not really matter in this case)
        (false, candidateTerm, satAss)
      }
    }
  }

  /**
    * Simplifies an AND sentence by discarding redundant conjuncts, i.e., conjuncts entailed by other conjuncts.
    *
    * @param sentence The original sentence.
    * @param exclusives The set of exclusives based on which the simplification proceeds.
    * @return The (possibly) simplified sentence.
    */
  private def simplify(
                        sentence: Sentence,
                        exclusives: Set[Set[Predicate]]
                      ): Sentence = {
    sentence match {
      case x: AtomicSentence => x // nothing to simplify here
      case x: ComplexSentence => {
        if ((x.op == BooleanOperator.OR) | (x.op == BooleanOperator.NOT)) {
          // for OR and NOT operators, we do nothing
          //TODO: maybe try to simplify those as well, but does not seem to make much sense, given that the only
          // constraints we currently have are exclusives (do not really help in simplifying OR sentences) and the
          // function is used to simplify minterms constructed with AND operators
          sentence
        } else {
          // we keep only those sentences that are not entailed by any other sentence
          val remainder = x.sentences.filter(s => !x.sentences.exists(s1 => s1.entailsStrict(s, exclusives)))
          if (remainder.isEmpty) throw new Error("Oversimplification")
          else if (remainder.size == 1) remainder.head
          else ComplexSentence(BooleanOperator.AND, remainder)
        }
      }
    }
  }

  /*
  def buildMinTermsOld(
      originalSentences: Set[Sentence],
      exclusivePredicates: Set[Set[Predicate]]): Set[Sentence] = {
    logger.debug("Creating assignments...")
    val assProd = new AssignmentProducer(exclusivePredicates)
    logger.debug("Creating permutations...")
    val boolPerm = new BooleanPermutator(originalSentences.size)
    val truthPerms = boolPerm.getPermutations(originalSentences.size)
    //val truthPerms = SingletonBooleanPermutator.getPermutations(originalSentences.size)
    val osl = originalSentences.toList
    logger.debug("Creating all min-terms from " + truthPerms.size + " permutations...")
    val allMinTerms = truthPerms.map(tp => buildMinTerm(osl, tp))
    logger.debug("Keeping satisfiable from a total of " + allMinTerms.size + " min-terms...")
    //val satisfiable = allMinTerms.filter(s => s.isSatisfiable(exclusivePredicates))
    val it = allMinTerms.iterator
    val satisfiable = scala.collection.mutable.Set.empty[Sentence]
    var counter = 0
    var progress = 0
    while (it.hasNext) {
      val mt = it.next()
      if (mt.isSatisfiable(exclusivePredicates, assProd)) satisfiable += mt
      counter += 1
      progress = utils.MiscUtils.updateProgress(counter, allMinTerms.size, progress)
    }
    logger.debug("Simplifying remaining " + satisfiable.size + " satisfiable min-terms...")
    val simplified = satisfiable.map(s => simplify(s, exclusivePredicates))
    simplified.toSet
  }

  private def buildMinTerm(
                            sentences: List[Sentence],
                            posneg: List[Boolean]): Sentence = {
    require(sentences.size == posneg.size)
    val sentencesZposneg = sentences.zip(posneg)
    val literals = sentencesZposneg.map(spn => if (spn._2) spn._1 else ComplexSentence(BooleanOperator.NOT, List(spn._1)))
    if (literals.size == 1) literals.head
    else ComplexSentence(BooleanOperator.AND, literals)
  }
  */

}


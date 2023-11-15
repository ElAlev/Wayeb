package fsm.symbolic.logic

import fsm.symbolic.Valuation
import stream.GenericEvent

/**
  * An abstract class for logic sentences.
  * Can be either an atomic sentence fsm.symbolic.sfa.logic.AtomicSentence (just a predicate)
  * or a complex sentence fsm.symbolic.sfa.logic.ComplexSentence (predicates combined with Boolean operators)/
  * Sentences are constructed from the "terminal symbols"/Boolean expressions of SRE.
  */
abstract class Sentence {
  /**
    * Evaluates the sentence against an event and a valuation (i.e., register contents),
   *  according to its predicates and operators.
    * @param event The given event.
   *  @param valuation The given valuation
    * @return True if the sentence evaluates to true for the given event and valuation.
    */
  def evaluate(
                event: GenericEvent,
                valuation: Valuation
              ): Boolean

  /**
   * Evaluates the sentence against an event, according to its predicates and operators.
   * Valuation is assumed to be empty.
   *
   * @param event The given event.
   * @return True if the sentence evaluates to true for the given event.
   */
  def evaluate(event: GenericEvent): Boolean = evaluate(event, Valuation())

  /**
    * Evaluates a sentence according to an assignment of truth values to its predicates.
    * Uses the Boolean operators and the assignments to derive the final truth value.
    * @param assignment The given assignment.
    * @return True if the sentence evaluates to true.
    */
  def evaluate(assignment: Assignment): Boolean

  /**
    * Retrieves the truth table of the sentence. The truth table is a map structure that holds, for every possible
    * assignment of truth values to the sentence's predicates, the final truth value of the sentence itself.
    * @return The sentence's truth table.
    */
  def getTruthTable: TruthTable

  /**
    * Extracts the predicates contained in the sentence.
    * @return A set of predicates contained in the sentence.
    */
  def extractPredicates: Set[Predicate]

  /**
    * Extracts the predicates contained in the sentence as strings.
    * @return A set of predicates contained in the sentence as strings.
    */
  def extractPredicateSymbols: Set[String]

  /**
   * If sentence references any registers (as arguments), these registers are returned.
   * @return The register selection, i.e., the set of all referenced registers.
   */
  def getRegisterSelection: Set[String]

  /**
    * Determines whether the sentence is satisfiable according to the constraints of the exclusives.
    * @param exclusive The exclusives.
    * @param assignmentProducer An object holding all valid (according to the exclusives) assignments.
    *                           This object must have been created with the same exclusives.
    *                           We pass such an object in order to avoid recomputing the valid assignments every time
    *                           we have to call this method.
    * @return True if the sentence is satisfiable, given the exclusives.
    */
  def isSatisfiable(
                     exclusive: Set[Set[Predicate]],
                     assignmentProducer: AssignmentProducer
                   ): Boolean

  /**
    * Determines whether the sentence is satisfiable according to an assignment. The assignment is assumed to have been
    * created by a set of exclusives.
    * @param exAss The given assignment.
    * @return True if the sentence is satisfiable.
    */
  def isSatisfiableAux(exAss: Assignment): Boolean

  /**
    * Checks if the sentence has a True predicate, i.e., a predicate that always evaluates to true.
    * @return True if the sentence does indeed have a True predicate.
    */
  def isTrue: Boolean

  /**
    * Checks the type of the sentence.
    * @return ATOMIC if it is an atomic sentence, AND if it has an AND operator, OR if it has an OR operator and NOT if
    *         it has a NOT operator.
    */
  def isWhat: String

  /**
    * Returns all the the subsentences by removing all Boolean operators.
    * @return A list of all subsentences.
    */
  def getSubStentences: List[Sentence]

  /**
    * Constructs the truth table of this sentence according to a list of predicates.
    * @param predicates The list of given predicates.
    * @return The truth table.
    */
  def getTruthTable(predicates: List[Predicate]): TruthTable = getTruthTable(predicates, Set.empty[Set[Predicate]])

  /**
    * Constructs the truth table of this sentence according to a list of predicates and a set of exclusives.
    * First creates all valid (not violating exclusives) assignments of truth values of predicates and then evaluates
    * the sentence against every such assignment.
    * @param predicates The list of given predicates. Not enough to use the predicates of this sentence only. We might
    *                   also need to take into account predicates from another sentence.
    *                   See fsm.symbolic.sfa.logic.Sentence#entails(fsm.symbolic.sfa.logic.Sentence)
    * @param exclusives The sets of mutually exclusive predicates.
    * @return The truth table.
    */
  private def getTruthTable(
                             predicates: List[Predicate],
                             exclusives: Set[Set[Predicate]]
                           ): TruthTable = {
    val assignments = buildAssignments(predicates, exclusives)
    val ttm = assignments.map(a => (a, evaluate(a))).toMap
    val tt = TruthTable(ttm)
    tt
  }

  def entails(otherSentence: Sentence): Boolean = entails(otherSentence, Set.empty[Set[Predicate]])

  /**
    * Checks whether this sentence "entails" another sentence and that the other sentence does not entail this.
    * @param otherSentence The other sentence to check.
    * @param exclusives The sets of exclusive predicates.
    * @return True if this sentence entails the given and the given does not entail this.
    */
  def entailsStrict(
                     otherSentence: Sentence,
                     exclusives: Set[Set[Predicate]]
                   ): Boolean = {
    entails(otherSentence, exclusives) & !otherSentence.entails(this, exclusives)
  }

  /**
    * Checks whether this sentence "entails" another sentence, i.e., whenever (for each assignment) this sentence
    * evaluates to true, the other sentence must also evaluate to true. Done through the truth tables.
    * @param otherSentence The other sentence that could be entailed by this one.
    * @param exclusive The sets of exclusive predicates.
    * @return True if this sentence entails the given.
    */
  def entails(
               otherSentence: Sentence,
               exclusive: Set[Set[Predicate]]
             ): Boolean = {
    if (otherSentence.isTrue) true
    else {
      val allPredicates = (this.extractPredicates ++ otherSentence.extractPredicates).toList
      val thisTruthTable = this.getTruthTable(allPredicates, exclusive)
      val otherTruthTable = otherSentence.getTruthTable(allPredicates, exclusive)
      thisTruthTable.entails(otherTruthTable)
    }
  }

  /**
    * Given a set of exclusives, find those assignments for which the sentence is satisfiable. With an empty second
    * argument, first create all possible assignments for the exclusive predicates and test them. If the second argument
    * is non-empty, then we check only for those assignments. i.e., we do not care about other assignments. This usually
    * means that these other assignments have already been tested and filtered out in previous steps.
    *
    * @param assignments A (possibly empty) list of assignments to be checked. If empty, all assignments will be created
    *                    and tested.
    * @param assignmentProducer Holds all valid assignments of the exclusives if exclusives set non-empty.
    * @return A list of assignments for which the sentence is satisfiable given the exclusives.
    */
  def isSatisfiableFor(
                        assignments: List[Assignment],
                        assignmentProducer: AssignmentProducer
                      ): List[Assignment] = {
    if (assignments.isEmpty) {
      val exAssigns = assignmentProducer.getAssignments
      exAssigns.filter(ass => isSatisfiableAux(ass))
    } else {
      assignments.filter(ass => isSatisfiableAux(ass))
    }
  }

  /**
    * From a list of predicates, creates all possible assignments, i.e., assignments of truth values to the predicates.
    * Simply create all permutations of true/false of length equal to the number of predicates.
    * @param predicates The list of given predicates.
    * @return A set of assignments constructed from the list of predicates.
    */
  private def buildAssignments(predicates: List[Predicate]): Set[Assignment] = {
    val allPredicates = (extractPredicates ++ predicates.toSet).toList.sortBy(p => p.toString)
    val boolPerm = new BooleanPermutator(allPredicates.size)
    val truthPerms = boolPerm.getPermutations(allPredicates.size)
    //val truthPerms = SingletonBooleanPermutator.getPermutations(allPredicates.size)
    val assignments = truthPerms.map(perm => allPredicates.zip(perm)).map(x => Assignment(x))
    assignments
  }

  /**
    * From a list of predicates, creates all possible assignments that are valid, i.e., that do not violate the
    * constraints imposed by the exclusives.
    * @param predicates The list of given predicates.
    * @param exclusives The sets of mutually exclusive predicates.
    * @return A set of valid assignments constructed from the list of predicates and not violating the exclusives.
    */
  private def buildAssignments(
                                predicates: List[Predicate],
                                exclusives: Set[Set[Predicate]]
                              ): Set[Assignment] = {
    val allAssignments = buildAssignments(predicates)
    val validAssignments = allAssignments.filter(a => a.isValidSet(exclusives))
    validAssignments
  }
}

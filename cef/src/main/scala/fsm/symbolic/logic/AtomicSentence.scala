package fsm.symbolic.logic

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.Valuation
import fsm.symbolic.logic.predicates.TruePredicate
import stream.GenericEvent

object AtomicSentence {
  def apply(p: Predicate): AtomicSentence = AtomicSentence(p, Set.empty[String])

  def apply(
             p: Predicate,
             registerSelection: Set[String]
           ): AtomicSentence = new AtomicSentence(p, registerSelection)
}

case class AtomicSentence(
                           p: Predicate,
                           registerSelection: Set[String]
                         ) extends Sentence with LazyLogging {
  lazy val extractedPredicates: Set[Predicate] = Set(p)

  /**
    * Simply use the predicate to evaluate an atomic sentence.
    * @param event The given event.
    * @param valuation The given valuation.
    * @return True if the sentence evaluates to true for the given event and valuation.
    */
  override def evaluate(
                         event: GenericEvent,
                         valuation: Valuation
                       ): Boolean = p.evaluate(event, valuation)

  /**
    * Check if the assignment contains the predicate and then retrieve the truth value assigned to the predicate.
    * @param assignment The given assignment.
    * @return True if the sentence evaluates to true.
    */
  override def evaluate(assignment: Assignment): Boolean = {
    require(assignment.contains(p))
    val result = {
      if (p.isInstanceOf[TruePredicate]) true
      else assignment.getValueOf(p)
    }
    //val result = assignment.getValueOf(p)
    result
  }

  /**
    * The truth value of an atomic sentence contains two rows, one for which the predicate is false and the sentence's
    * value is also false, and one for which the predicate is true and the sentence also true.
    * @return The sentence's truth table.
    */
  override def getTruthTable: TruthTable = {
    val tass = Assignment(List((p, true)))
    val fass = Assignment(List((p, false)))
    val tt = TruthTable(Map(tass -> true, fass -> false))
    tt
  }

  /**
    * Just return the singleton set containing the sentence's predicate.
    * @return A set of predicates contained in the sentence.
    */
  override def extractPredicates: Set[Predicate] = extractedPredicates

  /**
    * Return the name of the class.
    * @return A set of predicates contained in the sentence as strings.
    */
  override def extractPredicateSymbols: Set[String] = Set(p.getClass.getName)

  /**
   *  @return The register selection, i.e., the set of all referenced registers.
   */
  override def getRegisterSelection: Set[String] = registerSelection

  /**
    * Since we do not have constraints that forbid a single predicate from evaluating to true, an atomic sentence is
    * always satisfiable.
    * @param exclusive The exclusives.
    * @param assignmentProducer An object holding all valid (according to the exclusives) assignments.
    *                           This object must have been created with the same exclusives.
    *                           We pass such an object in order to avoid recomputing the valid assignments every time
    *                           we have to call this method.
    * @return True if the sentence is satisfiable, given the exclusives.
    */
  override def isSatisfiable(
      exclusive: Set[Set[Predicate]],
      assignmentProducer: AssignmentProducer
  ): Boolean = true

  /**
    * Determines whether the atomic sentence is satisfiable according to a given assignment.
    * @param exAss The given assignment.
    * @return True if the sentence is satisfiable.
    */
  override def isSatisfiableAux(exAss: Assignment): Boolean = isSatisfiableAux(exAss.assign)

  /**
    * Determines whether the atomic sentence is satisfiable according to the truth values assigned to predicates.
    * Either predicate is not in the list (indifferent, could be true/false) and therefore it is satisfiable or is in
    * the list and its value is true.
    * @param exclusiveWithValues A list of predicates with their Boolean values.
    * @return True if the sentence is satisfiable.
    */
  def isSatisfiableAux(exclusiveWithValues: List[(Predicate, Boolean)]): Boolean = {
    logger.whenDebugEnabled {
      // make sure each predicate is contained only once
      require(exclusiveWithValues.map(x => x._1).toSet.size == exclusiveWithValues.size)
    }
    exclusiveWithValues match {
      case Nil => true
      case head :: tail => if (head._1 == p) head._2 else isSatisfiableAux(tail)
    }
  }

  /**
    *
    * @return True if the sentence does indeed have a True predicate.
    */
  override def isTrue: Boolean = p.isInstanceOf[TruePredicate]

  /**
    *
    * @return ATOMIC since it is an atomic sentence.
    */
  override def isWhat: String = "ATOMIC"

  /**
    * Just return the predicate.
    * @return A list of all subsentences.
    */
  override def getSubStentences: List[Sentence] = List(this)

  override def toString: String = p.toString

}


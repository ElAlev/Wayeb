package fsm.symbolic.sfa.logic

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.sre.BooleanOperator.{AND, BooleanOperator, NOT, OR}
import stream.GenericEvent
import utils.StringUtils.list2Str

case class ComplexSentence(
                            op: BooleanOperator,
                            inputSentences: List[Sentence]
                          ) extends Sentence with LazyLogging {
  //val sentences = inputSentences
  val sentences: List[Sentence] = flatten
  require((op == NOT & sentences.size == 1) | (sentences.size > 1), "NOT must have only a single operand. AND and OR at least two.")

  /**
    * Removes all Boolean operators from the sentence and returns all subsentences as a list.
    * @return A list of all the subsentences.
    */
  private def flatten: List[Sentence] = {
    op match {
      case AND => {
        val partSentences = inputSentences.partition(s => s.isWhat == "AND")
        val andSentences = partSentences._1.flatMap(s => s.getSubStentences)
        andSentences ::: partSentences._2
      }
      case OR => {
        val partSentences = inputSentences.partition(s => s.isWhat == "OR")
        val orSentences = partSentences._1.flatMap(s => s.getSubStentences)
        orSentences ::: partSentences._2
      }
      case NOT => inputSentences //TODO: should we call getSubsentences on inputSentences?
    }
  }

  lazy val extractedPredicates: Set[Predicate] = sentences.foldLeft(Set.empty[Predicate]) { (acc, s) => acc ++ s.extractPredicates }

  /**
    * Evaluates the sentence according to its operators.
    * @param event The given event.
    * @return True if the sentence evaluates to true for the event.
    */
  override def evaluate(event: GenericEvent): Boolean = op match {
    case NOT => !sentences.head.evaluate(event)
    case AND => sentences.forall(s => s.evaluate(event))
    case OR => sentences.exists(s => s.evaluate(event))
  }

  /**
    * Evaluates the sentence according to its operators.
    * @param assignment The given assignment.
    * @return True if the sentence evaluates to true.
    */
  override def evaluate(assignment: Assignment): Boolean = {
    val preds = extractPredicates
    require(preds.forall(pred => assignment.contains(pred)))
    op match {
      case NOT => !sentences.head.evaluate(assignment)
      case AND => sentences.forall(s => s.evaluate(assignment))
      case OR => sentences.exists(s => s.evaluate(assignment))
    }
  }

  /**
    * First extract all predicates from the sentence and use these to construct the truth table.
    * @return The sentence's truth table.
    */
  override def getTruthTable: TruthTable = getTruthTable(extractPredicates.toList)

  /**
    * Finds all predicates from the subsentences.
    * @return A set of predicates contained in the sentence.
    */
  override def extractPredicates: Set[Predicate] = extractedPredicates

  /**
    * Get the names of the classes of the predicates.
    * @return A set of predicates contained in the sentence as strings.
    */
  override def extractPredicateSymbols: Set[String] = extractPredicates.map(p => p.getClass.getName)

  /**
    * If the exclusives is empty, then the sentence is satisfiable.
    * Otherwise, we need to check every assignment.
    * @param exclusives
    * @param assignmentProducer An object holding all valid (according to the exclusives) assignments.
    *                           This object must have been created with the same exclusives.
    *                           We pass such an object in order to avoid recomputing the valid assignments every time
    *                           we have to call this method.
    * @return True if the sentence is satisfiable, given the exclusives.
    */
  override def isSatisfiable(
                              exclusives: Set[Set[Predicate]],
                              assignmentProducer: AssignmentProducer
                            ): Boolean = {
    if (exclusives.isEmpty) true
    else {
      //val exAssigns = SingletonAssignmentProducer.getAssignments(exclusives)
      val exAssigns = assignmentProducer.getAssignments
      exAssigns.exists(ass => isSatisfiableAux(ass))
    }
  }

  /**
    * Determines whether the complex sentence is satisfiable according to a given assignment.
    * If the sentence operator is AND, then all subsentences must be satisfiable.
    * If the sentence operator is OR, then at least one subsentence must be satisfiable.
    * If the sentence operator is NOT, then we don't really care. The only way for the sentence to be unsatisfiable
    * would be for the subsentence to be always satisfied. Too complex to check right now. Just return true.
    * @param exAss The given assignment.
    * @return True if the sentence is satisfiable.
    */
  override def isSatisfiableAux(exAss: Assignment): Boolean = {
    logger.whenDebugEnabled {
      val exclusiveWithValues = exAss.assign
      // make sure each predicate is contained only once
      require(exclusiveWithValues.map(x => x._1).toSet.size == exclusiveWithValues.size)
    }
    op match {
      case AND => sentences.forall(s => s.isSatisfiableAux(exAss))
      case OR => sentences.exists(s => s.isSatisfiableAux(exAss))
      case NOT => true //TODO: can tolerate but not correct
    }
  }

  /**
    *
    * @return Always false, since, by definition, a complex sentence cannot have a single True predicate.
    */
  override def isTrue: Boolean = false

  /**
    *
    * @return AND if it has an AND operator, OR if it has an OR operator and NOT if it has a NOT operator.
    */
  override def isWhat: String = {
    op match {
      case AND => "AND"
      case OR => "OR"
      case NOT => "NOT"
    }
  }

  /**
    * See fsm.symbolic.sfa.logic.ComplexSentence#flatten().
    * @return A list of all subsentences.
    */
  override def getSubStentences: List[Sentence] = sentences

  override def toString: String = op.toString + "(" + list2Str(sentences, ",") + ")"
}

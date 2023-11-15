package fsm.symbolic.sre

import fsm.symbolic.Valuation
import fsm.symbolic.logic.SentenceConstructor
import fsm.symbolic.sre.RegularOperator._
import stream.GenericEvent
import utils.StringUtils.list2Str

/**
  * Classes to build formulas with fsm.symbolic.sre.SREParser.
  */

abstract class SREFormula {
  private var marked: Boolean = true

  /**
    * @return All sentences contained in the formula.
    */
  def getSentences: Set[LogicSentence]

  /**
   * @return All register variables declared in the formula.
   */
  def getRegisterVariables: Set[RegisterVariable]

  /**
   * @return All variables used as arguments in logic sentences contained in the formula.
   */
  def getVariableArguments: Set[String]

  /**
    * A function that tests whether a given string (list of events) is a member of the expression's language.
    * For SREM, a valuation must also be passed as an argument. Also returns a new valuation (for SREM)
    *
    * @param events The string to be tested.
    * @param valuation The given valuation (register contents) from which it is assumed that we start the evaluation.
    * @return TRUE if the string is member of the expression's language, FALSE otherwise. If TRUE, a set of valid
    *         (non-empty) updated valuations is also returned. If FALSE, an empty set of valuations is returned
    *         (i.e., no valid valuation has been found).
    */
  def accepts(
               events: List[GenericEvent],
               valuation: Valuation
             ): (Boolean, Set[Valuation])

  /**
   * Version of fsm.symbolic.sre.SREFormula#accepts(scala.collection.immutable.List, fsm.symbolic.Valuation) where
   * the valuation is assumed to be empty (no registers). Useful for non-SREM expressions, which do not make any use
   * of registers.
   *
   * @param events The string to be tested.
   * @return TRUE if the string is member of the expression's language, FALSE otherwise.
   */
  def accepts(events: List[GenericEvent]): Boolean = accepts(events, Valuation())._1

  def accepts(
               events: List[GenericEvent],
               window: Int
             ): Boolean = {
    if (events.length > window) false else accepts(events)
  }

  def isMarked: Boolean = marked

  def mark(): Unit = marked = true

  def unmark(): Unit = marked = false

  def cloneFormula(): SREFormula
}

object SRESentence {
  def apply(
             sentence: LogicSentence,
             registerVariable: RegisterVariable
           ): SRESentence = new SRESentence(sentence, Option(registerVariable))

  def apply(sentence: LogicSentence): SRESentence = new SRESentence(sentence, None)
}

/**
  * A formula composed of a single sentence, i.e., with no regular expression operators.
  *
  * @param sentence The single sentence
  */
case class SRESentence(
                        sentence: LogicSentence,
                        registerVariable: Option[RegisterVariable]
                      ) extends SREFormula {

  /**
    * @return All sentences contained in the formula. A singleton set in this case.
    */
  override def getSentences: Set[LogicSentence] = Set(sentence)

  /**
   *  @return All register variables declared in the formula. A singleton set in this case.
   */
  override def getRegisterVariables: Set[RegisterVariable] =
    registerVariable match {
      case None => Set.empty[RegisterVariable]
      case Some(rv) => Set(rv)
    }

  /**
   *  @return All variables used as arguments in the logic sentence.
   */
  override def getVariableArguments: Set[String] = sentence.getRegisterVariableArguments

  /**
    * A function that tests whether a given string (list of events) is a member of the expression's language.
    * Simply evaluates the sentence against the event and a given valuation. The valuation is used only with SREM.
    * If more than one events are given, FALSE by default.
    * Also returns a new valuation (for SREM). If no register variable has been declared with the sentence, the same
    * valuation is returned. If a register variable has been declared, the valuation is updated. If the sentence
    * evaluates to FALSE, an empty set of valuations is returned.
    *
    * @param events The string to be tested.
    * @param valuation The given valuation.
    * @return TRUE and an updated (singleton) set of valuations if the string is member of the expression's language.
    *         FALSE otherwise, along with an empty set of valuations.
    */
  override def accepts(
                        events: List[GenericEvent],
                        valuation: Valuation
                      ): (Boolean, Set[Valuation]) = {
    val (result, newValuation) = events match {
      case head :: Nil => {
        val r = SentenceConstructor.getNewSentenceInstance(sentence).evaluate(head, valuation)
        val newV =
          registerVariable match {
            case None => valuation
            case Some(rv) => valuation.update(rv, head)
          }
        (r, Set(newV))
      }
      case Nil => sentence match {
        case _: EpsilonSentence => (true, Set.empty[Valuation])
        case _ => (false, Set.empty[Valuation])
      }
      case _ => (false, Set.empty[Valuation])
    }
    (result, newValuation)
  }

  override def toString: String = {
    val sentenceStr = sentence.toString
    val regStr = registerVariable match {
      case Some(r) => "[" + r.toString + "]"
      case None => ""
    }
    sentenceStr + regStr
  }

  override def cloneFormula(): SREFormula = SRESentence(sentence)

}

/**
  * When a formula contains regular expression operators, it then has the form of a tree with the leaves having the
  * sentences and the internal nodes the operators.
  *
  * @param op The operator of the current sub-expression.
  * @param formulas The formulas under this operator.
  */
case class SREOperator(
                        op: RegularOperator,
                        formulas: List[SREFormula]
                      ) extends SREFormula {
  require(((op == ITER | op == NEG | op == ANY | op == NEXT) & formulas.size == 1) | (formulas.size >= 2), "ITER, NEG, ANY, NEXT must have only a single child. SEQ and OR at least two.")

  /**
    * @return All sentences contained in the formula.
    */
  override def getSentences: Set[LogicSentence] = {
    op match {
      case ITER => formulas.head.getSentences
      case ANY => formulas.head.getSentences
      case _ => formulas.foldLeft(Set.empty[LogicSentence]) { (acc, f) => acc ++ f.getSentences }
    }
  }

  /**
   *  @return All register variables declared in the formula.
   */
  override def getRegisterVariables: Set[RegisterVariable] = {
    formulas.foldLeft(Set.empty[RegisterVariable]) { (acc, f) => acc ++ f.getRegisterVariables }
  }

  /**
   *  @return All variables used as arguments in logic sentences contained in the formula.
   */
  override def getVariableArguments: Set[String] = formulas.map(f => f.getVariableArguments).toSet.flatten

  /**
    * A function that tests whether a given string (list of events) is a member of the expression's language.
    * For SEQ, we split the string in two sub-strings in all the possible ways and check whether the first sub-string
    * is accepted by the first operand of SEQ and the second sub-string is accepted by the second operand of SEQ.
    * For CHOICE, we simply check whether the string is accepted by either of the operands of CHOICE.
    * For ITER, e.g., R*, we split again the string and check whether the first sub-string is accepted by R and the
    * second sub-string by R*.
    * If the expression is a SREM and evaluates to TRUE, a set of valid * (non-empty) updated valuations is also
    * returned. If FALSE, an empty set of valuations is returned (i.e., no valid valuation has been found).
    *
    * @param events The string to be tested.
    * @param valuation The given valuation.
    * @return TRUE and an updated (singleton) set of valuations if the string is member of the expression's language.
    *         FALSE otherwise, along with an empty set of valuations.
    */
  override def accepts(
                        events: List[GenericEvent],
                        valuation: Valuation
                      ): (Boolean, Set[Valuation]) = {

    op match {
      case SEQ =>
        val allSplits = for (i <- 0 to events.size) yield events.splitAt(i)
        val newValuations = allSplits.foldLeft(Set.empty[Valuation]) { (acc, s) => acc ++ checkSplit(formulas.head,s._1,formulas(1),s._2,valuation)._2 }
        if (newValuations.nonEmpty) (true, newValuations) else (false, Set.empty[Valuation])
        //allSplits.exists(s => formulas.head.accepts(s._1) & formulas(1).accepts(s._2))
      case CHOICE => {
        val (result1, newValuation1) = formulas.head.accepts(events, valuation)
        val (result2, newValuation2) = formulas(1).accepts(events, valuation)
        val result = result1 || result2
        val newValuation  = {
          if (result1) {
            if (result2) newValuation1 ++ newValuation2 else newValuation1
          }
          else {
            if (result2) newValuation2 else Set.empty[Valuation]
          }
        }
        (result, newValuation)
      }
      case ITER => {
        events match {
          case Nil => (true, Set(valuation))
          case _ :: Nil => formulas.head.accepts(events, valuation)
          case _ => {
            val allSplits = for (i <- 0 to events.size) yield events.splitAt(i)
            // s._2 could be the same as events. With this.accepts(s._2), we would test again the same string and
            // enter an infinite recursion.
            val keptSplits = allSplits.filter(s => s._2 != events)
            val newValuations = keptSplits.foldLeft(Set.empty[Valuation]) { (acc, s) => acc ++ checkSplit(formulas.head,s._1,this,s._2,valuation)._2 }
            if (newValuations.nonEmpty) (true, newValuations) else (false, Set.empty[Valuation])
          }
        }
      }
    }
  }


  /**
   * Checks whether a sequential expression, split into two sun-expressions, is satisfied with a given word, also split
   * into two sub-words. The first sub-expression must be satisfied by the first sub-word. The second sub-expression by
   * the second sub-word, also taking into account the new valuation produced by the first sub-expression.
   *
   * @param formula1 The first sub-expression.
   * @param events1 The first sub-word.
   * @param formula2 The second sub-expression.
   * @param events2 The second sub-word.
   * @param valuation The given, initial valuation.
   * @return TRUE if the two sub-expression are satisfied by the split, along with all the valid, new valuations.
   *         Otherwise FALSE, along with an empty set of valuations.
   */
  private def checkSplit(
                          formula1: SREFormula,
                          events1: List[GenericEvent],
                          formula2: SREFormula,
                          events2: List[GenericEvent],
                          valuation: Valuation
                        ): (Boolean, Set[Valuation]) = {
    val (result1, newValuation1) = formula1.accepts(events1, valuation)
    val newValuation2 = if (result1) newValuation1.map(v => formula2.accepts(events2, v)).filter(x => x._1).flatMap(y => y._2) else Set.empty[Valuation]
    if (newValuation2.nonEmpty) (true, newValuation2) else (false, Set.empty[Valuation])
  }

  override def toString: String = op.toString + "(" + list2Str[SREFormula](formulas, ",") + ")"

  override def cloneFormula(): SREFormula = SREOperator(op, formulas)
}

class SRETrueSentence extends SRESentence(new TrueSentence, None)

class SREEpsilonSentence extends SRESentence(new EpsilonSentence, None)


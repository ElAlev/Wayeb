package fsm.symbolic.sre

import fsm.symbolic.sfa.logic.SentenceConstructor
import fsm.symbolic.sre.RegularOperator._
import stream.GenericEvent
import utils.StringUtils.list2Str

/**
  * Classes to build formulas with fsm.symbolic.sre.SREParser.
  */

abstract class SREFormula {
  /**
    * @return All sentences contained in the formula.
    */
  def getSentences: Set[LogicSentence]

  /**
    * A function that tests whether a given string (list of events) is a member of the SRE's language.
    *
    * @param events The string to be tested.
    * @return TRUE if the string is member of the SRE's language, FALSE otherwise.
    */
  def accepts(events: List[GenericEvent]): Boolean
}

/**
  * A formula composed of a single sentence, i.e., with no regular expression operators.
  *
  * @param sentence The single sentence
  */
case class SRESentence(sentence: LogicSentence) extends SREFormula {
  /**
    * @return All sentences contained in the formula. A singleton set in this case.
    */
  override def getSentences: Set[LogicSentence] = Set(sentence)

  /**
    * A function that tests whether a given string (list of events) is a member of the SRE's language.
    * Simply evaluates the sentence against the event. If more than one events are given, FALSE by default.
    *
    * @param events The string to be tested.
    * @return TRUE if the string is member of the SRE's language, FALSE otherwise.
    */
  override def accepts(events: List[GenericEvent]): Boolean = {
    events match {
      case head :: Nil => SentenceConstructor.getNewSentenceInstance(sentence).evaluate(head)
      case _ => false
    }
  }

  override def toString: String = sentence.toString
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
    * A function that tests whether a given string (list of events) is a member of the SRE's language.
    * For SEQ, we split the string in two sub-strings in all the possible ways and check whether the first sub-string
    * is accepted by the first operand of SEQ and the second sub-string is accepted by the second operand of SEQ.
    * For CHOICE, we simply check whether the string is accepted by either of the operands of CHOICE.
    * For ITER, e.g., R*, we split again the string and check whether the first sub-string is accepted by R and the
    * second sub-string by R*.
    *
    * @param events The string to be tested.
    * @return TRUE if the string is member of the SRE's language, FALSE otherwise.
    */
  override def accepts(events: List[GenericEvent]): Boolean = {

    op match {
      case SEQ =>
        val allSplits = for (i <- 0 to events.size) yield events.splitAt(i)
        allSplits.exists(s => formulas.head.accepts(s._1) & formulas(1).accepts(s._2))
      case CHOICE => formulas.head.accepts(events) || formulas(1).accepts(events)
      case ITER => {
        events match {
          case Nil => true
          case _ :: Nil => formulas.head.accepts(events)
          case _ => {
            val allSplits = for (i <- 0 to events.size) yield events.splitAt(i)
            // s._2 could be the same as events. With this.accepts(s._2), we would test again the same string and
            // enter an infinite recursion.
            val keptSplits = allSplits.filter(s => s._2 != events)
            keptSplits.exists(s => {
              formulas.head.accepts(s._1) & this.accepts(s._2)
            })
          }
        }
      }
    }
  }

  override def toString: String = op.toString + "(" + list2Str[SREFormula](formulas, ",") + ")"
}

class SRETrueSentence extends SRESentence(new TrueSentence)


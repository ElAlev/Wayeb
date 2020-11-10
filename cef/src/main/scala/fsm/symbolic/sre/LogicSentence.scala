package fsm.symbolic.sre

import fsm.symbolic.sre.BooleanOperator._
import utils.StringUtils.list2Str

/**
  * Some classes required by fsm.symbolic.sre.SREParser.
  * Note that these classes are used just by the parser for the initial parsing and not internally for reasoning.
  * Actual SDFA make use of the classes under fsm.symbolic.sfa.logic.
  */

abstract class LogicSentence

case class LogicAtomicSentence(
                                p: LogicPredicate,
                                terms: List[LogicTerm]
                              ) extends LogicSentence {
  override def toString: String = {
    p.toString + "(" + list2Str[LogicTerm](terms, "") + ")"
  }

}
case class LogicComplexSentence(
    op: BooleanOperator,
    sentences: List[LogicSentence]
) extends LogicSentence {
  require((op == NOT & sentences.size == 1) | (sentences.size > 1), "NOT must have only a single operand. AND and OR at least two.")

  override def toString: String = op.toString + "(" + list2Str[LogicSentence](sentences, ",") + ")"

}

class TrueSentence extends LogicAtomicSentence(new TruePredicate, List.empty[LogicTerm])

case class PartialComplexSentence(
    op: BooleanOperator,
    sentence: LogicSentence
)

abstract class LogicTerm

case class LogicConstant(value: String) extends LogicTerm {
  override def toString: String = value
}

case class NumericalConstant(value: Double) extends LogicTerm {
  override def toString: String = value.toString
}

case class LogicVariable(name: String) extends LogicTerm {
  override def toString: String = name
}

case class LogicPredicate(name: String) {
  override def toString: String = name
}

class TruePredicate extends LogicPredicate("TruePredicate")

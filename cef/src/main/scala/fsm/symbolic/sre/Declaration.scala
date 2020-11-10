package fsm.symbolic.sre

import fsm.symbolic.sfa.logic.{LogicUtils, Predicate, Sentence}
import utils.StringUtils.list2Str

abstract class Declaration

/**
  * A holder for the list of exclusives. Each fsm.symbolic.sre.LogicAtomicSentence is converted to a
  * fsm.symbolic.sfa.logic.Sentence.
  *
  * @param sentencesSRE The parsed pattern predicates to be converted to actual sentences.
  */
case class Exclusive(sentencesSRE: List[LogicAtomicSentence]) extends Declaration {
  val sentences: List[Sentence] = sentencesSRE.map(s => LogicUtils.parsed2ActualSentence(s))
  val predicates: Set[Predicate] = sentences.foldLeft(Set.empty[Predicate]) { (acc, x) => acc ++ x.extractPredicates }

  val getPredicates: Set[Predicate] = predicates

  override def toString: String = "Exclusive(" + list2Str(predicates.toList, ",") + ")"
}

/**
  * A holder for the list of extras. Each fsm.symbolic.sre.LogicAtomicSentence is converted to a
  * * fsm.symbolic.sfa.logic.Sentence.
  *
  * @param sentencesSRE The parsed pattern predicates to be converted to actual sentences.
  */
case class Extras(sentencesSRE: List[LogicAtomicSentence]) extends Declaration {
  val sentences: List[Sentence] = sentencesSRE.map(s => LogicUtils.parsed2ActualSentence(s))
  val predicates: Set[Predicate] = sentences.foldLeft(Set.empty[Predicate]) { (acc, x) => acc ++ x.extractPredicates }

  val getPredicates: Set[Predicate] = predicates

  val getSentences: List[Sentence] = sentences

  override def toString: String = "Extras(" + list2Str(predicates.toList, ",") + ")"
}

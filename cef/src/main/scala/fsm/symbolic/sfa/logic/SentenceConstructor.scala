package fsm.symbolic.sfa.logic

import fsm.symbolic.sre.LogicSentence

class SentenceConstructor {

}

object SentenceConstructor {

  /**
    * Creates a new atomic sentence from a formula's logic sentence. The logic sentence is parsed and converted to an
    * actual sentence.
    *
    * @param ls The formula's logic sentence.
    * @return An atomic sentence corresponding to the formula's logic sentence.
    */
  def getNewSentenceInstance(ls: LogicSentence): Sentence = LogicUtils.parsed2ActualSentence(ls)

  /**
    * Creates a new atomic "sentence" that checks always evaluates to true, even without an event.
    *
    * @return An atomic sentence corresponding to an epsilon transition.
    */
  def getNewEpsilonSentence: AtomicSentence = new EpsilonSentence(PredicateConstructor.getEpsilonPred)

  /**
    * Creates a new atomic sentence that checks always (for every event) evaluates to true.
    *
    * @return An atomic sentence that always returns true.
    */
  def getNewTrueSentence: AtomicSentence = new TrueSentence(PredicateConstructor.getTruePred)

  /**
    * Creates a new atomic sentence that checks if type of an event is equal to a given type.
    *
    * @param eventType The type that the event must have for the sentence to be satisfied.
    * @return An atomic sentence that checks the event type.
    */
  def getNewEventTypeSentence(eventType: String): AtomicSentence =
    new IsEventTypeSentence(PredicateConstructor.getEventTypePred(eventType))
}

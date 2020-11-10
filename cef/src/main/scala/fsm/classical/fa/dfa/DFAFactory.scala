package fsm.classical.fa.dfa

import fsm.classical.pattern.regexp.RegExpTree
import fsm.CountPolicy._

class DFAFactory {

}

object DFAFactory {

  /**
    * Constructs a DFA from a regular expression.
    *
    * @param pattern The pattern given as a regular expression tree.
    * @param countPolicy The counting policy, OVERLAP or NONOVERLAP.
    * @param streamSymbols All the symbols to be encountered in the stream.
    * @param order The disambiguation order.
    * @return A disambiguated, streaming DFA corresponding to the given pattern.
    */
  def buildDFAFromRe(
                      pattern: RegExpTree,
                      countPolicy: CountPolicy,
                      streamSymbols: scala.collection.immutable.Set[String],
                      order: Int
                    ): DFA = {
    DFA(pattern, countPolicy, streamSymbols, order)
  }


}

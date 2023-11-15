package fsm.classical.fa.dfa

//import fsm.archived.AhoCorasick.{ACAutomaton, ACFactory}
import fsm.symbolic.sfa.sdfa.SDFA
import fsm.classical.pattern.regexp.RegExpTree
import scala.collection.mutable.Set
import fsm.CountPolicy._
import model.vmm.mapper.Isomorphism

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

  /**
    * Constructs a DFA from a SDFA according to a given isomorphism. The two automata are structurally the same.
    * The only difference is that the transitions of the DFA have a symbol corresponding to the sentence of the
    * corresponding transition of the SDFA.
    *
    * @param sdfa The original SDFA.
    * @param iso The given isomorphism.
    * @return
    */
  def buildDFAFromSDFA(
                        sdfa: SDFA,
                        iso: Isomorphism
                      ): DFA = {
    val dfa = new DFA
    val sdfaStates = sdfa.states.keySet
    val sdfaTransitions = sdfa.transitions
    val inputSymbols = scala.collection.mutable.Set.empty[String]
    for (sdfaState <- sdfaStates) {
      val dfaState = new DFAState(sdfaState)
      if (sdfa.isStart(sdfaState)) dfaState.setAsStart(true)
      if (sdfa.isFinal(sdfaState)) dfaState.setOutput(Set("final"))
      val outgoingSdfaTransitions = sdfaTransitions.filter(t => t.source == sdfaState)
      for (outTransition <- outgoingSdfaTransitions) {
        val sentence = outTransition.guard.sentence
        val symbol = iso.getSymbolForMinTerm(sentence)
        val symbolAsString = symbol.toString
        inputSymbols += symbolAsString
        val target = outTransition.target
        dfaState.addDelta(symbolAsString, target)
      }
      dfa.addState(sdfaState, dfaState)
    }
    dfa.setInputSymbols(inputSymbols)
    dfa
  }

  /*
  def buildDFAFromRe(patternFile: String, countPolicy: CountPolicy, streamSymbols: scala.collection.immutable.Set[Char], memory: Int): DFA = {
    val rer = new RegExpReader(patternFile,streamSymbols.map(x=>x.toString))
    val patternRE = rer.getRegExpTreeWithSigmaStar()
    val elnfa = NFAFactory.buildEliminatedNFAForStream(patternRE,streamSymbols.toSet)
    val nfa = NFAUtils.setCountPolicy(elnfa,countPolicy)
    var dfa = DFAUtils.convertNfa2Dfa(nfa) //TODO: .withMemory(memory)
    if (memory>0) {
      val dis = new Disambiguator(dfa,memory)
      // WARNING: Do NOT ever use append after disambiguation
      dfa = dis.disambiguate()
    }
    dfa
  }

  private def buildDFA(aca: ACAutomaton): DFA = {
    val dfa = new DFA
    for ((k, v) <- aca.getStates) {
      val dfas = new DFAState(k)
      dfas.setDelta(v.getDelta.map(s => (s._1.toString, s._2)))
      dfas.setOutput(v.getOutput())
      if (k == 0) dfas.setAsStart(true)
      dfa.addState(k, dfas)
    }
    dfa.setInputSymbols(aca.getInputSymbols.map(c => c.toString))
    dfa
  }

  private def appendDFA(oldDfa: DFA, aca: ACAutomaton): DFA = {
    val newDfa = buildDFA(aca)
    // WARNING: Do NOT ever use append after disambiguation
    oldDfa.append(newDfa)
    oldDfa
  }

  def buildDFA(
      myPattern: String,
      countPolicy: CountPolicy,
      inputSymbols: Set[String],
      memory: Int): DFA = {
    //require(memory>0)
    var dfa = buildDFA(myPattern, countPolicy, inputSymbols)
    if (memory > 0) {
      val dis = new Disambiguator(dfa, memory)
      // WARNING: Do NOT ever use append after disambiguation
      dfa = dis.disambiguate()
    }
    dfa
  }

  private def buildDFA(
      myPattern: String,
      countPolicy: CountPolicy,
      inputSymbols: Set[String]): DFA = {
    require(inputSymbols.forall(s => s.length == 1))
    val r = new pattern.Reader
    val modules = r.getModules(myPattern, inputSymbols)
    if (modules.size > 1 & countPolicy == OVERLAP)
      throw new IllegalArgumentException("Overlapping policy currently not supported for modular patterns")
    var dfa = new DFA
    for (m <- modules) {
      val aca = ACFactory.buildACA(List(m), countPolicy, inputSymbols.map(s => s.head))
      dfa = DFAFactory.appendDFA(dfa, aca)
    }
    dfa.setPattern(myPattern)
    dfa
  }
  */

}

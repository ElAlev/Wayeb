package fsm.symbolic.sfa.snfa

import fsm.symbolic.TransitionOutput
import fsm.symbolic.logic.SentenceConstructor
import fsm.symbolic.sfa._
import fsm.symbolic.sre.RegularOperator._
import fsm.symbolic.sre._

/**
  * Set of utils for building and handling SNFA.
  */
object SNFAUtils {

  /**
    * Builds a streaming SNFA from a LearnLib DFA.
    *
    * @param llstates The states of the LLDFA.
    * @param lltransitions The transitions of the LLDFA.
    * @param llstart The id of the start state of the LLDFA.
    * @param llfinals The ids of the final states of the LLDFA.
    * @return A streaming SNFA corresponding to the given LLDFA.
    */
  def buildStreamSNFAFromLLDFA(
                                llstates: Set[Int],
                                lltransitions: Set[(Int, Int, String)],
                                llstart: Int,
                                llfinals: Set[Int]
                              ): SNFA = {
    // first map all states of the LLDFA to states of the SNFA
    val states = llstates.map(llstate => (llstate, SNFAState(llstate))).toMap
    // now create all transitions using the symbols as guards
    val transitions = lltransitions.map(lltransition => SFATransition(lltransition._1, lltransition._2, SFAGuard(SentenceConstructor.getNewEventTypeSentence(lltransition._3)))).toList
    // we have to make the SNFA streaming, so we have to add a new start state with a true loop and connect it with
    // an epsilon transition to the old start state
    val startStateId = llstates.max + 1
    val startState = SNFAState(startStateId)
    val epsilonTransition = SFATransition(startStateId, llstart, SFAGuard(SentenceConstructor.getNewEpsilonSentence))
    val loopTransition = SFATransition(startStateId, startStateId, SFAGuard(SentenceConstructor.getNewTrueSentence))
    val allStates = states + (startStateId -> startState)
    val allTransitions = loopTransition :: epsilonTransition :: transitions
    // all done, create the SNFA
    val streamSNFA = SNFA(allStates, allTransitions, startStateId, llfinals)
    streamSNFA
  }

  /**
    * Eliminates all epsilon transitions.
    *
    * @param esnfa Original SNFA, possibly with epsilon transitions.
    * @return SNFA without epsilon transitions.
    */
  def eliminateEpsilon(esnfa: SNFA): SNFA = {
    val elim = new Eliminator(esnfa)
    elim.eliminate
  }

  /**
    * Creates a streaming SNFA from a formula. The SNFA must work on streams, so, if f is the original formula, we must
    * create the SNFA for (TRUE* ; f) so that we can start detection at any time.
    *
    *
    * @param f The initial formula.
    * @return Streaming SNFA corresponding to f.
    */
  def buildSNFAForStream(f: SREFormula): SNFA = {
    val trueFormula = new SRETrueSentence
    //trueFormula.unmark()
    //val sigmaStar = SREOperator(ITER, List(trueFormula))
    //val streamFormula = SREOperator(SEQ, List(sigmaStar, f))
    //buildSNFA(streamFormula)
    val nonStreamSNFA = buildSNFA(f)
    val streamSNFA = addInitialSelfLoop(nonStreamSNFA)
    streamSNFA
  }

  def addInitialSelfLoop(snfa: SNFA): SNFA = {
    val newStateId = snfa.states.keySet.max + 1
    val newState = SNFAState(newStateId)
    val newEpsilonTransition = SFATransition(
      newStateId,
      snfa.start,
      SFAGuard(SentenceConstructor.getNewEpsilonSentence)
    )
    val newTrueTransition = SFATransition(
      newStateId,
      newStateId,
      SFAGuard(SentenceConstructor.getNewTrueSentence),
      TransitionOutput.IGNORE
    )
    val newStates = snfa.states + (newStateId -> newState)
    val newTransitions = newTrueTransition :: newEpsilonTransition :: snfa.transitions
    val snfaWithSkip = SNFA(newStates, newTransitions, newStateId, snfa.finals)
    snfaWithSkip
  }

  /**
    * Creates a SNFA from a formula. It first creates an ID generator for the labels of the states.
    *
    * @param f The original formula.
    * @return The SNFA corresponding to f.
    */
  def buildSNFA(f: SREFormula): SNFA = {
    val idg = IdGenerator()
    buildSNFA(f, idg)
  }

  /**
    * Creates a SNFA from a formula, given an ID generator. For each of the three operators, there is a separate
    * function. This is essentially a recursive function, called from within buildSNFAFromSEQ, buildSNFAFromOR and
    * buildSNFAFromITER. buildSNFAFromSentence handles the base case.
    *
    * @param formula The original formula.
    * @param idg The ID generator. Can generate unique IDs for the SNFA's states.
    * @return The SNFA corresponding to f.
    */
  private def buildSNFA(
                         formula: SREFormula,
                         idg: IdGenerator
                       ): SNFA = {
    formula match {
      case SRESentence(sentence: LogicSentence, _) => // case when no regular expression operators exist
        buildSNFAFromSentence(sentence, idg, formula.isMarked)
      case SREOperator(op: RegularOperator, formulas: List[SREFormula]) => {
        op match {
          case SEQ => buildSNFAFromSEQ(formulas, idg)
          case CHOICE => buildSNFAFromOR(formulas, idg)
          case ITER => buildSNFAFromITER(formulas, idg)
          case NEG => buildSNFAFromNEG(formulas, idg)
          case _ => throw new IllegalArgumentException("Unknown operator " + op)
        }
      }
      case _ => throw new IllegalArgumentException("Unknown formula " + formula.toString)
    }
  }

  /**
    * Creates SNFA from a single sentence (base case). Creates two states and connects them with a transition that
    * checks the given sentence.
    *
    * @param sentence The single sentence of the formula.
    * @param idg The ID generator.
   *  @param marked Flag that informs whether this sentence must mark its transition.
    * @return The SNFA for the single sentence.
    */
  private def buildSNFAFromSentence(
                                     sentence: LogicSentence,
                                     idg: IdGenerator,
                                     marked: Boolean
                                   ): SNFA = {
    val startId = idg.getIdCautiousImmut
    val startState = SNFAState(startId)
    val finalId = idg.getIdCautiousImmut
    val finalState = SNFAState(finalId)
    val actualSentence = sentence match {
      case _: EpsilonSentence => SentenceConstructor.getNewEpsilonSentence
      case _ => SentenceConstructor.getNewSentenceInstance(sentence)
    }
    val guard = SFAGuard(actualSentence)
    val output = if (marked) TransitionOutput.TAKE else TransitionOutput.IGNORE
    val transition = SFATransition(startId, finalId, guard, output)
    val states: Map[Int, SNFAState] = Map(startId -> startState, finalId -> finalState)
    SNFA(states, List(transition), startId, Set(finalId))
  }

  /**
    * Creates SNFA from sequence. Connects the two sub NFAs by inserting an epsilon transition from some final of left
    * NFA to the start state of right NFA
    *
    * @param formulas The left and right sub-formulas of the sequence operator.
    * @param idg The ID generator.
    * @return The SNFA for the sequence operator.
    */
  private def buildSNFAFromSEQ(
                                formulas: List[SREFormula],
                                idg: IdGenerator
                              ): SNFA = {
    require(formulas.size == 2)
    val leftNFA = buildSNFA(formulas.head, idg)
    val rightNFA = buildSNFA(formulas(1), idg)
    val finalTransitions = for (leftFinal <- leftNFA.finals) yield {
      val rightStartId = rightNFA.start
      val sentence = SentenceConstructor.getNewEpsilonSentence
      val guard = SFAGuard(sentence)
      val transition = SFATransition(leftFinal, rightStartId, guard)
      transition
    }
    val transitions = finalTransitions.toList ::: (leftNFA.transitions ::: rightNFA.transitions)
    val states = leftNFA.states ++ rightNFA.states
    SNFA(states, transitions, leftNFA.start, rightNFA.finals)
  }

  /**
    * Creates SNFA from disjunction.
    *   Creates a new start state.
    *   From this start state, inserts epsilon transitions to the start states of the sub NFAs.
    *   Creates a new final state.
    *   From some final of the sub NFAs, inserts epsilon transitions to the new final state.
    *
    * @param formulas The left and right sub-formulas of the or operator.
    * @param idg The ID generator.
    * @return The SNFA for the or operator.
    */
  private def buildSNFAFromOR(
                               formulas: List[SREFormula],
                               idg: IdGenerator
                             ): SNFA = {
    require(formulas.size == 2)

    val leftNFA = buildSNFA(formulas.head, idg)
    val rightNFA = buildSNFA(formulas(1), idg)

    val startId = idg.getIdCautiousImmut
    val startState = SNFAState(startId)
    val leftStartId = leftNFA.start
    val rightStartId = rightNFA.start
    val leftStartSent = SentenceConstructor.getNewEpsilonSentence
    val rightStartSent = SentenceConstructor.getNewEpsilonSentence
    val leftStartGuard = SFAGuard(leftStartSent)
    val rightStartGuard = SFAGuard(rightStartSent)
    val leftStartTrans = SFATransition(startId, leftStartId, leftStartGuard)
    val rightStartTrans = SFATransition(startId, rightStartId, rightStartGuard)

    val finalId = idg.getIdCautiousImmut
    val finalState = SNFAState(finalId)
    val states = (leftNFA.states ++ rightNFA.states) ++
      Map(startId -> startState, finalId -> finalState)

    val transitions = {
      var finalTrans: List[SFATransition] = List.empty

      val leftFinalTransitions = for (leftFinal <- leftNFA.finals) yield {
        val leftFinalSent = SentenceConstructor.getNewEpsilonSentence
        val leftFinalGuard = SFAGuard(leftFinalSent)
        val leftFinalTrans = SFATransition(leftFinal, finalId, leftFinalGuard)
        leftFinalTrans
      }
      finalTrans = leftFinalTransitions.toList ::: finalTrans

      val rightFinalTransitions = for (rightFinal <- rightNFA.finals) yield {
        val rightFinalSent = SentenceConstructor.getNewEpsilonSentence
        val rightFinalGuard = SFAGuard(rightFinalSent)
        val rightFinalTrans = SFATransition(rightFinal, finalId, rightFinalGuard)
        rightFinalTrans
      }
      finalTrans = rightFinalTransitions.toList ::: finalTrans

      List(leftStartTrans, rightStartTrans) ::: finalTrans :::
        (leftNFA.transitions ::: rightNFA.transitions)
    }
    SNFA(states, transitions, startId, Set(finalId))
  }

  /**
    * Creates SNFA from iteration.
    *   Insert an epsilon transition from some final to the start state.
    *   Create a new start state.
    *   Create a new final state.
    *   Insert an epsilon transition from some final of sub NFA to new final.
    *   Insert an epsilon transition from new start to new final.
    *   Insert an epsilon transition from new start to start of sub NFA.
    *
    * @param formulas The sub-formula of the iteration operator (assumes only one exists).
    * @param idg The ID generator.
    * @return The SNFA for the iteration operator.
    */
  private def buildSNFAFromITER(
                                 formulas: List[SREFormula],
                                 idg: IdGenerator
                               ): SNFA = {
    require(formulas.size == 1)

    val subNFA = buildSNFA(formulas.head, idg)

    val loopTransitions = for (subFinal <- subNFA.finals) yield {
      val loopSent = SentenceConstructor.getNewEpsilonSentence
      val loopGuard = SFAGuard(loopSent)
      val loopTrans = SFATransition(subFinal, subNFA.start, loopGuard)
      loopTrans
    }

    val startId = idg.getIdCautiousImmut
    val startState = SNFAState(startId)
    val finalId = idg.getIdCautiousImmut
    val finalState = SNFAState(finalId)

    val finalTransitions = for (subFinal <- subNFA.finals) yield {
      val finalsSent = SentenceConstructor.getNewEpsilonSentence
      val finalsGuard = SFAGuard(finalsSent)
      val finalsTrans = SFATransition(subFinal, finalId, finalsGuard)
      finalsTrans
    }

    val startFinalSent = SentenceConstructor.getNewEpsilonSentence
    val startFinalGuard = SFAGuard(startFinalSent)
    val startFinalTrans = SFATransition(startId, finalId, startFinalGuard)

    val startsSent = SentenceConstructor.getNewEpsilonSentence
    val startsGuard = SFAGuard(startsSent)
    val startsTrans = SFATransition(startId, subNFA.start, startsGuard)

    val states = subNFA.states ++ Map(startId -> startState, finalId -> finalState)
    val transitions = loopTransitions.toList ::: finalTransitions.toList ::: List(startFinalTrans, startsTrans) ::: subNFA.transitions
    SNFA(states, transitions, startId, Set(finalId))
  }

  /**
    * Creates SNFA from negation. First creates the SNFA for the formula inside the negation, and then flips all
    * finals and non-finals, i.e., make all final states non-final and make all non-final final.
    *
    *
    * @param formulas The sub-formula of the negation operator (assumes only one exists).
    * @param idg The ID generator.
    * @return The SNFA for the negation operator.
    */
  private def buildSNFAFromNEG(
                                formulas: List[SREFormula],
                                idg: IdGenerator
                              ): SNFA = {
    require(formulas.size == 1)

    val initialSNFA = buildSNFA(formulas.head, idg)
    val completedSNFA = makeSNFAComplete(initialSNFA, idg)
    val startId = completedSNFA.start
    //val finalId = idg.getId
    val finalIds = completedSNFA.states.keySet &~ completedSNFA.finals
    val states = completedSNFA.states
    val transitions = completedSNFA.transitions.map(t => SFATransition(t.source, t.target, t.guard.asInstanceOf[SFAGuard], TransitionOutput.IGNORE))
    SNFA(states, transitions, startId, finalIds)
  }

  //TODO: find a way to complete a SNFA without determinizing it.
  //Each state to be created when needed. No reuse of dead states.
  //Dead states not to be touched when eliminating epsilon or completing.
  /**
    * Creates a complete SNFA from a possibly incomplete one.
    * The most straightforward way to complete a SNFA is to determinize it.
    *
    * @param initSnfa A SNFA that is possibly not complete.
    * @param idg The ID generator.
    * @return A SNFA equivalent to snfa but complete.
    */
  def makeSNFAComplete(
                        initSnfa: SNFA,
                        idg: IdGenerator
                      ): SNFA = {
    val sdfa = SFAUtils.determinize(initSnfa, idg)
    val states = sdfa.states.map(s => (s._1, SNFAState(s._1)))
    SNFA(states, sdfa.transitions, sdfa.start, sdfa.finals)
  }

}

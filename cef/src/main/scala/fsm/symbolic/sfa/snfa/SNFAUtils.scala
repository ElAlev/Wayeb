package fsm.symbolic.sfa.snfa

import fsm.symbolic.sre.RegularOperator._
import fsm.symbolic.sre._
import fsm.symbolic.sfa._
import fsm.symbolic.sfa.logic.SentenceConstructor

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
    val transitions = lltransitions.map(lltransition => Transition(lltransition._1, lltransition._2, Guard(SentenceConstructor.getNewEventTypeSentence(lltransition._3)))).toList
    // we have to make the SNFA streaming, so we have to add a new start state with a true loop and connect it with
    // an epsilon transition to the old start state
    val startStateId = llstates.max + 1
    val startState = SNFAState(startStateId)
    val epsilonTransition = Transition(startStateId, llstart, Guard(SentenceConstructor.getNewEpsilonSentence))
    val loopTransition = Transition(startStateId, startStateId, Guard(SentenceConstructor.getNewTrueSentence))
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
    val sigmaStar = SREOperator(ITER, List(trueFormula))
    val streamFormula = SREOperator(SEQ, List(sigmaStar, f))
    buildSNFA(streamFormula)
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
    * @param f The original formula.
    * @param idg The ID generator. Can generate unique IDs for the SNFA's states.
    * @return The SNFA corresponding to f.
    */
  private def buildSNFA(
                         f: SREFormula,
                         idg: IdGenerator
                       ): SNFA = {
    f match {
      case SRESentence(sentence: LogicSentence) => // case when no regular expression operators exist
        buildSNFAFromSentence(sentence, idg)
      case SREOperator(op: RegularOperator, formulas: List[SREFormula]) => {
        op match {
          case SEQ => buildSNFAFromSEQ(formulas, idg)
          case CHOICE => buildSNFAFromOR(formulas, idg)
          case ITER => buildSNFAFromITER(formulas, idg)
          case NEG => buildSNFAFromNEG(formulas, idg)
          case _ => throw new IllegalArgumentException("Unknown operator " + op)
        }
      }
      case _ => throw new IllegalArgumentException("Unknown formula " + f.toString)
    }
  }

  /**
    * Creates SNFA from a single sentence (base case). Creates two states and connects them with a transition that
    * checks the given sentence.
    *
    * @param sentence The single sentence of the formula.
    * @param idg The ID generator.
    * @return The SNFA for the single sentence.
    */
  private def buildSNFAFromSentence(
                                     sentence: LogicSentence,
                                     idg: IdGenerator
                                   ): SNFA = {
    val startId = idg.getId
    val startState = SNFAState(startId)
    val finalId = idg.getId
    val finalState = SNFAState(finalId)
    val actualSentence = SentenceConstructor.getNewSentenceInstance(sentence)
    val guard = Guard(actualSentence)
    val transition = Transition(startId, finalId, guard)
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
      val guard = Guard(sentence)
      val transition = Transition(leftFinal, rightStartId, guard)
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

    val startId = idg.getId
    val startState = SNFAState(startId)
    val leftStartId = leftNFA.start
    val rightStartId = rightNFA.start
    val leftStartSent = SentenceConstructor.getNewEpsilonSentence
    val rightStartSent = SentenceConstructor.getNewEpsilonSentence
    val leftStartGuard = Guard(leftStartSent)
    val rightStartGuard = Guard(rightStartSent)
    val leftStartTrans = Transition(startId, leftStartId, leftStartGuard)
    val rightStartTrans = Transition(startId, rightStartId, rightStartGuard)

    val finalId = idg.getId
    val finalState = SNFAState(finalId)
    val states = (leftNFA.states ++ rightNFA.states) ++
      Map(startId -> startState, finalId -> finalState)

    val transitions = {
      var finalTrans: List[Transition] = List.empty

      val leftFinalTransitions = for (leftFinal <- leftNFA.finals) yield {
        val leftFinalSent = SentenceConstructor.getNewEpsilonSentence
        val leftFinalGuard = Guard(leftFinalSent)
        val leftFinalTrans = Transition(leftFinal, finalId, leftFinalGuard)
        leftFinalTrans
      }
      finalTrans = leftFinalTransitions.toList ::: finalTrans

      val rightFinalTransitions = for (rightFinal <- rightNFA.finals) yield {
        val rightFinalSent = SentenceConstructor.getNewEpsilonSentence
        val rightFinalGuard = Guard(rightFinalSent)
        val rightFinalTrans = Transition(rightFinal, finalId, rightFinalGuard)
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
      val loopGuard = Guard(loopSent)
      val loopTrans = Transition(subFinal, subNFA.start, loopGuard)
      loopTrans
    }

    val startId = idg.getId
    val startState = SNFAState(startId)
    val finalId = idg.getId
    val finalState = SNFAState(finalId)

    val finalTransitions = for (subFinal <- subNFA.finals) yield {
      val finalsSent = SentenceConstructor.getNewEpsilonSentence
      val finalsGuard = Guard(finalsSent)
      val finalsTrans = Transition(subFinal, finalId, finalsGuard)
      finalsTrans
    }

    val startFinalSent = SentenceConstructor.getNewEpsilonSentence
    val startFinalGuard = Guard(startFinalSent)
    val startFinalTrans = Transition(startId, finalId, startFinalGuard)

    val startsSent = SentenceConstructor.getNewEpsilonSentence
    val startsGuard = Guard(startsSent)
    val startsTrans = Transition(startId, subNFA.start, startsGuard)

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
    val transitions = completedSNFA.transitions
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

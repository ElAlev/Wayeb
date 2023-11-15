package fsm.symbolic.sra.nsra

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.TransitionOutput
import fsm.symbolic.TransitionOutput.TransitionOutput
import fsm.symbolic.logic._
import fsm.symbolic.sfa.IdGenerator
import fsm.symbolic.sra.{SRAGuard, SRATransition}
import fsm.symbolic.sre.RegularOperator.{CHOICE, ITER, RegularOperator, SEQ}
import fsm.symbolic.sre.{SREFormula, SREOperator, SRESentence}

object NSRAUtils extends LazyLogging {
  /**
   * Creates a nSRA from a formula. It first creates an ID generator for the labels of the states.
   *
   * @param f The original formula.
   * @return The nSRA corresponding to f.
   */
  def buildNSRA(f: SREFormula): NSRA = {
    val idg = IdGenerator()
    //val registers = f.getRegisterVariables.map(_.toString)
    buildNSRA(f, idg)
  }

  /**
   * Creates a windowed nSRA from a formula and a window.
   *
   * @param f The original formula.
   * @param window The given window.
   * @return The nSRA corresponding to f and satisfying the window constraint.
   */
  def buildUnrolledNSRA(
                         f: SREFormula,
                         window: Int
                       ): NSRA = {
    val NSRAWithEpsilon = buildNSRA(f)
    val NSRAWithoutEpsilon = eliminateEpsilon(NSRAWithEpsilon)
    val singleRegisterNSRA = convertToSingleRegister(NSRAWithoutEpsilon)
    val unrolledNSRA = unroll(singleRegisterNSRA, window)
    unrolledNSRA
  }

  def buildStreamingNSRANoWindow(f: SREFormula): NSRA = {
    val NSRAWithEpsilon = buildNSRA(f)
    val streamingNSRA = addInitialSelfLoop(NSRAWithEpsilon)
    val NSRAWithoutEpsilon = eliminateEpsilonWithTracker(streamingNSRA)//eliminateEpsilon(streamingNSRA)
    val singleRegisterNSRA = convertToSingleRegister(NSRAWithoutEpsilon)
    singleRegisterNSRA
  }

  /**
   * Creates a streaming windowed nSRA from a formula and a window.
   * First creates the windowed nSRA, then adds a new start state with a TRUE self-loop and an epsilon transition to the
   * old start state.
   *
   * @param f      The original formula.
   * @param window The given window.
   * @return The streaming nSRA corresponding to f and satisfying the window constraint.
   */
  def buildNSRAForStream(
                          f: SREFormula,
                          window: Int
                        ): NSRA = {
    //val trueFormula = new SRETrueSentence
    //val sigmaStar = SREOperator(ITER, List(trueFormula))
    //val streamFormula = SREOperator(SEQ, List(sigmaStar, f))
    //buildNSRA(streamFormula, window)
    val unrolledNSRA = buildUnrolledNSRA(f, window)
    val nsraWithSkip = addInitialSelfLoop(unrolledNSRA)
    val elimNSRA = eliminateEpsilon(nsraWithSkip)
    elimNSRA
  }

  def addInitialSelfLoop(nsra: NSRA): NSRA = {
    val newStateId = nsra.states.keySet.max + 1
    val newState = NSRAState(newStateId)
    val newEpsilonTransition = SRATransition(
      newStateId,
      nsra.start,
      SRAGuard(SentenceConstructor.getNewEpsilonSentence)
    )
    val newTrueTransition = SRATransition(
      newStateId,
      newStateId,
      SRAGuard(SentenceConstructor.getNewTrueSentence),
      TransitionOutput.IGNORE
    )
    val newStates = nsra.states + (newStateId -> newState)
    val newTransitions = newTrueTransition :: newEpsilonTransition :: nsra.transitions
    val nsraWithSkip = NSRA(newStates, newTransitions, newStateId, nsra.finals)
    nsraWithSkip
  }

  /**
   * Eliminates all epsilon transitions from a nSRA. See Algorithm 5 in thesis.
   *
   * @param nsra The original nSRA, possibly with epsilon transitions.
   * @return An equivalent nSRA without any epsilon transitions.
   */
  def eliminateEpsilon(nsra: NSRA): NSRA = {
    var nonEpsilonStatesIds: Set[Int] = Set.empty[Int]
    var nonEpsilonTransitions: List[SRATransition] = List.empty[SRATransition]
    var nonEpsilonFinalsIds: Set[Int] = Set.empty[Int]
    //var dead = -1

    val nonEpsilonStartStateId = 0
    nonEpsilonStatesIds += nonEpsilonStartStateId
    var nextNonEpsilonStateId = 1
    val nonEpsilonStartSet = nsra.enclose(nsra.start)
    var id2set: Map[Int,Set[Int]] = Map(nonEpsilonStartStateId -> nonEpsilonStartSet)
    var set2id: Map[Set[Int], Int] = Map(nonEpsilonStartSet -> nonEpsilonStartStateId)
    if (nonEpsilonStartSet.intersect(nsra.finals).nonEmpty) nonEpsilonFinalsIds = nonEpsilonFinalsIds + nonEpsilonStartStateId
    var frontier: Set[Int] = Set(nonEpsilonStartStateId)

    while (frontier.nonEmpty) {
      val frontierSampleStateId = frontier.head
      val frontierSampleStateSet = id2set(frontierSampleStateId)
      for (epsilonState <- frontierSampleStateSet) {
        val transitionsFromCurrentState = nsra.transitions.filter(t => t.source == epsilonState & !t.isEpsilon)
        for (transitionFromCurrentState <- transitionsFromCurrentState) {
          val nonEpsilonTargetStateSet = nsra.enclose(transitionFromCurrentState.target)
          if (set2id.contains(nonEpsilonTargetStateSet)) {
            val nonEpsilonTargetStateId = set2id(nonEpsilonTargetStateSet)
            val newTransition = SRATransition(
              frontierSampleStateId,
              nonEpsilonTargetStateId,
              SRAGuard(transitionFromCurrentState.guard.sentence),
              transitionFromCurrentState.output,
              transitionFromCurrentState.writeRegisters
            )
            nonEpsilonTransitions = newTransition :: nonEpsilonTransitions
          }
          else {
            id2set = id2set + (nextNonEpsilonStateId -> nonEpsilonTargetStateSet)
            set2id = set2id + (nonEpsilonTargetStateSet -> nextNonEpsilonStateId)
            nonEpsilonStatesIds += nextNonEpsilonStateId
            if (nonEpsilonTargetStateSet.intersect(nsra.finals).nonEmpty) nonEpsilonFinalsIds = nonEpsilonFinalsIds + nextNonEpsilonStateId
            val newTransition = SRATransition(
              frontierSampleStateId,
              nextNonEpsilonStateId,
              SRAGuard(transitionFromCurrentState.guard.sentence),
              transitionFromCurrentState.output,
              transitionFromCurrentState.writeRegisters
            )
            nonEpsilonTransitions = newTransition :: nonEpsilonTransitions
            frontier += nextNonEpsilonStateId
            nextNonEpsilonStateId += 1
          }
        }
      }
      frontier = frontier - frontierSampleStateId
    }

    val states = nonEpsilonStatesIds.map(s => (s,NSRAState(s))).toMap
    val nonEpsilonNSRA = NSRA(states, nonEpsilonTransitions, nonEpsilonStartStateId, nonEpsilonFinalsIds)
    //nsra
    nonEpsilonNSRA
  }

  def eliminateEpsilonWithTracker(nsra: NSRA): NSRA = {
    var states = Map[Int, NSRAState]()
    var transitions = List[SRATransition]()
    var finals = Set[Int]()
    var dead = -1

    val tracker = new Tracker()
    val starte = nsra.start
    val startd = nsra.enclose(starte)
    tracker.addStateToSee(startd)

    while (tracker.hasStatesToSee) {
      val states2See = tracker.getStatesToSee
      for ((newId, stateSet) <- states2See) {
        val newNSRAState = NSRAState(newId)
        states = states + (newId -> newNSRAState)
        tracker.addSeenState(newId)
        val successors = buildSuccessorsForState(nsra, stateSet)
        for ((pred, (nextSet, output, writeRegisters)) <- successors) {
          val nextId = tracker.addStateToSee(nextSet)
          val newTransition = SRATransition(newId, nextId, SRAGuard(pred), output, writeRegisters)
          transitions = newTransition :: transitions
        }
        if (stateSet.intersect(nsra.finals).nonEmpty) finals = finals + newId
        if (stateSet.isEmpty) dead = newId
      }

    }
    val elnsra = NSRA(states, transitions, 0, finals)
    elnsra
  }

  private def buildSuccessorsForState(
                                       nsra: NSRA,
                                       stateSet: Set[Int]
                                     ): Map[Sentence, (Set[Int], TransitionOutput, Set[String])] = {
    val relevantTransitions = nsra.transitions.filter(t => stateSet.contains(t.source) & !t.guard.sentence.isInstanceOf[EpsilonSentence])
    // From all transitions, we need to isolate the sentences. Some transitions may have the same sentence/output.
    // These should have the same target state.
    val sentencesOutputs = relevantTransitions.map(t => (t.guard.sentence, t.output)).toSet
    // We need to find the write registers for each sentence/output pair. We need to gather all the write registers
    // from all transitions which have the same sentence/output.
    val sentencesOutputsRegisters: Set[(Sentence, TransitionOutput, Set[String])] = sentencesOutputs.map(so => {
      val registers = relevantTransitions.filter(t => (t.guard.isSentence(so._1) & t.output == so._2)).flatMap(r => r.writeRegisters).toSet
      (so._1, so._2, registers)
    })
    val succ1 = sentencesOutputsRegisters.map(sor => {
      val states = relevantTransitions.filter(t => t.guard.isSentence(sor._1)).flatMap(x => nsra.enclose(x.target)).toSet
      (sor._1, (states, sor._2, sor._3))
    }).toMap
    succ1
  }

  /**
   * Should convert a multi-register nSRA to an equivalent single-register nSRA.
   * Currently just checks whether the given nSRA is single-register.
   *
   * @param nsra The original nSRA.
   * @return The same nSRA if it is single-register, otherwise throws an exception.
   */
  private def convertToSingleRegister(nsra: NSRA): NSRA = {
    if (ensureSingleRegister(nsra)) nsra
    else {
      logger.error("NSRA is multi-register.")
      throw new IllegalArgumentException("NSRA is multi-register.")
    }
  }

  /**
   * Checks whether a nSRA is single-register.
   *
   * @param nsra The nSRA to check.
   * @return True if the nSRA is single-register, false otherwise.
   */
  private def ensureSingleRegister(nsra: NSRA): Boolean = {
    nsra.transitions.forall(t => t.writeRegisters.size <= 1)
  }

  /**
   * Unrolls a nSRA up to the given window.
   * Assumes that the given nSRA has no epsilon transitions and that the window is greater or equal to 0.
   *
   * @param nsra The given nSRA.
   * @param window The given window.
   * @return The unrolled nSRA.
   */
  def unroll(
              nsra: NSRA,
              window: Int
            ): NSRA = {
    require(window >= 0, "Window must greater or equal to zero.")
    require(nsra.transitions.forall(t => !t.isEpsilon), "Unrolling cannot be performed on NSRA with epsilon transitions")
    val stateIdGenerator = IdGenerator()
    val registerIdGenerator = IdGenerator()
    unroll(nsra, window, stateIdGenerator, registerIdGenerator)._1
  }

  /**
   * Unrolls a nSRA up to the given window, given state id and register id generators.
   * See Algorithm 8 in thesis.
   *
   * @param nsra The original nSRA.
   * @param window The given window.
   * @param stateGenerator The state id generator.
   * @param registerGenerator The register id generator.
   * @return The unrolled nSRA.
   */
  private def unroll(
                      nsra: NSRA,
                      window: Int,
                      stateGenerator: IdGenerator,
                      registerGenerator: IdGenerator
                    ): (NSRA, Set[Int], Map[Int, Int], Map[String, String]) = {
    require(window >= 0, "Window must greater or equal to zero.")
    require(nsra.transitions.forall(t => !t.isEpsilon), "Unrolling cannot be performed on NSRA with epsilon transitions")
    if (window == 0) unroll0(nsra)
    else unrollK(nsra, window, stateGenerator, registerGenerator)
  }

  /**
   * Unrolls a nSRA with a window of 0.
   * See Algorithm 9 in thesis.
   *
   * @param nsra The original nSRA.
   * @return The unrolled nSRA.
   */
  private def unroll0(
                       nsra: NSRA
                     ): (NSRA, Set[Int], Map[Int, Int], Map[String, String]) = {
    val newStateId = 0
    val copyOfQ: Map[Int, Int] = Map(newStateId -> nsra.start)
    val copyOfR: Map[String, String] = Map.empty
    val frontier: Set[Int] = Set(newStateId)
    val Qf: Set[Int] = if (nsra.finals.contains(nsra.start)) Set(newStateId) else Set.empty
    val newState = NSRAState(newStateId)
    val newStatesMap: Map[Int,NSRAState] = Map(newStateId -> newState)
    val nsra0 = NSRA(newStatesMap, List.empty[SRATransition], newStateId, Qf)
    (nsra0, frontier, copyOfQ, copyOfR)
  }

  /**
   * Unrolls a nSRA up to k, given state id and register id generators.
   * See Algorithm 10 in thesis.
   *
   * @param nsra              The original nSRA.
   * @param k                 The given window.
   * @param stateGenerator    The state id generator.
   * @param registerGenerator The register id generator.
   * @return The unrolled nSRA.
   */
  private def unrollK(
                       nsra: NSRA,
                       k: Int,
                       stateGenerator: IdGenerator,
                       registerGenerator: IdGenerator
                     ): (NSRA, Set[Int], Map[Int, Int], Map[String, String]) = {
    var (unrolledK1, frontier, copyOfQ, copyOfR) = unroll(nsra, k - 1, stateGenerator, registerGenerator)
    var nextFrontier: Set[Int] = Set.empty
    var Qk: Set[Int] = unrolledK1.states.keySet
    var Qkf: Set[Int] = unrolledK1.finals
    var Rk: Set[String] = unrolledK1.getWriteRegisters
    var Dk: List[SRATransition] = unrolledK1.transitions
    for (q <- frontier) {
      val qc = copyOfQ(q)
      val transitionsFromqc = nsra.transitions.filter(t => t.source == qc)
      for (d <- transitionsFromqc) {
        val qnew = stateGenerator.getIdCautiousImmut
        Qk += qnew
        copyOfQ += (qnew -> d.target)
        if (nsra.finals.contains(d.target)) Qkf += qnew
        val Rnew: Set[String] = {
          if (d.writeRegisters.isEmpty) Set.empty
          else {
            val rnew: String = "r" + registerGenerator.getIdCautiousImmut.toString
            Rk += rnew
            copyOfR += (rnew -> d.writeRegisters.head)
            Set(rnew)
          }
        }
        val phi: Sentence = d.guard.sentence
        val rs: Set[String] = phi.getRegisterSelection
        var rsnew: List[String] = List.empty
        var regMapping: Map[String, String] = Map.empty
        for (r <- rs) {
          val rlatest = findLastAppearance(r, q, unrolledK1, copyOfR)
          rsnew = rlatest :: rsnew
          regMapping += (r -> rlatest)
        }
        rsnew = rsnew.reverse
        val phinew: Sentence = constructNewPhi(phi, regMapping)
        val newTransition: SRATransition = SRATransition(q,qnew,SRAGuard(phinew),Rnew)
        Dk = newTransition :: Dk
        nextFrontier += qnew
      }
    }
    val newStatesMap: Map[Int, NSRAState] = Qk.map(q => (q,NSRAState(q))).toMap
    val unrolledK = NSRA(newStatesMap, Dk, unrolledK1.start, Qkf)
    (unrolledK, nextFrontier, copyOfQ, copyOfR)
  }

  /**
   * Constructs a new sentence with a new register selection (new arguments).
   * See lines 21-23 in Algorithm 10 of thesis.
   *
   * Steps:
   *  1. go inside (complex) sentence,
   *  2. for each atomic sentence, find predicate, register selection
   *  3. get predicate name and arguments,
   *  4. replace arguments
   *  5. construct new predicate as in fsm.symbolic.logic.LogicUtils.parsed2ActualSentence
   *
   * @param phi The original sentence.
   * @param newRegMapping A mapping from old to new register names.
   * @return A new sentence with the new register selection.
   */
  private[sra] def constructNewPhi(
                                    phi: Sentence,
                                    newRegMapping: Map[String, String]
                                  ): Sentence = {
    phi match {
      case AtomicSentence(p, registerSelection) => {
        val fullPredicateName = phi.extractPredicateSymbols.head
        val predicateName = fullPredicateName.split("\\.").reverse.head
        val predicateArguments = p.arguments
        val newArguments = predicateArguments.map(arg => {
          if (newRegMapping.contains(arg)) newRegMapping(arg)
          else arg
        })
        val newPredicate = PredicateConstructor.getNewPredicateInstance(predicateName, newArguments)
        val newRegisterSelection = registerSelection.map(reg => {
          if (newRegMapping.contains(reg)) newRegMapping(reg)
          else reg
        })
        val newSentence = AtomicSentence(newPredicate, newRegisterSelection)
        newSentence
      }
      case ComplexSentence(op, inputSentences) => {
        val newSubSentences = inputSentences.map(s => constructNewPhi(s, newRegMapping))
        val newSentence = ComplexSentence(op, newSubSentences)
        newSentence
      }
    }
  }

  /**
   * Returns a register that is a copy of r and appears last in the trail of Ak1 to q
   * (no other copies of r appear after rlatest ).
   * Due to the construction, only a single walk/trail to q exists.
   *
   * See line 22 in Algorithm 10 of thesis.
   *
   * @param r The original register r.
   * @param q The state q.
   * @param Ak1 The nSRA Ak1.
   * @param copyOfR See Algorithm 10.
   * @return a register that is a copy of r and appears last in the trail of Ak1 to q.
   */
  private def findLastAppearance(
                                  r: String,
                                  q: Int,
                                  Ak1: NSRA,
                                  copyOfR: Map[String, String]
                                ): String = {
    var previousq = q
    var foundCopy = false
    var rlatest = ""
    while (previousq != Ak1.start & !foundCopy) {
      // there must exist exactly one transition targeting previousq,
      // otherwise Ak1 has not been unrolled properly
      val currentTransition = Ak1.transitions.filter(t => t.target == previousq).head
      for (currentr <- currentTransition.writeRegisters) {
        val rcopy = copyOfR(currentr)
        if (rcopy == r) {
          rlatest = currentr
          foundCopy = true
        }
      }
      previousq = currentTransition.source
    }
    if (previousq == Ak1.start & !foundCopy) throw new Error("findLastAppearance reached start state")
    rlatest
  }

  /**
   * Creates a nSRA from a formula and an id generator.
   *
   * @param formula The original formula.
   * @param idg The id generator.
   * @return The nSRA.
   */
  private def buildNSRA(
                         formula: SREFormula,
                         idg: IdGenerator
                       ): NSRA = {
    formula match {
      case SRESentence(_, _) =>
        buildNSRAFromSentence(formula, idg, formula.isMarked)
      case SREOperator(op: RegularOperator, formulas: List[SREFormula]) => {
        op match {
          case SEQ => buildNSRAFromSEQ(formulas, idg)
          case CHOICE => buildNSRAFromOR(formulas, idg)
          case ITER => buildNSRAFromITER(formulas, idg)
          case _ => throw new IllegalArgumentException("Unknown/unsupported operator " + op)
        }
      }
    }
    //NSRA(Map.empty,List.empty,0,Set(0))
  }

  /**
   * Creates NSRA from sequence. Connects the two sub NFAs by inserting an epsilon transition from the finals of left
   * NFA to the start state of right NFA
   *
   * @param formulas The left and right sub-formulas of the sequence operator.
   * @param idg The ID generator.
   * @return The NSRA for the sequence operator.
   */
  private def buildNSRAFromSEQ(
                                formulas: List[SREFormula],
                                idg: IdGenerator
                              ): NSRA = {
    require(formulas.size == 2)
    val leftSRA = buildNSRA(formulas.head, idg)
    val rightSRA = buildNSRA(formulas(1), idg)
    val finalTransitions = for (leftFinal <- leftSRA.finals) yield {
      val rightStartId = rightSRA.start
      val sentence = SentenceConstructor.getNewEpsilonSentence
      val guard = SRAGuard(sentence)
      val transition = SRATransition(leftFinal, rightStartId, guard)
      transition
    }
    val transitions = finalTransitions.toList ::: (leftSRA.transitions ::: rightSRA.transitions)
    val states = leftSRA.states ++ rightSRA.states
    NSRA(states, transitions, leftSRA.start, rightSRA.finals)
  }

  /**
   * Creates NSRA from disjunction.
   *   Creates a new start state.
   *   From this start state, inserts epsilon transitions to the start states of the sub NFAs.
   *   Creates a new final state.
   *   From some final of the sub NFAs, inserts epsilon transitions to the new final state.
   *
   * @param formulas The left and right sub-formulas of the or operator.
   * @param idg The ID generator.
   * @return The NSRA for the or operator.
   */
  private def buildNSRAFromOR(
                               formulas: List[SREFormula],
                               idg: IdGenerator
                             ): NSRA = {
    require(formulas.size == 2)

    val leftSRA = buildNSRA(formulas.head, idg)
    val rightSRA = buildNSRA(formulas(1), idg)

    val startId = idg.getIdCautiousImmut
    val startState = NSRAState(startId)
    val leftStartId = leftSRA.start
    val rightStartId = rightSRA.start
    val leftStartSent = SentenceConstructor.getNewEpsilonSentence
    val rightStartSent = SentenceConstructor.getNewEpsilonSentence
    val leftStartGuard = SRAGuard(leftStartSent)
    val rightStartGuard = SRAGuard(rightStartSent)
    val leftStartTrans = SRATransition(startId, leftStartId, leftStartGuard)
    val rightStartTrans = SRATransition(startId, rightStartId, rightStartGuard)

    val finalId = idg.getIdCautiousImmut
    val finalState = NSRAState(finalId)
    val states = (leftSRA.states ++ rightSRA.states) ++
      Map(startId -> startState, finalId -> finalState)

    val transitions = {
      var finalTrans: List[SRATransition] = List.empty

      val leftFinalTransitions = for (leftFinal <- leftSRA.finals) yield {
        val leftFinalSent = SentenceConstructor.getNewEpsilonSentence
        val leftFinalGuard = SRAGuard(leftFinalSent)
        val leftFinalTrans = SRATransition(leftFinal, finalId, leftFinalGuard)
        leftFinalTrans
      }
      finalTrans = leftFinalTransitions.toList ::: finalTrans

      val rightFinalTransitions = for (rightFinal <- rightSRA.finals) yield {
        val rightFinalSent = SentenceConstructor.getNewEpsilonSentence
        val rightFinalGuard = SRAGuard(rightFinalSent)
        val rightFinalTrans = SRATransition(rightFinal, finalId, rightFinalGuard)
        rightFinalTrans
      }
      finalTrans = rightFinalTransitions.toList ::: finalTrans

      List(leftStartTrans, rightStartTrans) ::: finalTrans :::
        (leftSRA.transitions ::: rightSRA.transitions)
    }
    NSRA(states, transitions, startId, Set(finalId))
  }

  /**
   * Creates NSRA from iteration.
   *   Insert an epsilon transition from some final to the start state.
   *   Create a new start state.
   *   Create a new final state.
   *   Insert an epsilon transition from some final of sub NFA to new final.
   *   Insert an epsilon transition from new start to new final.
   *   Insert an epsilon transition from new start to start of sub NFA.
   *
   * @param formulas The sub-formula of the iteration operator (assumes only one exists).
   * @param idg The ID generator.
   * @return The NSRA for the iteration operator.
   */
  private def buildNSRAFromITER(
                                 formulas: List[SREFormula],
                                 idg: IdGenerator
                               ): NSRA = {
    require(formulas.size == 1)

    val subSRA = buildNSRA(formulas.head, idg)

    val loopTransitions = for (subFinal <- subSRA.finals) yield {
      val loopSent = SentenceConstructor.getNewEpsilonSentence
      val loopGuard = SRAGuard(loopSent)
      val loopTrans = SRATransition(subFinal, subSRA.start, loopGuard)
      loopTrans
    }

    val startId = idg.getIdCautiousImmut
    val startState = NSRAState(startId)
    val finalId = idg.getIdCautiousImmut
    val finalState = NSRAState(finalId)

    val finalTransitions = for (subFinal <- subSRA.finals) yield {
      val finalsSent = SentenceConstructor.getNewEpsilonSentence
      val finalsGuard = SRAGuard(finalsSent)
      val finalsTrans = SRATransition(subFinal, finalId, finalsGuard)
      finalsTrans
    }

    val startFinalSent = SentenceConstructor.getNewEpsilonSentence
    val startFinalGuard = SRAGuard(startFinalSent)
    val startFinalTrans = SRATransition(startId, finalId, startFinalGuard)

    val startsSent = SentenceConstructor.getNewEpsilonSentence
    val startsGuard = SRAGuard(startsSent)
    val startsTrans = SRATransition(startId, subSRA.start, startsGuard)

    val states = subSRA.states ++ Map(startId -> startState, finalId -> finalState)
    val transitions = loopTransitions.toList ::: finalTransitions.toList ::: List(startFinalTrans, startsTrans) ::: subSRA.transitions
    NSRA(states, transitions, startId, Set(finalId))
  }

  /**
   * Creates a nSRA from a formula that is a single sentence. Creates two states connected via the sentence.
   *
   * @param formula The original formula.
   * @param idg The id generator.
   * @return The nSRA.
   */
  private def buildNSRAFromSentence(
                                   formula: SREFormula,
                                   idg: IdGenerator,
                                   marked: Boolean
                                   ): NSRA = {
    val sentence = formula.getSentences.head
    val writeRegisters = formula.getRegisterVariables.map(_.toString)
    val registerSelection = sentence.getRegisterVariableArguments
    val startId = idg.getIdCautiousImmut
    val startState = NSRAState(startId)
    val finalId = idg.getIdCautiousImmut
    val finalState = NSRAState(finalId)
    val actualSentence = SentenceConstructor.getNewSentenceInstanceWithRegisterSelection(sentence, registerSelection)
    val guard = SRAGuard(actualSentence)
    val output = if (marked & !actualSentence.isInstanceOf[EpsilonSentence]) TransitionOutput.TAKE else TransitionOutput.IGNORE
    val transition = SRATransition(startId, finalId, guard, output, writeRegisters)
    val states: Map[Int, NSRAState] = Map(startId -> startState, finalId -> finalState)
    NSRA(states, List(transition), startId, Set(finalId))
  }

}

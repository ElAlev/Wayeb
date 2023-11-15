package fsm.symbolic.sre

import fsm.symbolic.sre.RegularOperator.RegularOperator
import fsm.symbolic.sre.SelectionStrategy.SelectionStrategy

object SelectionUtils {

  /**
    * Transforms a formula with no selection strategies to an equivalent formula where each SEQ and CHOICE node have
    * only two children. De-flattens SEQ and CHOICE nodes.
    *
    * @param f The original formula, as parsed by fsm.symbolic.sre.SREParser.
    * @return A de-flattened formula.
    */
  private[sre] def transformFormulaNoSelection(f: SREFormula): SREFormula = {
    f match {
      case x: SRESentence => x
      case x: SREOperator => {
        val op = x.op
        op match {
          case RegularOperator.SEQ => {
            val transformedSubFormulas = x.formulas.map(subformula => transformFormulaNoSelection(subformula))
            val transformedFormula = connectFormulasWithOperator(transformedSubFormulas, RegularOperator.SEQ)
            transformedFormula
          }
          case RegularOperator.CHOICE => {
            val transformedSubFormulas = x.formulas.map(subformula => transformFormulaNoSelection(subformula))
            val transformedFormula = connectFormulasWithOperator(transformedSubFormulas, RegularOperator.CHOICE)
            transformedFormula
          }
          case RegularOperator.ITER => {
            val transformedSubFormulas = x.formulas.map(subformula => transformFormulaNoSelection(subformula))
            val transformedFormula = SREOperator(RegularOperator.ITER, List(transformedSubFormulas.head))
            transformedFormula
          }
          case RegularOperator.NEG => {
            val transformedSubFormulas = x.formulas.map(subformula => transformFormulaNoSelection(subformula))
            val transformedFormula = SREOperator(RegularOperator.NEG, List(transformedSubFormulas.head))
            transformedFormula
          }
          case _ => throw new IllegalArgumentException("Cannot recognize operator " + op)
        }
      }
    }
  }

  /**
    * Applies internal transformations to a formula f, returns another formula containing only standard operators that
    * can be accommodated by the compiler, e.g., expressions with selection policies are converted to expressions
    * with standard operators.
    * Calls fsm.symbolic.sre.SREUtils#transformFormulaWithSelectionAux(fsm.symbolic.sre.SREFormula, scala.Enumeration.Value)
    * with STRICT selection strategy initially.
    *
    * R = any(R1;R2) -> R = R1;T*;R2 (or, in general, if R=any(R1;R2;...;Rn), it becomes R=R1;T*;R2;T*...T*;Rn)
    * R' = any(R*) -> R' = R;(T*;R)*
    * R = next(R1;R2) -> R = R1;!(T*;R2;T*);R2 + epsilon
    * R' = next(R*) -> R' = R;( !(T*;R;T*) ;R)* + epsilon
    *
    * @param f The formula to be transformed.
    * @return The transformed formula
    */
  private[sre] def transformFormulaWithSelection(f: SREFormula): SREFormula = transformFormulaWithSelectionAux(f, SelectionStrategy.STRICT)

  /**
    * Recursive function to apply transformations.
    *
    * @param f The formula to be transformed.
    * @param selectionStrategy The selection strategy to be applied.
    * @return The transformed formula
    */
  private def transformFormulaWithSelectionAux(
                                                f: SREFormula,
                                                selectionStrategy: SelectionStrategy
                                              ): SREFormula = {
    val trueFormula = new SRETrueSentence
    trueFormula.unmark()
    f match {
      case x: SRESentence => x
      case x: SREOperator => {
        val op = x.op
        op match {
          case RegularOperator.SEQ => {
            val transformedSubFormulas = x.formulas.map(subformula => transformFormulaWithSelectionAux(subformula, SelectionStrategy.STRICT))
            val transformedFormula =
              // case R = any(R1;R2) -> R = R1;T*;R2
              if (selectionStrategy == SelectionStrategy.ANY) connectFormulasWithTrueStar(transformedSubFormulas, trueFormula)
              // case R = next(R1;R2) -> R = R1;!(T*;R2;T*);R2
              else if (selectionStrategy == SelectionStrategy.NEXT) connectFormulasForNextSeq(transformedSubFormulas, trueFormula)
              else connectFormulasWithOperator(transformedSubFormulas, RegularOperator.SEQ)
            transformedFormula
          }
          case RegularOperator.CHOICE => {
            val transformedSubFormulas = x.formulas.map(subformula => transformFormulaWithSelectionAux(subformula, SelectionStrategy.STRICT))
            val transformedFormula = connectFormulasWithOperator(transformedSubFormulas, RegularOperator.CHOICE)
            transformedFormula
          }
          case RegularOperator.ITER => {
            val transformedSubFormulas = x.formulas.map(subformula => transformFormulaWithSelectionAux(subformula, SelectionStrategy.STRICT))
            val transformedFormula =
              // case R' = any(R*) -> R' = R;(T*;R)* + epsilon
              if (selectionStrategy == SelectionStrategy.ANY) connectFormulasForAnyIter(transformedSubFormulas, trueFormula)
              // case R' = next(R*) -> R' = R;( !(T*;R;T*) ;R)* + epsilon
              else if (selectionStrategy == SelectionStrategy.NEXT) connectFormulasForNextIter(transformedSubFormulas, trueFormula)
              else SREOperator(RegularOperator.ITER, List(transformedSubFormulas.head))
            transformedFormula
          }
          case RegularOperator.NEG => {
            val transformedSubFormulas = x.formulas.map(subformula => transformFormulaWithSelectionAux(subformula, SelectionStrategy.STRICT))
            val transformedFormula = SREOperator(RegularOperator.NEG, List(transformedSubFormulas.head))
            transformedFormula
          }
          case RegularOperator.ANY => {
            val transformedSubFormulas = x.formulas.map(subformula => transformFormulaWithSelectionAux(subformula, SelectionStrategy.ANY))
            val transformedFormula = transformedSubFormulas.head // ANY should have only one sub-formula
            transformedFormula
          }
          case RegularOperator.NEXT => {
            val transformedSubFormulas = x.formulas.map(subformula => transformFormulaWithSelectionAux(subformula, SelectionStrategy.NEXT))
            val transformedFormula = transformedSubFormulas.head // NEXT should have only one sub-formula
            transformedFormula
          }
        }
      }
    }
  }

  /**
    * Recursive function for applying transformation for formula any(R1;R2) to R' = R1;T*;R2
    *
    * @param formulas a list of formulas
    *                 if the size of list is less than 2, then the method throws exception
    * @param trueFormula the true formula
    * @return the transformed formula
    */
  private def connectFormulasWithTrueStar(
                                           formulas: List[SREFormula],
                                           trueFormula: SRETrueSentence
                                         ): SREFormula = {
    require(formulas.size >= 2)
    connectFormulasWithTrueStar(formulas.tail, formulas.head, trueFormula)
  }

  @scala.annotation.tailrec
  private def connectFormulasWithTrueStar(
                                           formulas: List[SREFormula],
                                           connectedFormula: SREFormula,
                                           trueFormula: SRETrueSentence
                                         ): SREFormula = {
    formulas match {
      case Nil => connectedFormula
      case head :: tail => {
        val newFormulaToConnect = head
        val trueStar = SREOperator(RegularOperator.ITER, List(trueFormula))
        val trueStartNew = SREOperator(RegularOperator.SEQ, List(trueStar, newFormulaToConnect))
        val newConnected = SREOperator(RegularOperator.SEQ, List(connectedFormula, trueStartNew))
        connectFormulasWithTrueStar(tail, newConnected, trueFormula)
      }
    }
  }

  /**
    * Recursive function for applying transformation for formula next(R1;R2) to R' = R1;!(T*;R2;T*);R2
    *
    * @param formulas a list of formulas
    *                 if the size of list is less than 2, then the method throws exception
    * @param trueFormula the true formula
    * @return the transformed formula
    */
  private def connectFormulasForNextSeq(
                                         formulas: List[SREFormula],
                                         trueFormula: SRETrueSentence
                                       ): SREFormula = {
    require(formulas.size >= 2)
    connectFormulasForNextSeq(formulas.tail, formulas.head, trueFormula)
  }

  @scala.annotation.tailrec
  private def connectFormulasForNextSeq(
                                         formulas: List[SREFormula],
                                         connectedFormula: SREFormula,
                                         trueFormula: SRETrueSentence
                                       ): SREFormula = {
    formulas match {
      case Nil => connectedFormula
      case head :: tail =>
        // R2 formula
        val newFormulaToConnect = head.cloneFormula()
        newFormulaToConnect.unmark()
        // T*
        val trueStar = SREOperator(RegularOperator.ITER, List(trueFormula))
        // T*;R2
        val subformula1 = SREOperator(RegularOperator.SEQ, List(trueStar, newFormulaToConnect))
        // T*;R2;T*
        val subformula2 = SREOperator(RegularOperator.SEQ, List(subformula1, trueStar))
        // !(T*;R2;T*)
        val notSubformula = SREOperator(RegularOperator.NEG, List(subformula2))
        // R1;!(T*;R2;T*)
        val subformula3 = SREOperator(RegularOperator.SEQ, List(connectedFormula, notSubformula))
        //R1;!(T*;R2;T*);R2
        val newConnected = SREOperator(RegularOperator.SEQ, List(subformula3, head))
        connectFormulasForNextSeq(tail, newConnected, trueFormula)
    }
  }

  /**
    * Transforms formula any(R*) to R' = R;(T*;R)* + epsilon
    *
    * @param formulas a list of formulas
    *                 if the size of list is not equal to 1, then the method throws exception
    * @param trueFormula the true formula
    * @return the transformed formula
    */
  private def connectFormulasForAnyIter(
                                         formulas: List[SREFormula],
                                         trueFormula: SRETrueSentence
                                       ): SREFormula = {
    // R' = R;(T*;R)* + epsilon
    require(formulas.size == 1)
    val r = formulas.head
    val trueStar = SREOperator(RegularOperator.ITER, List(trueFormula))
    val rightSubformula = SREOperator(RegularOperator.SEQ, List(trueStar, r))
    val subformulaStar = SREOperator(RegularOperator.ITER, List(rightSubformula))
    val rprimeNoEpsilon = SREOperator(RegularOperator.SEQ, List(r, subformulaStar))
    val rprime = SREOperator(RegularOperator.CHOICE, List(new SREEpsilonSentence, rprimeNoEpsilon))
    rprime
  }

  /**
    * Transforms the formula next(R*) to R' = R;( !(T*;R;T*) ;R)* + epsilon
    * @param formulas a list of formulas
    *                 if the size of list is not equal to 1, then the method throws exception
    * @param trueFormula the true formula
    * @return the transformed formula
    */
  private def connectFormulasForNextIter(
                                          formulas: List[SREFormula],
                                          trueFormula: SRETrueSentence
                                        ): SREFormula = {
    require(formulas.size == 1)
    val r = formulas.head
    // T*
    val trueStar = SREOperator(RegularOperator.ITER, List(trueFormula))
    // R;T*
    val fSeqTrueStar = SREOperator(RegularOperator.SEQ, List(r, trueStar))
    // T*;R;T*
    val trueStarSeqfSeqTrueStar = SREOperator(RegularOperator.SEQ, List(trueStar, fSeqTrueStar))
    // !(T*;R;T*)
    val notSubformula = SREOperator(RegularOperator.NEG, List(trueStarSeqfSeqTrueStar))
    // !(T*;R;T*) ;R
    val notSubformulaSeqFormula = SREOperator(RegularOperator.SEQ, List(notSubformula, r))
    // ( !(T*;R;T*) ;R)*
    val iteratedSubformula = SREOperator(RegularOperator.ITER, List(notSubformulaSeqFormula))
    // R;( !(T*;R;T*) ;R)*
    val rprimeNoEpsilon = SREOperator(RegularOperator.SEQ, List(r, iteratedSubformula))
    // R;( !(T*;R;T*) ;R)* + epsilon
    val rprime = SREOperator(RegularOperator.CHOICE, List(new SREEpsilonSentence, rprimeNoEpsilon))
    rprime
  }

  /**
    * Connects 2 or more formulas with SEQ or CHOICE operator
    * @param formulas a list of formulas
    *                 if the size of list is less than 2, then the method throws exception
    * @param op regular operator SEQ or CHOICE
    * @return a new formula
    */
  private def connectFormulasWithOperator(
                                           formulas: List[SREFormula],
                                           op: RegularOperator
                                         ): SREFormula = {
    require(formulas.size >= 2)
    require(op == RegularOperator.SEQ | op == RegularOperator.CHOICE)
    connectFormulasWithOperator(formulas.tail, formulas.head, op)
  }

  @scala.annotation.tailrec
  private def connectFormulasWithOperator(
                                           formulas: List[SREFormula],
                                           connectedFormula: SREFormula,
                                           op: RegularOperator
                                         ): SREFormula = {
    formulas match {
      case Nil => connectedFormula
      case head :: tail => {
        val newConnected = SREOperator(op, List(connectedFormula, head))
        connectFormulasWithOperator(tail, newConnected, op)
      }
    }
  }

}

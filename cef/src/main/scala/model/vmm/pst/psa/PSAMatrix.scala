package model.vmm.pst.psa

import breeze.linalg.eig.DenseEig
import breeze.linalg.{DenseMatrix, DenseVector, eig}
import model.vmm.SymbolWord

object PSAMatrix {
  /**
    * Constructor for PSA transition matrices.
    *
    * @param matrix The matrix.
    * @param state2row A map from PSA state labels to Markov chain state ids.
    * @param row2state A map from Markov chain state ids to PSA state labels.
    * @return A PSA matrix.
    */
  def apply(
             matrix: DenseMatrix[Double],
             state2row: Map[SymbolWord, Int],
             row2state: Map[Int, SymbolWord]
           ): PSAMatrix = new PSAMatrix(matrix, state2row, row2state)
}

/**
  * Transition matrix of a PSA.
  *
  * @param matrix The matrix.
  * @param state2row A map from PSA state labels to Markov chain state ids.
  * @param row2state A map from Markov chain state ids to PSA state labels.
  */
class PSAMatrix(
                 matrix: DenseMatrix[Double],
                 state2row: Map[SymbolWord, Int],
                 row2state: Map[Int, SymbolWord]
               ) {
  // some results about the eigenvalues and eigenvectors of the matrix
  // could be interesting in order to study the limiting behavior of the matrix, e.g., for long-term forecasting
  val eigen: DenseEig = eig(matrix)
  val eigenValues: DenseVector[Double] = eigen.eigenvalues
  val eigenValuesComplex: DenseVector[Double] = eigen.eigenvaluesComplex
  val eigenVectors: DenseMatrix[Double] = eigen.eigenvectors

  override def toString: String = {
    "MATRIX\n" + matrix.toString() +
      "\nEigenValues\n" + eigenValues +
      "\nEigenValuesComplex\n" + eigenValuesComplex +
      "\nEigenVectors\n" + eigenVectors +
      "\nRow2State\n" + row2state +
      "\nState2Row\n" + state2row
  }
}

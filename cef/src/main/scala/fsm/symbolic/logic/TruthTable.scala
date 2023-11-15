package fsm.symbolic.logic

/**
  * For each assignment of truth values to predicates, a truth table contains a single truth value.
  * Useful when, for a sentence, we need to know when it evaluates to true, i.e., for what assignments of values to its
  * predicates the sentence evaluates to true.
  *
  * @param rows A map of assignments with their truth value.
  */
case class TruthTable(rows: Map[Assignment, Boolean]) {

  /**
    * Checks whether this table "entails" another table, i.e., for every true row of this table, the row of the other
    * table must also be true.
    * Both tables must be the "same", i.e., they must have the same assignments.
    *
    * @param otherTable the other table to check.
    * @return True if this table entails the other table.
    */
  def entails(otherTable: TruthTable): Boolean = {
    val otherRows = otherTable.rows
    require(rows.keySet == otherRows.keySet)
    rows.forall(row => rowEntails(row._2, otherRows(row._1)))
  }

  /**
    * Checks if one row entails another, i.e., if row1 is true, so must be row2.
    *
    * @param row1 The truth value of the first row.
    * @param row2 The truth value of the second row.
    * @return True if row1 entails row2.
    */
  def rowEntails(
                  row1: Boolean,
                  row2: Boolean
                ): Boolean = if (!row1) true else row2

  override def toString: String = {
    rows.map(row => row._1.toString + "\t\t\t\t\t\t" + row._2).foldLeft("") { (acc, x) => acc + "\n" + x }
  }
}

package workflow.provider

import workflow.condition.Condition

abstract class AbstractProvider(conditions: List[Condition]) {

  private val checks: List[Boolean] = conditions.map(x => x.check())
  /*private var conditions = List[Condition]()

  def addCondition(cond: Condition): Unit = {
    conditions = cond :: conditions
  }*/

  def check(): List[Boolean] = checks

  def provide(): Object

}

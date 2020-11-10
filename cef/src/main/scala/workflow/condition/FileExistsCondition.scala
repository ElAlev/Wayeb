package workflow.condition

import java.nio.file.{Files, Paths}

/**
  * Simple condition that checks whether there exists a file at the given path.
  *
  * @param fn The given path.
  */
class FileExistsCondition(fn: String) extends Condition {

  override def check(): Boolean = Files.exists(Paths.get(fn))

}

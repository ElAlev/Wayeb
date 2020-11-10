package workflow.condition

/**
  * Providers may need to have some conditions to be checks, e.g., that a SRE file with patterns exists.
  * Some of these conditions may be very similar. In order to avoid code duplication, we can create conditions with the
  * checking code by extending this trait.
  */
trait Condition {
  /**
    * Every condition should implement this function. If it returns true (e.g., a file does indeed exist for a given
    * path), it means the condition is satisfied.
    *
    * @return True if the condition is satisfied,
    */
  def check(): Boolean
}

package estimator

import fsm.runtime.RunListener

/**
  * Abstract class from which all other estimators should inherit. It is a RunListener and all sub-classes should
  * implement newEventProcessed and shutdown methods. The general idea is that you first feed an estimator with a
  * training stream in order to build a first model (or some first structures) and then you call estimate to build
  * the final model.
  */
abstract class RunEstimator extends RunListener {

  def estimate(): Unit

}

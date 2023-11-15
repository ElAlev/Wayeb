package fsm.symbolic

import com.typesafe.scalalogging.LazyLogging

abstract class AutomatonState(val id: Int) extends Serializable with LazyLogging {

}

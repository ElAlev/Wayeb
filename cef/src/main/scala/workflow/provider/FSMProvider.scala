package workflow.provider

import fsm.FSMInterface
import workflow.condition.Condition

object FSMProvider {
  /**
    * Constructor for FSM provider.
    *
    * @param wrappedProvider The FSM provider to be wrapped. One of:
    *                        - workflow.provider.DFAProvider;
    *                        - workflow.provider.SDFAProvider;
    *                        - workflow.provider.SPSAProvider;
    *                        - workflow.provider.SPSTProvider.
    * @param conditions A list of conditions that must be checked and satisfied.
    * @return A FSM provider.
    */
  def apply(
             wrappedProvider: AbstractProvider,
             conditions: List[Condition]
           ): FSMProvider =
    new FSMProvider(wrappedProvider, conditions)

  /**
    * Constructor for FSM provider with no conditions.
    *
    * @param wrappedProvider The FSM provider to be wrapped. One of:
    *                        - workflow.provider.DFAProvider;
    *                        - workflow.provider.SDFAProvider;
    *                        - workflow.provider.SPSAProvider;
    *                        - workflow.provider.SPSTProvider.
    * @return A FSM provider.
    */
  def apply(wrappedProvider: AbstractProvider): FSMProvider =
    new FSMProvider(wrappedProvider, List.empty[Condition])
}

/**
  * Wrapper for the various FSM providers.
  *
  * @param wrappedProvider The FSM provider to be wrapped. One of:
  *                        - workflow.provider.DFAProvider;
  *                        - workflow.provider.SDFAProvider;
  * @param conditions A list of conditions that must be checked and satisfied.
  */
class FSMProvider(
                   val wrappedProvider: AbstractProvider,
                   conditions: List[Condition]
                 ) extends AbstractProvider(conditions) {

  /**
    * Checks the type of provider and calls its provide() method.
    *
    * @return A list of FSM interfaces.
    */
  override def provide(): List[FSMInterface] = {
    wrappedProvider match {
      case x: DFAProvider => x.provide()
      case x: SDFAProvider => x.provide()
      case _ => throw new IllegalArgumentException("FSM provider not recognized")
    }
  }
}

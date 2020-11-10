package workflow.provider.source.dfa

import fsm.CountPolicy.CountPolicy

object DFASourceFromXML {

  def apply(
             xmlFile: String,
             policy: CountPolicy,
             order: Int,
             streamSymbols: Set[String]
           ): DFASourceFromXML = new DFASourceFromXML(
    xmlFile,
    policy,
    order,
    streamSymbols
  )
}

class DFASourceFromXML(
                        val xmlFile: String,
                        val policy: CountPolicy,
                        val order: Int,
                        val streamSymbols: Set[String]
                      ) extends DFASource {

}

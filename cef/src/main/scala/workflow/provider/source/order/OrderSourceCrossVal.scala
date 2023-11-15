package workflow.provider.source.order

import fsm.CountPolicy.CountPolicy
import stream.source.StreamSource

class OrderSourceCrossVal(
                           val fsmType: String,
                           val patternFile: String,
                           val declarations: String,
                           val streamSource: StreamSource,
                           val policy: CountPolicy
                         ) extends OrderSource {

}

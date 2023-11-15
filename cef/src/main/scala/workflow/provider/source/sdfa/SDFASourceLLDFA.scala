package workflow.provider.source.sdfa

object SDFASourceLLDFA {
  def apply(
             states: Set[Int],
             transitions: Set[(Int, Int, String)],
             start: Int,
             finals: Set[Int],
             order: Int,
             streaming: Boolean,
             disambiguate: Boolean,
             partitionAttribute: String
           ): SDFASourceLLDFA = new SDFASourceLLDFA(states, transitions, start, finals, order, streaming, disambiguate, partitionAttribute)

  def apply(
             states: Set[Int],
             transitions: Set[(Int, Int, String)],
             start: Int,
             finals: Set[Int],
             order: Int,
             streaming: Boolean
           ): SDFASourceLLDFA = new SDFASourceLLDFA(states, transitions, start, finals, order, streaming, false, "$")

  def apply(
             states: Set[Int],
             transitions: Set[(Int, Int, String)],
             start: Int,
             finals: Set[Int],
             order: Int
           ): SDFASourceLLDFA = new SDFASourceLLDFA(states, transitions, start, finals, order, true, false, "$")

  def apply(
             states: Set[Int],
             transitions: Set[(Int, Int, String)],
             start: Int,
             finals: Set[Int]
           ): SDFASourceLLDFA = new SDFASourceLLDFA(states, transitions, start, finals, 0, true, false, "$")
}

class SDFASourceLLDFA(
                       val states: Set[Int],
                       val transitions: Set[(Int, Int, String)],
                       val start: Int,
                       val finals: Set[Int],
                       val order: Int,
                       val streaming: Boolean,
                       val disambiguate: Boolean,
                      val partitionAttribute: String
                     ) extends SDFASource {
  require(states.forall(state => state > 0))
  require(states.contains(start))
  require(finals.subsetOf(states))
  require(transitions.map(t => t._1).subsetOf(states) & transitions.map(t => t._2).subsetOf(states))
  require(order >= 0)
}

package workflow.provider.source.snfa

import fsm.classical.pattern.regexp.RegExpTree

object SNFASourceRegExp {
  def apply(
             re: RegExpTree,
             partitionAttribute: String,
             window: Int
           ): SNFASourceRegExp = new SNFASourceRegExp(re, partitionAttribute, window)
}

class SNFASourceRegExp(
                        val re: RegExpTree,
                        val partitionAttribute: String,
                        val window: Int
                      ) extends SNFASource {

}

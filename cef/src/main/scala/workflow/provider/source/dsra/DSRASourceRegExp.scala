package workflow.provider.source.dsra

import fsm.classical.pattern.regexp.RegExpTree

object DSRASourceRegExp {
  def apply(
             re: RegExpTree,
             partitionAttribute: String,
             window: Int
           ): DSRASourceRegExp = new DSRASourceRegExp(re, partitionAttribute, window)
}

class DSRASourceRegExp(
                        val re: RegExpTree,
                        val partitionAttribute: String,
                        val window: Int
                      ) extends DSRASource {

}

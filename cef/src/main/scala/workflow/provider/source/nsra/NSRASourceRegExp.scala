package workflow.provider.source.nsra

import fsm.classical.pattern.regexp.RegExpTree

object NSRASourceRegExp {
  def apply(
             re: RegExpTree,
             partitionAttribute: String,
             window: Int
           ): NSRASourceRegExp = new NSRASourceRegExp(re, partitionAttribute, window)
}

class NSRASourceRegExp(
                        val re: RegExpTree,
                        val partitionAttribute: String,
                        val window: Int
                      ) extends NSRASource {

}

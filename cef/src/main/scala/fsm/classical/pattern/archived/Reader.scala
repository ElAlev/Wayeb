package fsm.classical.pattern.archived

import scala.collection.mutable.Set

class Reader() {

  def getModules(
      pattern: String,
      inputSymbols: Set[String]
  ): List[String] = {
    require(!inputSymbols.contains("#"))
    require(pattern.size > 0)
    var mods = List.empty[String]
    var mod = ""
    for (c <- pattern) {
      if (c == '#') {
        mods = mod :: mods
        mod = ""
      } else {
        if (!inputSymbols.contains(c.toString))
          throw new IllegalArgumentException("Pattern contains symbol not found in inputSymbols")
        mod += c
      }
    }
    mods = mod :: mods
    mods.reverse
  }

}

package stream.domain.bio

import com.typesafe.scalalogging.LazyLogging
import stream.source.LineParser
import stream.{GenericEvent, ResetEvent}

object BioLineParser extends LineParser with LazyLogging {

  override def line2Event(
                           line: Seq[String],
                           id: Int
                         ): GenericEvent = {
    try {
      val timestamp = line(0).toLong
      val eventType = "simpoint"
      if (timestamp == -1) ResetEvent(Map("simId" -> "dummy"))
      else {
        val aliveSymbol = line(1)
        val necroticSymbol = line(2)
        val apoptoticSymbol = line(3)
        val aliveNo = line(4).toDouble
        val necroticNo = line(5).toDouble
        val apoptoticNo = line(6).toDouble
        val prevAliveNo = line(7).toDouble
        val prevNecroticNo = line(8).toDouble
        val prevApoptoticNo = line(9).toDouble
        val interesting = line(10).toDouble

        val nextCETimestamp = line(11).toString

        val simId = line(12).toString

        val prevCellsNo = prevAliveNo + prevNecroticNo + prevApoptoticNo
        val prevAlivePercent = prevAliveNo.toDouble / prevCellsNo
        val prevNecroticPercent = prevNecroticNo.toDouble / prevCellsNo
        val prevPercentDiff = prevNecroticPercent - prevAlivePercent

        val cellsNo = aliveNo + necroticNo + apoptoticNo
        val alivePercent = aliveNo.toDouble / cellsNo
        val necroticPercent = necroticNo.toDouble / cellsNo
        val percentDiff = necroticPercent - alivePercent

        val percentDiffChange = percentDiff - prevPercentDiff

        val alivePercentChange = (aliveNo - prevAliveNo).toDouble / prevAliveNo
        val necroticPercentChange = (necroticNo - prevNecroticNo).toDouble / prevNecroticNo

        val ge = GenericEvent(id, eventType, timestamp,
          Map("aliveSymbol" -> aliveSymbol,
            "necroticSymbol" -> necroticSymbol,
            "apoptoticSymbol" -> apoptoticSymbol,
            "aliveNo" -> aliveNo,
            "necroticNo" -> necroticNo,
            "apoptoticNo" -> apoptoticNo,
            "prevAliveNo" -> prevAliveNo,
            "prevNecroticNo" -> prevNecroticNo,
            "prevApoptoticNo" -> prevApoptoticNo,
            "alivePercent" -> alivePercent,
            "necroticPercent" -> necroticPercent,
            "percentDiff" -> percentDiff,
            "prevPercentDiff" -> prevPercentDiff,
            "percentDiffChange" -> percentDiffChange,
            "alivePercentChange" -> alivePercentChange,
            "necroticPercentChange" -> necroticPercentChange,
            "interesting" -> interesting,
            "nextCETimestamp" -> nextCETimestamp,
            "simId" -> simId
          ))
        ge
      }
    } catch {
      case e: Exception => {
        logger.warn("COULD NOT PARSE LINE " + line)
        throw new Error
      }
    }
  }

}

package fsm.symbolic.logic.predicates

import fsm.symbolic.Valuation
import fsm.symbolic.logic.Predicate
import stream.GenericEvent
import utils.StringUtils.list2Str
import utils.SpatialUtils.distanceBetween

case class DistanceBetweenPredicate(override val arguments: List[String]) extends Predicate(arguments) {
  private val centerLon = arguments(0).toDouble
  private val centerLat = arguments(1).toDouble
  private val innerRadius = arguments(2).toDouble
  private val outerRadius = arguments(3).toDouble

  override def evaluate(
                         event: GenericEvent,
                         valuation: Valuation
                       ): Boolean = {
    if (event.hasAttribute("lon") & event.hasAttribute("lat")) {
      val lon = event.getValueOf("lon").toString.toDouble
      val lat = event.getValueOf("lat").toString.toDouble
      distanceBetween(lon, lat, centerLon, centerLat, innerRadius, outerRadius)
    } else false // in case a RESET event appears
  }

  override def toString: String = "DistanceBetweenPredicate(" + list2Str(arguments, ",") + ")"
}

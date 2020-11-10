package fsm.symbolic.sfa.logic.predicates

import fsm.symbolic.sfa.logic.Predicate
import stream.GenericEvent
import utils.StringUtils.list2Str
import utils.SpatialUtils.withinCircle

case class OutsideCirclePredicate(arguments: List[String]) extends Predicate {
  val centerLon: Double = arguments(0).toDouble
  val centerLat: Double = arguments(1).toDouble
  val radius: Double = arguments(2).toDouble

  override def evaluate(event: GenericEvent): Boolean = {
    val lon = event.getValueOf("lon").toString.toDouble
    val lat = event.getValueOf("lat").toString.toDouble
    !withinCircle(lon, lat, centerLon, centerLat, radius)
  }

  override def toString: String = "OutsideCirclePredicate(" + list2Str(arguments, ",") + ")"
}

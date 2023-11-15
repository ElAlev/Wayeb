package utils

import org.locationtech.jts.geom.Coordinate

/**
  * Utils for geospatial functions.
  */
object SpatialUtils {
  final private val earthRadiusMeters: Double = 6371.0 * 1000 // meters
  final private val earthRadiusKiloMeters: Double = 6371.0 // kilometers
  final private val earthRadiusMiles: Double = 3959.0 // miles

  /**
    * Estimates the Harvesine distance between two points in kilometers.
    *
    * @param lon1 Longitude of first point.
    * @param lat1 Latitude of first point.
    * @param lon2 Longitude of second point.
    * @param lat2 Latitude of second point.
    * @return The Harvesine distance.
    */
  def point2pointDist(
                       lon1: Double,
                       lat1: Double,
                       lon2: Double,
                       lat2: Double
                     ): Double = {
    val distance = harvesineDistanceKm(lon1, lat1, lon2, lat2)
    distance
  }

  /**
    * Determines whether the (Harvesine) distance between two points is smaller than a given threshold.
    *
    * @param lon1 Longitude of first point.
    * @param lat1 Latitude of first point.
    * @param lon2 Longitude of second point.
    * @param lat2 Latitude of second point.
    * @param max The threshold in kilometers.
    * @return True if the distande is smaller than the threshold.
    */
  def withinCircle(
                    lon1: Double,
                    lat1: Double,
                    lon2: Double,
                    lat2: Double,
                    max: Double
                  ): Boolean = {
    val distance = point2pointDist(lon1, lat1, lon2, lat2)
    distance < max
  }

  /**
    * Determines whether the (Harvesine) distance between two points is greater than a given threshold and smaller than
    * another given threshold.
    *
    * @param lon1 Longitude of first point.
    * @param lat1 Latitude of first point.
    * @param lon2 Longitude of second point.
    * @param lat2 Latitude of second point.
    * @param min The threshold of the minimum distance in kilometers.
    * @param max The threshold of the maximum distance in kilometers.
    * @return
    */
  def distanceBetween(
                       lon1: Double,
                       lat1: Double,
                       lon2: Double,
                       lat2: Double,
                       min: Double,
                       max: Double
                     ): Boolean = {
    val distance = point2pointDist(lon1, lat1, lon2, lat2)
    (distance > min) & (distance < max)
  }

  /**
    * Estimates the Harvesince distance between two points in meters.
    *
    * Haversine formula:
    * a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
    * c = 2 ⋅ atan2( √a, √(1−a) )
    * d = R ⋅ c
    * where 	φ is latitude, λ is longitude, R is earth’s radius (mean radius = 6,371km);
    * note that angles need to be in radians to pass to trig functions!
    *
    * JavaScript:
    *
    * var R = 6371e3; // metres
    * var φ1 = lat1.toRadians();
    * var φ2 = lat2.toRadians();
    * var Δφ = (lat2-lat1).toRadians();
    * var Δλ = (lon2-lon1).toRadians();
    *
    * var a = Math.sin(Δφ/2) * Math.sin(Δφ/2) +
    *         Math.cos(φ1) * Math.cos(φ2) *
    *         Math.sin(Δλ/2) * Math.sin(Δλ/2);
    * var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    *
    * var d = R * c;
    *
    * @param lon1 Longitude of first point.
    * @param lat1 Latitude of first point.
    * @param lon2 Longitude of second point.
    * @param lat2 Latitude of second point.
    * @return The Harvesine distance in meters.
    */
  private def harvesineDistanceMeters(
                                       lon1: Double,
                                       lat1: Double,
                                       lon2: Double,
                                       lat2: Double
                                     ): Double = {
    val phi1 = Math.toRadians(lat1)
    val phi2 = Math.toRadians(lat2)
    val deltaPhi = Math.toRadians(lat2 - lat1)
    val deltaLamda = Math.toRadians(lon2 - lon1)
    val a = (Math.sin(deltaPhi / 2) * Math.sin(deltaPhi / 2)) +
      (Math.cos(phi1) * Math.cos(phi2) * Math.sin(deltaLamda / 2) * Math.sin(deltaLamda / 2))
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    val d = earthRadiusMeters * c
    d
  }

  /**
    * Estimates the Harvesince distance between two points in kilometers.
    *
    * @param lon1 Longitude of first point.
    * @param lat1 Latitude of first point.
    * @param lon2 Longitude of second point.
    * @param lat2 Latitude of second point.
    * @return The Harvesine distance in kilometers.
    */
  private def harvesineDistanceKm(
                                   lon1: Double,
                                   lat1: Double,
                                   lon2: Double,
                                   lat2: Double
                                 ): Double = {
    harvesineDistanceMeters(lon1, lat1, lon2, lat2) / 1000
  }

  /**
    * Projects a given point with given speed and heading timeOffset seconds into the future.
    *
    * Given a start point, initial bearing, and distance, this will calculate the destination point and final bearing
    * travelling along a (shortest distance) great circle arc.
    *
    * Formula: 	φ2 = asin( sin φ1 ⋅ cos δ + cos φ1 ⋅ sin δ ⋅ cos θ )
    * λ2 = λ1 + atan2( sin θ ⋅ sin δ ⋅ cos φ1, cos δ − sin φ1 ⋅ sin φ2 )
    * where 	φ is latitude, λ is longitude, θ is the bearing (clockwise from north),
    * δ is the angular distance d/R; d being the distance travelled, R the earth’s radius
    *
    * See https://www.movable-type.co.uk/scripts/latlong.html
    *
    * @param lon The point's longitude.
    * @param lat The point's latitude.
    * @param timeOffset The time offset in seconds.
    * @param speed The vessel's speed (knots).
    * @param heading The vessel's heading.
    * @return (new longitude, new latitude)
    */
  def projectPoint(
                    lon: Double,
                    lat: Double,
                    timeOffset: Int,
                    speed: Double,
                    heading: Double
                  ): (Double, Double) = {
    val secondsInAnHour = 3600
    val distanceTravelledMiles = timeOffset * (speed / secondsInAnHour)
    val distanceTravelledKiloMeters = distanceTravelledMiles * 1.852
    val delta: Double =  distanceTravelledKiloMeters / earthRadiusKiloMeters.toInt
    val new_lat: Double = BigDecimal(Math.asin((Math.sin(lat.toRadians) * Math.cos(delta)) + (Math.cos(lat.toRadians) * Math.sin(delta) *
      Math.cos(heading.toRadians))).toDegrees).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
    val new_lon: Double = BigDecimal((lon.toRadians + Math.atan2(Math.sin(heading.toRadians) * Math.sin(delta) * Math.cos(lat.toRadians),
      Math.cos(delta) - (Math.sin(lat.toRadians) * Math.sin(new_lat.toRadians))
    )).toDegrees).setScale(6,
      BigDecimal.RoundingMode.HALF_UP
    ).toDouble
    (new_lon, new_lat)
  }

}

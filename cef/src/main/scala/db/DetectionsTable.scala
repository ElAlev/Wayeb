package db

import slick.jdbc.PostgresProfile.api._
import ui.ConfigUtils

/**
  * Table to store detected complex events.
  * For each complex event, we store its timestamp, the value of its partition attribute, the (final) state the
  * automaton was in when the event was detected and the input events that led to it.
  * @param tag
  */
class DetectionsTable(tag: Tag) extends Table[(Int, Long, String, Int, String)](tag, Some(ConfigUtils.detectionsSchema), ConfigUtils.detectionsTable) {
  def id = column[Int]("DET_ID", O.PrimaryKey) // This is the primary key column
  def ts = column[Long]("ts")
  def attr = column[String]("partitionval")
  def state = column[Int]("state")
  def events = column[String]("events")

  def * = (id, ts, attr, state, events)
}

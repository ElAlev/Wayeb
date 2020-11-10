package stream.source

object EmitMode extends Enumeration {
  type EmitMode = Value
  val BUFFER, ONLINE = Value
}

package utils.structures

import scala.reflect.ClassTag

object CyclicBuffer {
  def apply[T](size: Int): CyclicBuffer[T] = new CyclicBuffer[T](size)
}

class CyclicBuffer[T](val size: Int) {
  require(size > 0)
  /*def createBuffer(size: Int)(implicit m: ClassTag[T]): Array[T] = {
    val result = new Array[T](size)
    result
  }*/
  val buffer: scala.collection.mutable.Map[Int, T] =
    scala.collection.mutable.Map((0 until size).map(e => (e, None.asInstanceOf[T])).toMap.toSeq: _*)
  private val MaxHead = size - 1
  private var head = -1
  private var full = false

  /*private def createArray(arraySize: Int)(implicit m: ClassTag[T]): Array[T] = {
    val result = new Array[T](arraySize)
    result
  }*/

  def pushElement(element: T): (Int, Option[T]) = {
    val prevHead = head
    head = getNextHead
    val droppedElement: Option[T] = if (prevHead == -1 | buffer(head) == None) None else Some(buffer(head))
    buffer(head) = element
    if (head == MaxHead) full = true
    (head, droppedElement)
  }

  def pushWord(w: List[T]): Int = {
    val word = w.reverse // head of w is the most recent symbol, so we need to reverse it to start from the oldest
    require(word.size == size)
    pushAux(word, head)
  }

  @scala.annotation.tailrec
  private def pushAux(
                       l: List[T],
                       h: Int
                     ): Int = {
    l match {
      case head :: tail => pushAux(tail, pushElement(head)._1)
      case Nil => h
    }
  }

  def pop: List[T] = {
    //require(head != -1)
    if (head == -1) List.empty[T]
    // do not start popAux from head or you won't have a sound termination condition
    else popAux(getPreviousHead(head), List[T](buffer(head)))
  }

  @scala.annotation.tailrec
  private def popAux(
                      currentHead: Int,
                      output: List[T]
                    ): List[T] = {
    if ((currentHead == -1) | (currentHead == head)) output.reverse
    else popAux(getPreviousHead(currentHead), buffer(currentHead) :: output)
  }

  private def getNextHead: Int = {
    if (head < MaxHead) head + 1
    else 0
  }

  private def getPreviousHead(currentHead: Int): Int = {
    if (currentHead > 0) currentHead - 1
    else if (currentHead == 0) {
      if (full) MaxHead
      else -1
    } else -1
  }

  def clear(): Unit = {
    head = -1
    //buffer.mapValues(v => None)
    (0 until size).foreach(i => buffer.update(i, None.asInstanceOf[T]))
    full = false
  }

  override def toString: String = buffer.toString + "\t head=" + head + "\t full=" + full
}


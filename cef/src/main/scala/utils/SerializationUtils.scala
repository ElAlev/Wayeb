package utils

import java.io.{FileOutputStream, ObjectOutputStream}

/**
  * Utils for serializing objects.
  */
object SerializationUtils {
  /**
    * Serializes a list of objects and writes them to a file.
    *
    * @param l The list of objects.
    * @param fn The path to the file.
    * @tparam T The type of objects.
    */
  def write2File[T](
                     l: List[T],
                     fn: String
                   ): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fn))
    oos.writeObject(l)
    oos.close()
  }
}

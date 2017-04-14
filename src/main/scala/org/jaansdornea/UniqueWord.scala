

package org.jaansdornea

object UniqueWord extends App {
 
  val dictionary = Map[String, Set[String]]("b1g" -> Set("big", "bag"), "t5y" -> Set("thirsty"), "t1y" -> Set("toy"), "p1e" -> Set("pie", "poe"), "in" -> Set("in"))
  
  def isUniqueKey(word: String): Boolean = 
    if (word.length() <= 2) !dictionary.contains(word)
    else {
      val key = word.head + (word.length - 2).toString + word.takeRight(1)
      (!dictionary.contains(key) || (dictionary.get(key).get.contains(word) && dictionary.get(key).get.size == 1))
  }
  
  assert(!isUniqueKey("big"))
  assert(!isUniqueKey("in"))
  assert(isUniqueKey("on"))
  assert(isUniqueKey("thirsty"))
  assert(isUniqueKey("pot"))
}
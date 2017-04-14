package org.jaansdornea

object ReverseString extends App {
 
  def reverse(s: String): String = {
    (1 to (s.length)).foldLeft("")( (composed, index) => {
      composed + s(s.length - index)
    })
  }
  
  assert (reverse("hello") == "olleh")
  
}
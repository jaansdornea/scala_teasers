package org.jaansdornea
/*
 * s = "catsanddog",
dict = ["cat", "cats", "and", "sand", "dog"].

A solution is ["cats and dog", "cat sand dog"].
 */
object WordBreak extends App{
  
  def wordBreak(s: String, wordDict: List[String]): List[String] = {
    val words = wordDict.toSet
    //1. build a list of lists of Strings by taking words greedily and not taking words
    //    a.) given that we can take a word we start again
    
    var solutionSet = List[List[String]]()
    def build(current: String, rest: String, accum: List[String]): Unit = {
      if (rest.isEmpty) {
        if (words.contains(current)) {
          solutionSet = solutionSet :+ (accum :+ current)
        }
      } else if (words.contains(current)) {
        build("", rest, accum :+ current)
        build(current :+ rest.head, rest.tail, accum)
      } else {
        build(current :+ rest.head, rest.tail, accum)
      }
    }
    build("", s, List[String]())
    solutionSet.map( (sentence) => sentence.mkString(" ") )
  }
  
  println (wordBreak("catsanddog", List("cat", "cats", "and", "sand", "dog")))
}
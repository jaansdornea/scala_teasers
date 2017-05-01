package org.jaansdornea.utility

object TFIDF {
   def apply(corpus: Array[Array[String]]): TFIDF = {
     new TFIDF(corpus)
   }
}

class TFIDF(private val corpus: Array[Array[String]]) {
  private def tf(term: String, document: Array[String]): Int = {
    val loweredTerm = term.toLowerCase()
    document.map((t) => t.toLowerCase()).count((t) => t == loweredTerm)
  }
  private def invDF(term: String): Double = {
    val countDocumentsContainingTerm = corpus.count ( (document) => tf(term, document) > 0 )
    Math.log(corpus.length/ countDocumentsContainingTerm)
  }
  
  def on(term: String, document: Array[String]): Double = tf(term, document) * invDF(term)
}
object Levenshtein {
  def main(args : Array[String]) : Unit = {
    args match {
      case (Array(s, t)) =>
        println("Levenshtein('"+s+"', '"+t+"') = " + compute(s, t))
      case (ary) =>
      	println("Levenshtein: expecting 2 arguments! Got: "
      	    + ary.mkString(" "))
    }
    
  }
  
  def compute(start : String, target : String) : Int = {
    val slen = start.length()
    val tlen = target.length()
    
    return computeMatrix(start, target)(slen)(tlen)
  }
  
  def computeMatrix(start : String, target : String) : Array[Array[Int]] = {
    val slen = start.length()
    val tlen = target.length()
    var matrix = Array.ofDim[Int](slen+1, tlen+1);
    var matrixCost = Array.ofDim[Int](slen, tlen);
    
	for (i <- 0 to slen) matrix(i)(0) = i
    for (j <- 0 to tlen) matrix(0)(j) = j 

    for (i <- 0 until slen ; j <- 0 until tlen) {
      // Playing around with tuples+variable-init together...
      val (s, t) = (start(i), target(j))
      // ... inline if ... else ... 
      val cost = if (s == t) 0 else 1
      // ... functional-style Math.min() evaluation ...
      val costs = List(
          matrix(i)(j+1) + 1,
          matrix(i+1)(j) + 1,
          matrix(i)(j) + cost)
      val min = costs.reduceLeft(Math.min)
      
      matrix(i+1)(j+1) = min;
    }
    
	return matrix
  }
}

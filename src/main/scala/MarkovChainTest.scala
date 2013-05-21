import scala.annotation.tailrec

object MarkovChainTest {

  def main(args: Array[String]) {
    val lines = scala.io.Source.fromFile("psalms.txt").getLines()
    
    val chain = MarkovChain2

    var count:Int = 0
    val state = buildMarkovChain(chain, lines) //.filter(_.contains("and overpaid")))
    
    @tailrec
    def getUntilSentenceEnd(prev: Token, twoPrev: Token = null, list: List[Token] = List.empty): List[Token] = {
      val sampleFn = (if (twoPrev != null) chain.sampleNext((twoPrev, prev)) else chain.sampleNext(prev))
      val curr = sampleFn(state)._2
      curr match {
        case EndToken() => (curr :: prev :: list).reverse
        case _          =>  getUntilSentenceEnd(curr, prev, prev :: list)
      }
    }
    
    val formatter: PartialFunction[Token, String] = {
        case StringToken(v) => " " + v
        case EndToken()     => "."
        case _              => ""
    }
    
    for (_ <- 0 until 10) {
      val gen = getUntilSentenceEnd(chain.sample(state)._2).collect({case s:StringToken =>s; case e:EndToken => e}).foldLeft("")((s, t) => s + formatter(t))
      println(gen)
    }
  }
  
  
  def buildMarkovChain(chain: MarkovChain2.type, lines: Iterator[String]): MarkovChain2 = {
    var state = chain()
    val start = StartToken()
    state = chain.addToBase(start)(state)._1
    
    lines.foreach { line => 
      var lastone:Token = start
      var lasttwo:(Token, Token) = null
      val parts = line.split(" ")
      parts.filter(!_.isEmpty())foreach { p =>
        val part = new StringToken(p)
        if (lasttwo != null) {
            state = chain.add(part, lasttwo)(state)._1
        } else if (lastone != null) {
            state = chain.addToFirstOrder(part, lastone)(state)._1
        } else {
            state = chain.addToBase(part)(state)._1
        }
        
        if (lastone != null) {
          lasttwo = (lastone, part)
        }
        lastone = part
      }
        if (lasttwo != null) {
            state = chain.add(EndToken(), lasttwo)(state)._1
        } else if (lastone != null) {
            state = chain.addToFirstOrder(EndToken(), lastone)(state)._1
        }
    }
    state
  }
}
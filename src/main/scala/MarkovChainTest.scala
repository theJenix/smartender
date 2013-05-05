object MarkovChainTest {

  def main(args: Array[String]) {
//    val chain = new MarkovChain2
//    
    val state1 = new MarkovState("Test")
    val state2 = new MarkovState("One")
    val state3 = new MarkovState("Two")
    val state4 = new MarkovState("Three")
//
//    val chain2 = MarkovChain2.add(state3, (state1, state2)).apply(chain)._1
//    val chain3 = MarkovChain2.add(state4, (state1, state2)).apply(chain2)._1
//    println(chain3)
//    val picked1 = MarkovChain2.sample.apply(chain3)
//    println(picked1)
//    val picked2 = MarkovChain2.sample.apply(chain3)
//    println(picked2)
//    val picked3 = MarkovChain2.sample.apply(chain3)
//    println(picked3)

    val chain = MarkovChain2

    val s1 = chain.add(state3, (state1, state2))(new MarkovChain2)._1
    val s2 = chain.add(state4, (state1, state2))(s1)._1
    println(chain.sample(s2)._2)
    println(chain.sample(s2)._2)
    println(chain.sample(s2)._2)
    println(chain.sample(s2)._2)
  }
}
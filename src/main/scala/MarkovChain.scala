
import scala.collection.immutable.Set
import scala.collection.immutable.HashMap
import scalaz.State
import scala.util.Random

case class MarkovState(value: String,
                       count: Int = 0) {

  def incrementCount = MarkovState(value, count + 1)
}

case class MarkovChain2(base: Set[MarkovState]
                                        = Set[MarkovState](),
                        firstOrder: Map[MarkovState, Set[MarkovState]]
                                        = HashMap[MarkovState, Set[MarkovState]](),
                        secondOrder: Map[(MarkovState, MarkovState), Set[MarkovState]]
                                        = HashMap[(MarkovState, MarkovState), Set[MarkovState]]()
                       )

/**
 * Second order markov chain
 * 
 */
object MarkovChain2 {
  
  import State.gets
  import State.modify
  
  def addToMappedSet[K](m: Map[K, Set[MarkovState]], k: K, v: MarkovState) = {
      val set = m getOrElse (k, Set[MarkovState]())
      //return a modified map with pred mapped to the set that includes
      // the new state
      val oldV = set find (o => o.value == v.value) getOrElse v
      //NOTE: make sure we increment the count before adding to the set
      m + (k -> (set + oldV.incrementCount))
  }
  
  def addToBase(state: MarkovState): State[MarkovChain2, Unit] = {
    modify[MarkovChain2](s => s.copy(base = s.base + state.incrementCount))
  }
  
  def addToFirstOrder(state: MarkovState, pred: MarkovState): State[MarkovChain2, Unit] =  {
    modify[MarkovChain2](s => s.copy(firstOrder = addToMappedSet(s.firstOrder, pred, state)))
  }
  
  def addToSecondOrder(state: MarkovState, pred: (MarkovState, MarkovState)): State[MarkovChain2, Unit] =  {
    modify[MarkovChain2](s => s.copy(secondOrder = addToMappedSet(s.secondOrder, pred, state)))
  }

  def add(state: MarkovState, chain: (MarkovState, MarkovState)): State[MarkovChain2, Unit] = {
    for {
      _ <- addToBase(state)
      _ <- addToFirstOrder(state, chain._2)
      _ <- addToSecondOrder(state, chain)
    } yield ()
  }
  
  def sample: State[MarkovChain2, MarkovState] = {
    gets[MarkovChain2, MarkovState] (s => rouletteSample(s.base))
  }
  
  def sampleNext(pred: MarkovState): State[MarkovChain2, MarkovState] = {
    gets[MarkovChain2, MarkovState] (s => rouletteSample2(s.firstOrder, pred))
  }
  
  def sampleNext(pred: (MarkovState, MarkovState)): State[MarkovChain2, MarkovState] = {
    gets[MarkovChain2, MarkovState] (s => rouletteSample2(s.secondOrder, pred))
  }
  
  private def rouletteSample(set: Set[MarkovState]): MarkovState = {
    val rnd = new Random
    val total = set.foldLeft(0)((sum, s) => sum + s.count)

    // selected is 1 based
    val sel = rnd.nextInt(total) + 1
    
    var bounds = 0
    var selS:MarkovState = null
    set.iterator.takeWhile(_ => bounds < sel).foreach {
      s => 
      selS = s
      bounds += s.count
    }
    selS
  }
  
  private def rouletteSample2[K](map: Map[K,Set[MarkovState]], pred: K): MarkovState = {
    val set = map(pred)

    rouletteSample(set)
  }
}

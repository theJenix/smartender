
import scala.collection.immutable.Set
import scala.collection.immutable.HashMap
import scalaz.State

case class MarkovState(value: String, count: Int);

case class MarkovChain2(base: Set[MarkovState] = Set[MarkovState](),
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
  
  import State.get
  import State.modify
  
  def addToMappedSet[K](m: Map[K, Set[MarkovState]], k: K, v: MarkovState) = {
      val set = m getOrElse (k, Set[MarkovState]())
      //return a modified map with pred mapped to the set that includes
      // the new state
      val newV = for (oldV <- set find (o => o.value == v.value))
        yield MarkovState(oldV.value, oldV.count + 1)
      m + (k -> (set + newV.getOrElse(v)))
  }
  
  def addToBase(state: MarkovState): State[MarkovChain2, Unit] = {
    modify[MarkovChain2](s => s.copy(base = s.base + state))
  }
  
  def addToFirstOrder(state: MarkovState, pred: MarkovState): State[MarkovChain2, Unit] =  {
    modify[MarkovChain2](s => s.copy(firstOrder = addToMappedSet(s.firstOrder, pred, state)))
//        {
//      val set = s.firstOrder getOrElse (pred, Set[MarkovState]())
//      //return a modified map with pred mapped to the set that includes
//      // the new state
//      s.firstOrder + (pred -> (set + state))
//    }))
  }
  
  def addToSecondOrder(state: MarkovState, pred: (MarkovState, MarkovState)): State[MarkovChain2, Unit] =  {
    modify[MarkovChain2](s => s.copy(secondOrder = addToMappedSet(s.secondOrder, pred, state)))
//    {
//      val set = s.secondOrder getOrElse (pred, Set[MarkovState]())
//      //return a modified map with pred mapped to the set that includes
//      // the new state
//      s.secondOrder + (pred -> (set + state))
//    }))
//    
  }

  def add(state: MarkovState, chain: (MarkovState, MarkovState)): State[MarkovChain2, Unit] = {
    for {
      _ <- addToBase(state)
      _ <- addToFirstOrder(state, chain._2)
      _ <- addToSecondOrder(state, chain)
    } yield ()
  }
  
//  def sample: State[MarkovChain2, MarkovState] = {
//    
//    get[MarkovChain2].
//  }
}

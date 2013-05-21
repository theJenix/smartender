
import scala.collection.immutable.Set
import scala.collection.immutable.HashMap
import scalaz.State
import scala.util.Random

case class MarkovState(value: Token,
                       count: Int) {

  def incrementCount = MarkovState(value, count + 1)
}

object MarkovState {
  def apply(token: Token): MarkovState = MarkovState(token, 1)
}

sealed abstract class Token

case class EndToken                   extends Token
case class StartToken                 extends Token
case class StringToken(value: String) extends Token


case class MarkovChain2(base: Set[MarkovState]
                                        = Set[MarkovState](),
                        firstOrder: Map[Token, Set[MarkovState]]
                                        = HashMap[Token, Set[MarkovState]](),
                        secondOrder: Map[(Token, Token), Set[MarkovState]]
                                        = HashMap[(Token, Token), Set[MarkovState]]()
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
  
  def addToBase(token: Token): State[MarkovChain2, Unit] = {
    modify[MarkovChain2](s => s.copy(base = s.base + MarkovState(token)))
  }
  
  def addToFirstOrder(token: Token, pred: Token): State[MarkovChain2, Unit] =  {
    modify[MarkovChain2](s => s.copy(firstOrder = addToMappedSet(s.firstOrder, pred, MarkovState(token))))
  }
  
  def addToSecondOrder(token: Token, pred: (Token, Token)): State[MarkovChain2, Unit] =  {
    modify[MarkovChain2](s => s.copy(secondOrder = addToMappedSet(s.secondOrder, pred, MarkovState(token))))
  }

  def add(token: Token, chain: (Token, Token)): State[MarkovChain2, Unit] = {
    for {
//      _ <- addToBase(token)
      _ <- addToFirstOrder(token, chain._2)
      _ <- addToSecondOrder(token, chain)
    } yield ()
  }
  
  def sample: State[MarkovChain2, Token] = {
    gets[MarkovChain2, Token] (s => rouletteSample(s.base))
  }
  
  def sampleNext(pred: Token): State[MarkovChain2, Token] = {
    gets[MarkovChain2, Token] (s => rouletteSample2(s.firstOrder, pred))
  }
  
  def sampleNext(pred: (Token, Token)): State[MarkovChain2, Token] = {
    gets[MarkovChain2, Token] (s => rouletteSample2(s.secondOrder, pred))
  }
  
  private def rouletteSample(set: Set[MarkovState]): Token = {
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
    selS.value
  }
  
  private def rouletteSample2[K](map: Map[K,Set[MarkovState]], pred: K): Token = {
    val set = map(pred)

    rouletteSample(set)
  }
}

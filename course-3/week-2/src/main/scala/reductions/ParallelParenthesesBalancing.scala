package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = chars.map {
    case '(' => 1
    case ')' => -1
    case _ => 0
  }.fold(0)((a1, a2) => if (a1 < 0) Int.MinValue else a1 + a2) == 0

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    type Least = Int //- min value for subsequence
    type Total = Int //- total parentheses level for subsequence

    def traverse(idx: Int, until: Int, least: Least, total: Total): (Least, Total) = {
      if (idx >= until) (least, total) else chars(idx) match {
        case '(' => traverse(idx + 1, until, least, total + 1)
        case ')' => traverse(idx + 1, until, math.min(least, total - 1), total -1)
        case _ => traverse(idx + 1, until, least, total)
      }
    }

    def reduce(from: Int, until: Int) : (Least, Total) = {
      if (until - from <= threshold) traverse(from, until, 0, 0) else {
        val mid = (until + from) / 2
        val ((l1, t1), (l2, t2)) = parallel(reduce(from, mid), reduce(mid, until))
        (math.min(l1, t1 + l2), t1 + t2) //- scanning left to right
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}

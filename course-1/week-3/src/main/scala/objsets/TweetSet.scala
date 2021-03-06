package objsets

import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet

  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
  override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  override def filter(p: (Tweet) => Boolean): TweetSet = this

  override def union(that: TweetSet): TweetSet = that

  override def mostRetweeted: Tweet = throw new NoSuchElementException()

  override def descendingByRetweet: TweetList = Nil
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {

    def res = if (p(elem)) acc.incl(elem) else acc
    right.filterAcc(p, left.filterAcc(p, res))
  }



  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean = (x, elem) match {
    case (a, b) if a.text < b.text => left.contains(a)
    case (a, b) if a.text > b.text => right.contains(a)
    case (_, _) => true
  }

  def incl(x: Tweet): TweetSet = (x, elem) match {
    case (a, b) if a.text < b.text => new NonEmpty(b, left.incl(a), right)
    case (a, b) if b.text < a.text => new NonEmpty(b, left, right.incl(a))
    case (_, _) => this
  }

  def remove(tw: Tweet): TweetSet = (tw, elem) match {
    case (a, b) if a.text < b.text => new NonEmpty(b, left.remove(a), right)
    case (a, b) if b.text < a.text => new NonEmpty(b, left, right.remove(a))
    case (_, _) => left.union(right)
  }


  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  override def filter(p: (Tweet) => Boolean): TweetSet = filterAcc(p, new Empty)

  override def union(that: TweetSet): TweetSet =
    filterAcc(t => true, that)

  def mostRetweeted: Tweet = {
    def res =
      if (left.isInstanceOf[Empty]) elem
      else {
        val leftRetweeted: Tweet = left.mostRetweeted
        if (elem.retweets >= leftRetweeted.retweets) elem
        else leftRetweeted
      }

    if (right.isInstanceOf[Empty]) res
    else {
      val rightRetweeted: Tweet = right.mostRetweeted
      if (res.retweets >= rightRetweeted.retweets) res
      else rightRetweeted
    }
  }

  override def descendingByRetweet: TweetList = {
    def descendingAcc(set: TweetSet, acc: TweetList): TweetList = {
      if (set.isInstanceOf[Empty]) acc
      else {
        def retwitChampion = set.mostRetweeted
        new Cons(retwitChampion, descendingAcc(set.remove(retwitChampion), acc))
      }
    }

    descendingAcc(this, Nil)
  }
}

trait TweetList {
  def head: Tweet
  def headOption: Option[Tweet]
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true

  def headOption: Option[Tweet] = None
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false

  def headOption: Option[Tweet] = Some(head)
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val allTweets = TweetReader.allTweets

// more sophisticated implementation
  lazy val googleTweets: TweetSet = allTweets.filter(t =>
    google.map(s => t.text.contains(s)).foldLeft(false)(_ || _))
  lazy val appleTweets: TweetSet = allTweets.filter(t =>
    apple.map(s => t.text.contains(s)).foldLeft(false)(_ || _))

//  lazy val googleTweets: TweetSet = allTweets.filter(t =>
//    google.exists(s => t.text.contains(s)))
//  lazy val appleTweets: TweetSet = allTweets.filter(t =>
//    apple.exists(s => t.text.contains(s)))


  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
   lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}

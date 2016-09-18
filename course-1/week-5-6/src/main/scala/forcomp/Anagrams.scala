package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase
      .filter(_.isLetter)
      .toList
      .groupBy(c => c)
      .map { case (l, total) => (l, total.size) }
      .toList
      .sortBy { case (c, freq) => c }

  val wordOccurrencesMap = dictionary
    .map(w => w -> wordOccurrences(w))
    .toMap
    .withDefaultValue(Nil)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = s match {
    case Nil => List()
    case _ =>
      val r: Word = s.reduce((w1, w2) => w1 + w2)
      wordOccurrencesMap(r) match {
        case Nil => wordOccurrences(r)
        case o => o
    }
  }


  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary
      .map(w => (wordOccurrences(w), w))
      .groupBy { case (c, _) => c } map {
        case (k, v) => (k, v.map { case (_, i) => i } )
      } withDefaultValue List()

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences(wordOccurrences(word))
  }


  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(List())
    case (ch, num) :: os => {
      for (
        n <- combinations(os);
        i <- 0 to num
      ) yield if (i > 0) (ch, i) :: n else n
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val ymap = y.map { case (yc, yi) =>  yc -> yi }.toMap withDefaultValue(-1)
    x.map { case (xc, xi) => ymap(xc) match {
      case -1 => (xc, xi)
      case n => (xc, xi - n)
    }}
      .filter { case (tc, ti) => ti > 0 }
      .sortBy { case (tc, ti) => tc }
  }

  def contains(x: Occurrences, y: Occurrences): Boolean = {
    val xmap = x.map { case (xc, xi) => xc -> xi }.toMap withDefaultValue Int.MinValue
    y.find { case (yc, yi) => xmap(yc) < yi || xmap(yc) == Int.MinValue } match {
      case None => true
      case Some(_) => false
    }
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
    *  List("Rex", "Lin", "Zulu"), List("Rex", "Zulu", "nil"), List("Lin", "Zulu", "Rex")) did not equal
    *  List("Rex", "Lin", "Zulu"), List("Rex", "Zulu", "nil"), List("Lin", "Zulu", "Rex"), List("Linux", "rulez"))
ScalaTestFailureLocation
    *
    *
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sentences(o: Occurrences): List[Sentence] = o match {
      case Nil => List(Nil)
      case _ =>
        for (
          word <- dictionary;
          occurrences = wordOccurrencesMap(word)
          if contains(o, occurrences);
          subtracted = subtract(o, occurrences);
          sent <- sentences(subtracted)
        ) yield (subtracted, word, sent) match {
          case (Nil, _, _) => List(word)
          case (_, _, List()) => List(word)
          case (_, _, s :: sx) => word :: s :: sx
        }
    }

    val res = sentences(sentenceOccurrences(sentence))
      .filterNot((s: Sentence) =>
        sentence.length == 1 && s.equals(sentence))

    res match {
      case List() => List(Nil)
      case _ => res
    }
  }
}

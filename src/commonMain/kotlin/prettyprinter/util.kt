package prettyprinter

/*
 * The first functions here are ported from
 * [Util.hs](https://hackage.haskell.org/package/prettyprinter-1.7.1/src/src/Prettyprinter/Util.hs),
 * others are kotlin-isms.
 */

/**
 *
-- Split an input into word-sized [Doc]s.
--
-- >>> putDoc (tupled (words "Lorem ipsum dolor"))
-- (Lorem, ipsum, dolor)
 */
fun words(text: String): Iterable<DocNo> = text.split(' ').map { text(it) }

/**
-- | Insert soft linebreaks between words, so that text is broken into multiple
-- lines when it exceeds the available width.
--
-- >>> putDocW 32 (reflow "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
-- Lorem ipsum dolor sit amet,
-- consectetur adipisicing elit,
-- sed do eiusmod tempor incididunt
-- ut labore et dolore magna
-- aliqua.
--
-- ```
-- [reflow] = [fillSep] . [words]
-- ```
reflow :: Text -> Doc ann
reflow = fillSep . words
*/
fun reflow(text: String): DocNo = fillSep(words(text))

/**
-- | Render a document with a certain width. Useful for quick-and-dirty testing
-- of layout behaviour. Used heavily in the doctests of this package, for
-- example.
--
-- >>> let doc = reflow "Lorem ipsum dolor sit amet, consectetur adipisicing elit"
-- >>> putDocW 20 doc
-- Lorem ipsum dolor
-- sit amet,
-- consectetur
-- adipisicing elit
-- >>> putDocW 30 doc
-- Lorem ipsum dolor sit amet,
-- consectetur adipisicing elit
 *
 * Port note: our tests just write to a string. Also, we show annotations, which doesn't generally matter as the tests
 * rarely have any.
 */
fun <A> putDocW(w: Int, doc: Doc<A>): String = doc.toStringPretty(PageWidth.AvailablePerLine(w, 1.0))

private fun viaCharArray(cs: CharSequence): String {
    val n = cs.length
    val ca = CharArray(n)
    for(i in 0 until n) {
        ca[i] = cs[i]
    }
    return ca.concatToString()
}

/**
 * A [CharSequence] of a repeating character. Mostly useful for indentation.
 */
fun repeatChar(c: Char, n: Int): CharSequence = when {
    n <= 0 -> ""
    else -> object : CharSequence {
        override val length: Int = n
        override fun get(index: Int): Char = c
        override fun subSequence(startIndex: Int, endIndex: Int): CharSequence = repeatChar(c, endIndex - startIndex)
        override fun toString(): String = viaCharArray(this)
    }
}

/**
 * Repeats a [CharSequence], and handles subsequences nicely.
 */
fun repeatText(cs: CharSequence, n: Int): CharSequence = repeatText(cs, 0, cs.length * n)

/**
 * Suppose the sequence repeats endlessly; `repeatText("abc")` would mean that
 * 0 = a, 1 = b, 2 = c, 3 = a, 4 = b, 5 = c, etc
 * This is then the subsequence of that repetition from `s` to `e`.
 */
private fun repeatText(cs: CharSequence, s: Int, e: Int): CharSequence = when {
    cs.isEmpty() -> ""
    cs.length == 1 -> repeatChar(cs[0], e - s)
    e - s <= 0 -> ""
    else -> object : CharSequence {
        override val length: Int = e - s
        override fun get(index: Int): Char = cs[(index + s) % cs.length]
        override fun subSequence(startIndex: Int, endIndex: Int): CharSequence =
            repeatText(cs, s + startIndex, s + endIndex)
        override fun toString(): String = viaCharArray(this)
    }
}

/**
 * A list of one thing, repeated many times.
 */
fun <E> repeatThing(elem: E, n: Int): List<E> = when {
    n <= 0 -> listOf()
    n == 1 -> listOf(elem)
    else -> object : AbstractList<E>() {
        override val size: Int = n
        override fun get(index: Int): E = elem
        override fun contains(element: E): Boolean {
            return elem == element
        }
        override fun subList(fromIndex: Int, toIndex: Int): List<E> {
            return repeatThing(elem, toIndex - fromIndex)
        }
    }
}

/**
 * Applies a mapping function to everything but the last element. That also means the mapping function can't change
 * types.
 */
fun <E> mapAllExceptLast(seq: Sequence<E>, func: (E) -> E): Sequence<E> = object : Sequence<E> {
    override fun iterator(): Iterator<E> = mapAllExceptLast(seq.iterator(), func)
}

fun <E> mapAllExceptLast(iterable: Iterable<E>, func: (E) -> E): Iterable<E> = object : Iterable<E> {
    override fun iterator(): Iterator<E> = mapAllExceptLast(iterable.iterator(), func)
}

fun <E> mapAllExceptLast(iter: Iterator<E>, func: (E) -> E): Iterator<E> =
    object : Iterator<E> {
        override fun hasNext(): Boolean {
            return iter.hasNext()
        }

        override fun next(): E {
            val elem = iter.next()
            return if (iter.hasNext()) {
                func(elem)
            } else {
                elem
            }
        }
    }

/**
 * The classic right-fold, implemented with a for loop.
 * @param init the initial value of the accumulator
 * @param list the list of elements
 * @param func Being a right fold, the accumulator argument is on the right. Thus, the arguments are the next element
 *             of the list and the accumulator it is being folded into.
 * @param E the list element type
 * @param A the accumulator / result type
 */
fun <E, A> foldr(init: A, list: Iterable<E>, func: (E, A) -> A): A {
    var accum: A = init
    for (elem in list) {
        accum = func(elem, accum)
    }
    return accum
}

/**
 * Implements a standard cons list. It'd be very easy to make this a list, a la LinkedList, but it's mostly intended to
 * be an aid in porting code.
 */
sealed class CList<out E>: Iterable<E> {
    class Cons<E>(val head: E, val tail: CList<E>) : CList<E>()

    object Nil : CList<Nothing>()

    override fun iterator(): Iterator<E> {
        class I(private var node: CList<E>) : Iterator<E> {
            override fun hasNext(): Boolean = node is Cons
            override fun next(): E {
                when (val n = node) {
                    Nil -> throw NoSuchElementException()
                    is Cons -> {
                        node = n.tail
                        return n.head
                    }
                }
            }
        }
        return I(this)
    }
}

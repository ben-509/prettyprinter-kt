package prettyprinter

/*
 * The first functions here are ported from
 * [Util.hs](https://hackage.haskell.org/package/prettyprinter-1.7.1/src/src/Prettyprinter/Util.hs),
 * others are kotlin-isms.
 */

/**
 *
 * Split an input into word-sized [Doc]s.
 *
 *     >>> putDoc ([tupled] (words "Lorem ipsum dolor"))
 *     (Lorem, ipsum, dolor)
 */
fun words(text: String): List<DocNo> = text.split(' ').map(::text)

/**
 * Create a list of Docs from strings.
 */
fun texts(vararg text: String): List<DocNo> = text.map(::text)

/**
 * Insert soft linebreaks between words, so that text is broken into multiple
 * lines when it exceeds the available width.
 *
 *     >>> putDocW 32 (reflow "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
 *     Lorem ipsum dolor sit amet,
 *     consectetur adipisicing elit,
 *     sed do eiusmod tempor incididunt
 *     ut labore et dolore magna
 *     aliqua.
 *
 * ```
 * [reflow] = [fillSep] . [words]
 * ```
 */
fun reflow(text: String): DocNo = fillSep(words(text))

/**
 * Render a document with a certain width. Useful for quick-and-dirty testing
 * of layout behaviour. Used heavily in the doctests of this package, for
 * example.
 *
 *     >>> let doc = reflow "Lorem ipsum dolor sit amet, consectetur adipisicing elit"
 *     >>> putDocW 20 doc
 *     Lorem ipsum dolor
 *     sit amet,
 *     consectetur
 *     adipisicing elit
 *     >>> putDocW 30 doc
 *     Lorem ipsum dolor sit amet,
 *     consectetur adipisicing elit
 *
 * Port note: our tests use [Doc.toStringPretty]. This also takes an [Appendable] rather than an IO stream.
 */
fun <A> putDocW(w: Int, doc: Doc<A>, app: Appendable) {
    val sds = layoutPretty(Opts(PageWidth.AvailablePerLine(w, 1.0)), doc)
    AppendableSink<A>(tgt = app).render(sds)
}

/**
 * `([putDoc] doc)` prettyprints document `doc` to standard output. Uses the
 * [Opts.default].
 *
 *     >>> putDoc ("hello" <+> "world")
 *     hello world
 *
 * ```
 * 'putDoc' = 'hPutDoc' 'stdout'
 * ```
 *
 * Port note: our tests use [Doc.toStringPretty]. This also takes an [Appendable] rather than an IO stream.
 */
fun <A> putDoc(doc: Doc<A>, app: Appendable) {
    val sds = layoutPretty(Opts(PageWidth.default), doc)
    AppendableSink<A>(tgt = app).render(sds)
}

/**
 * A [CharSequence] of a repeating character. Mostly useful for indentation.
 *
 * Port note: see [charTimes] notes. To repeat a codepoint outside the BMP, use [repeatText].
 */
fun repeatChar(char: Char, num: Int): CharSequence {
    require(!char.isSurrogate()) { "$char is a surrogate, which isn't allowed in repeatChar" }
    return CharArray(num) { char }.concatToString()
}

/**
 * Repeats a [CharSequence], and handles subsequences nicely.
 */
fun repeatText(cs: CharSequence, n: Int): CharSequence = if (n < 0) {
    ""
} else {
    cs.repeat(n)
}

/**
 * A list of one thing, repeated many times.
 */
fun <E> repeatThing(elem: E, n: Int): List<E> = when {
    n <= 0 -> listOf()
    n == 1 -> listOf(elem)
    else -> object : AbstractList<E>() {
        override val size: Int = n
        override fun get(index: Int): E = when (index) {
            in 0 until n -> elem
            else -> throw IndexOutOfBoundsException()
        }

        override fun contains(element: E): Boolean {
            return elem == element
        }

        override fun subList(fromIndex: Int, toIndex: Int): List<E> {
            return repeatThing(elem, toIndex - fromIndex)
        }
    }
}

/**
 * Counts codepoints by excluding all low surrogates. This counts each non-surrogate character and leading surrogate
 * once.
 */
fun countCodepoints(cs: CharSequence): Int {
    return cs.count { !it.isLowSurrogate() }
}

/**
 * Applies a position aware mapping function that knows when it's at the extremes of a list.
 */
fun <E, F> Iterable<E>.mapWhere(func: (Where, E) -> F): Iterable<F> =
    Iterable { this@mapWhere.iterator().mapWhere(func) }

enum class Where {
    FIRST, MIDDLE, LAST, ONLY
}

fun <E, F> Iterator<E>.mapWhere(func: (Where, E) -> F): Iterator<F> =
    object : Iterator<F> {
        var first = true
        override fun hasNext(): Boolean {
            return this@mapWhere.hasNext()
        }

        override fun next(): F {
            val elem = this@mapWhere.next()
            val last = !this@mapWhere.hasNext()
            val where = when {
                first && last -> Where.ONLY
                first -> Where.FIRST
                last -> Where.LAST
                else -> Where.MIDDLE
            }
            first = false
            return func(where, elem)
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
sealed class CList<out E> : Iterable<E> {
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

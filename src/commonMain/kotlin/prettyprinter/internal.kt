@file:Suppress("MemberVisibilityCanBePrivate", "unused", "SpellCheckingInspection")

/**
 * Porting https://hackage.haskell.org/package/prettyprinter-1.7.1/src/src/Prettyprinter/Internal.hs to Kotlin.
 * Kotlin doesn't require reexports so this is left in the same place.
 */
package prettyprinter

import prettyprinter.render.util.panicPeekedEmpty
import prettyprinter.render.util.panicSkippingInUnannotated
import prettyprinter.symbols.commaSpace
import prettyprinter.symbols.lbracket
import prettyprinter.symbols.lbracketSpace
import prettyprinter.symbols.lparen
import prettyprinter.symbols.lparenSpace
import prettyprinter.symbols.rbracket
import prettyprinter.symbols.rbracketSpace
import prettyprinter.symbols.rparen
import prettyprinter.symbols.rparenSpace
import prettyprinter.symbols.space
import kotlin.math.floor
import kotlin.math.max
import kotlin.math.min

sealed class Doc<out A> {
    /** Occurs when flattening a line. The layouter will reject this document, choosing a more suitable rendering. */
    internal object Fail : DocNo()

    /** The empty document; conceptually the unit of `Cat` */
    internal object Empty : DocNo()

    /** An individual character.  */
    internal class Char<A>(val char: kotlin.Char) : Doc<A>() {
        init {
            require(char != '\n') { "Doc.Char must not be newline; use Line" }
        }
    }

    /** Invariants exists because Text should never overlap Empty, Char or Line */
    internal class Text<A>(val text: CharSequence) : Doc<A>() {
        val length: Int
            get() = text.length
        init {
            require(length >= 2) { "Doc.Text must be at least 2 characters; use PChar or Empty" }
            require('\n' !in text) { "Doc.Text must not contain newline; use Line" }
        }
    }

    /** Hard line break. Use helper functions to get line breaks, e.g. [hardline], [softline] and others. */
    internal object Line : DocNo()

    /**
     * Lay out the first 'Doc', but when flattened (via 'group'), prefer
     * the second.
     *
     * The layout algorithms work under the assumption that the first
     * alternative is less wide than the flattened second alternative.
     */
    internal class FlatAlt<A>(val first: Doc<A>, val second: Doc<A>) : Doc<A>()

    /** Concatenation of two documents */
    internal class Cat<A>(val first: Doc<A>, val second: Doc<A>) : Doc<A>()

    /** Document indented by a number of columns */
    internal class Nest<A>(val indent: Int, val doc: Doc<A>) : Doc<A>()

    /**
     * Invariant: The first lines of first document should be longer than the
     * first lines of the second one, so the layout algorithm can pick the one
     * that fits best. Used to implement layout alternatives for 'group'.
     */
    internal class Union<A>(val first: Doc<A>, val second: Doc<A>) : Doc<A>()

    /** React on the current cursor position, see 'column' */
    internal class Column<A>(val react: (Int) -> Doc<A>) : Doc<A>()

    /** React on the document's width, see 'pageWidth' */
    internal class WithPageWidth<A>(val react: (PageWidth) -> Doc<A>) : Doc<A>()

    /** React on the current nesting level, see 'nesting' */
    internal class Nesting<A>(val react: (Int) -> Doc<A>) : Doc<A>()

    /**
     * Add an annotation to the enclosed 'Doc'. Can be used for example to add
     * styling directives or alt texts that can then be used by the renderer.
     */
    internal class Annotated<out A>(val ann: A, val doc: Doc<A>) : Doc<A>()
}

internal typealias DocNo = Doc<Nothing>

val emptyDoc: DocNo = Doc.Empty

// Semigroup operations
/** `doc <> doc` emulated through `doc + doc` */
operator fun <A> Doc<A>.plus(doc: Doc<A>): Doc<A> = Doc.Cat(this, doc)

fun <A> sconcat(docs: Iterable<Doc<A>>): Doc<A> = hcat(docs)
fun <A> sconcat(docs: Sequence<Doc<A>>): Doc<A> = hcat(docs)
fun <A> stimes(n: Int, doc: Doc<A>): Doc<A> = when {
    n <= 0 -> Doc.Empty
    n == 1 -> doc
    else -> when (doc) {
        Doc.Fail, Doc.Empty -> doc
        is Doc.Char -> Doc.Text(repeatChar(doc.char, n))
        is Doc.Text -> Doc.Text(repeatText(doc.text, n))
        else -> hcat(repeatThing(doc, n))
    }
}

// Monoid definitions
val mempty: DocNo = Doc.Empty  // emptyDoc, but Haskell defines out of order.
fun <A> mconcat(docs: Iterable<Doc<A>>): Doc<A> = hcat(docs)
fun <A> mconcat(docs: Sequence<Doc<A>>): Doc<A> = hcat(docs)
fun <A> mappend(left: Doc<A>, right: Doc<A>): Doc<A> = left + right

/**
 * Convert a string to a document, treating newlines as hardlines.
 */
fun text(cs: CharSequence): DocNo = vsep(cs.splitToSequence('\n').map { unsafe(it) })

private fun unsafe(cs: CharSequence): DocNo = when {
    cs.isEmpty() -> Doc.Empty;
    cs.length == 1 -> Doc.Char(cs[0])
    else -> Doc.Text(cs)
}

fun char(c: Char): DocNo = when (c) {
    '\n' -> Doc.Line
    else -> Doc.Char(c)
}

/**
 * Prettyprinter sets up a whole typeclass for this.
 * Generally, you want more control.
 */
interface Prettier {
    fun prettyList(e: Iterable<*>): DocNo = align(list(e.map { pretty(it) }))
    fun pretty(e: Any?): DocNo
}

/**
 * This is a straight port of some Haskell rules for prettifying, especially Haskell's list style.
 */
object Haskell : Prettier {
    override fun pretty(e: Any?): DocNo = when (e) {
        is CharSequence -> text(e)
        is Iterable<*> -> prettyList(e)
        is Unit -> unsafe("()")
        is Boolean, is Number -> unsafe(e.toString())
        is Char -> char(e)
        null -> unsafe("null")
        else -> text(e.toString())
    }
}

/**
 * `nest i x` lays out the document `x` with the current nesting level
 * (indentation of the following lines) increased by `i`. Negative values are
 * allowed, and decrease the nesting level accordingly.
 *
 * >>> vsep [nest 4 (vsep ["lorem", "ipsum", "dolor"]), "sit", "amet"]
 * lorem
 *     ipsum
 *     dolor
 * sit
 * amet
 *
 * See also:
 *
 *  * 'hang' ('nest' relative to current cursor position instead of
 *     current nesting level)
 *  * 'align' (set nesting level to current cursor position)
 *  * 'indent' (increase indentation on the spot, padding with spaces).
 */
fun <A> nest(i: Int, x: Doc<A>) = when (i) {
    0 -> x
    else -> Doc.Nest(i, x)
}

/**
 * The `line` document advances to the next line and indents to the current
 * nesting level.
 *
 * >>> let doc = "lorem ipsum" <> line <> "dolor sit amet"
 * >>> doc
 * lorem ipsum
 * dolor sit amet
 *
 * `line` behaves like `space` if the line break is undone by `group`:
 *
 * >>> group doc
 * lorem ipsum dolor sit amet
 */
val line: DocNo = Doc.FlatAlt(Doc.Line, space)

/**
 * `line_` is like `line`, but behaves like `mempty` if the line break
 * is undone by `group` (instead of `space`).
 *
 * >>> val doc = "lorem ipsum" <> line_ <> "dolor sit amet"
 * >>> doc
 * lorem ipsum
 * dolor sit amet
 * >>> group doc
 * lorem ipsumdolor sit amet
 */

val line_: DocNo = Doc.FlatAlt(Doc.Line, Doc.Empty)

/**
 * `softline` behaves like `space` if the resulting output fits the page,
 * otherwise like `line`.
 *
 * Here, we have enough space to put everything in one line:
 *
 * >>> let doc = "lorem ipsum" <> softline <> "dolor sit amet"
 * >>> putDocW 80 doc
 * lorem ipsum dolor sit amet
 *
 * If we narrow the page to width 10, the layouter produces a line break:
 *
 * >>> putDocW 10 doc
 * lorem ipsum
 * dolor sit amet
 *
 */

val softline: DocNo = Doc.Union(space, Doc.Line)

/**
 * `softline_` is like `softline`, but behaves like `mempty` if the
 * resulting output does not fit on the page (instead of `space`). In other
 * words, `line` is to `line_` how `softline` is to `softline_`.
 *
 * With enough space, we get direct concatenation:
 *
 * >>> let doc = "ThisWord" <> softline_ <> "IsWayTooLong"
 * >>> putDocW 80 doc
 * ThisWordIsWayTooLong
 *
 * If we narrow the page to width 10, the layouter produces a line break:
 *
 * >>> putDocW 10 doc
 * ThisWord
 * IsWayTooLong
 *
 * ```
 * 'softline'' = 'group' 'line''
 * ```
 */

val softline_: DocNo = Doc.Union(Doc.Empty, Doc.Line)

/**
 * A `hardline` is _always_ laid out as a line break, even when `group`ed or
 * when there is plenty of space. Note that it might still be simply discarded
 * if it is part of a `flatAlt` inside a `group`.
 *
 * >>> let doc = "lorem ipsum" <> hardline <> "dolor sit amet"
 * >>> putDocW 1000 doc
 * lorem ipsum
 * dolor sit amet
 *
 * >>> group doc
 * lorem ipsum
 * dolor sit amet
 */
val hardline: DocNo = Doc.Line

/**
 * `group x` tries laying out `x` into a single line by removing the
 * contained line breaks; if this does not fit the page, or when a `hardline`
 * within `x` prevents it from being flattened, `x` is laid out without any
 * changes.
 *
 * The `group` function is key to layouts that adapt to available space nicely.
 *
 * See `vcat`, `line`, or `flatAlt` for examples that are related, or make good
 * use of it.
 */
fun <A> group(doc: Doc<A>): Doc<A> = when (doc) {
    is Doc.Union<*> -> doc
    is Doc.FlatAlt<*> -> TODO()
    else -> TODO()
}

sealed class Flatten<out F> {
    /** `doc` is likely flatter than the input */
    class Flattened<F>(val doc: F) : Flatten<F>()

    /** The input was already flat, e.g. a `Text`. */
    object AlreadyFlat : Flatten<Nothing>()

    /** The input couldn't be flattened: It contained a `Line` or `Fail`. */
    object NeverFlat : Flatten<Nothing>()

    /**
     * Port note: instance of Functor
     */
    fun <X, Y> fmap(f: (X) -> Y, g: Flatten<X>): Flatten<Y> = when (g) {
        is AlreadyFlat -> AlreadyFlat
        is NeverFlat -> NeverFlat
        is Flattened -> Flattened(f(g.doc))
    }

    /**
     * Choose the first element of each `Union`, and discard the first field of
     * all `FlatAlt`s.
     *
     * The result is `Flattened` if the element might change depending on the layout
     * algorithm (i.e. contains differently renderable sub-documents), and `AlreadyFlat`
     * if the document is static (e.g. contains only a plain `Empty` node).
     * `NeverFlat` is returned when the document cannot be flattened because it
     * contains a hard `Line` or `Fail`.
     * See [Group: special flattening] for further explanations.
     */

    fun <A> changesUpon(doc: Doc<A>): Flatten<Doc<A>> = when (doc) {
        is Doc.FlatAlt -> Flattened(flatten(doc.second))
        is Doc.Line -> NeverFlat
        is Doc.Union -> Flattened(doc.first)
        is Doc.Nest -> fmap({ Doc.Nest(doc.indent, it) }, changesUpon(doc.doc))
        is Doc.Annotated -> fmap({ Doc.Annotated(doc.ann, it) }, changesUpon(doc.doc))
        is Doc.Column -> Flattened(Doc.Column { flatten(doc.react(it)) })
        is Doc.Nesting -> Flattened(Doc.Nesting { flatten(doc.react(it)) })
        is Doc.WithPageWidth -> Flattened(Doc.WithPageWidth { flatten(doc.react(it)) })
        is Doc.Cat -> {
            val cuFirst = changesUpon(doc.first)
            val cuSecond = changesUpon(doc.second)
            when {
                cuFirst == NeverFlat || cuSecond == NeverFlat -> NeverFlat
                cuFirst is Flattened && cuSecond is Flattened -> Flattened(cuFirst.doc + cuSecond.doc)
                cuFirst is Flattened && cuSecond is AlreadyFlat -> Flattened(cuFirst.doc + doc.second)
                cuFirst is AlreadyFlat && cuSecond is Flattened -> Flattened(doc.first + cuSecond.doc)
                else -> AlreadyFlat
            }
        }
        is Doc.Empty, is Doc.Char, is Doc.Text -> AlreadyFlat
        is Doc.Fail -> NeverFlat
    }

    /** Flatten, but don’t report whether anything changes. */
    private fun <A> flatten(doc: Doc<A>): Doc<A> = when (doc) {
        is Doc.FlatAlt -> flatten(doc.second)
        is Doc.Cat -> flatten(doc.first) + flatten(doc.second)
        is Doc.Nest -> nest(doc.indent, flatten(doc.doc))
        is Doc.Line -> Doc.Fail
        is Doc.Union -> flatten(doc.first)
        is Doc.Column -> Doc.Column { flatten(doc.react(it)) }
        is Doc.WithPageWidth -> Doc.WithPageWidth { flatten(doc.react(it)) }
        is Doc.Nesting -> Doc.Nesting { flatten(doc.react(it)) }
        is Doc.Annotated -> Doc.Annotated(doc.ann, flatten(doc.doc))
        is Doc.Fail, is Doc.Empty, is Doc.Char, is Doc.Text -> doc
    }
}

/**
 * By default, `flatAlt x y` renders as `x`. However when `group`ed,
 * `y` will be preferred, with `x` as the fallback for the case when `y`
 * doesn't fit.
 *
 * >>> let doc = flatAlt "a" "b"
 * >>> putDoc doc
 * a
 * >>> putDoc (group doc)
 * b
 * >>> putDocW 0 (group doc)
 * a
 *
 * `flatAlt` is particularly useful for defining conditional separators such as
 *
 * ```
 * softline = group (flatAlt hardline " ")
 * ```
 *
 * >>> let hello = "Hello" <> softline <> "world!"
 * >>> putDocW 12 hello
 * Hello world!
 * >>> putDocW 11 hello
 * Hello
 * world!
 *
 * === __Example: Haskell's do-notation__
 *
 * We can use this to render Haskell's do-notation nicely:
 *
 * >>> let open        = flatAlt "" "{ "
 * >>> let close       = flatAlt "" " }"
 * >>> let separator   = flatAlt "" "; "
 * >>> let prettyDo xs = group ("do" <+> align (encloseSep open close separator xs))
 * >>> let statements  = ["name:_ <- getArgs", "let greet = \"Hello, \" <> name", "putStrLn greet"]
 *
 * This is put into a single line with `{;}` style if it fits:
 *
 * >>> putDocW 80 (prettyDo statements)
 * do { name:_ <- getArgs; let greet = "Hello, " <> name; putStrLn greet }
 *
 * When there is not enough space the statements are broken up into lines
 * nicely:
 *
 * >>> putDocW 10 (prettyDo statements)
 * do name:_ <- getArgs
 *    let greet = "Hello, " <> name
 *    putStrLn greet
 *
 * === Notes
 *
 * Users should be careful to choose `x` to be less wide than `y`.
 * Otherwise, if `y` turns out not to fit the page, we fall back on an even
 * wider layout:
 *
 * >>> let ugly = group (flatAlt "even wider" "too wide")
 * >>> putDocW 7 ugly
 * even wider
 *
 * Also note that `group` will flatten `y`:
 *
 * >>> putDoc (group (flatAlt "x" ("y" <> line <> "y")))
 * y y
 *
 * This also means that an "unflattenable" `y` which contains a hard linebreak
 * will /never/ be rendered:
 *
 * >>> putDoc (group (flatAlt "x" ("y" <> hardline <> "y")))
 * x
 */
fun <A> flatAlt(default: Doc<A>, preferred: Doc<A>): Doc<A> = Doc.FlatAlt(default, preferred)

/**
 * `align` lays out the document `x` with the nesting level set to the
 * current column. It is used for example to implement `hang`.
 *
 * As an example, we will put a document right above another one, regardless of
 * the current nesting level. Without `align`ment, the second line is put simply
 * below everything we've had so far:
 *
 * >>> "lorem" <+> vsep ["ipsum", "dolor"]
 * lorem ipsum
 * dolor
 *
 * If we add an `align` to the mix, the `vsep`'s contents all start in the
 * same column:
 *
 * >>> "lorem" <+> align (vsep ["ipsum", "dolor"])
 * lorem ipsum
 *       dolor
 */
fun <A> align(d: Doc<A>): Doc<A> = column { k -> nesting { i -> nest(k - i, d) } }

/**
 * `hang i x` lays out the document `x` with a nesting level set to the
-- _current column_ plus `i`. Negative values are allowed, and decrease the
-- nesting level accordingly.
--
-- >>> let doc = reflow "Indenting these words with hang"
-- >>> putDocW 24 ("prefix" <+> hang 4 doc)
-- prefix Indenting these
--            words with
--            hang
--
-- This differs from `nest`, which is based on the /current nesting level/ plus
-- `i`. When you're not sure, try the more efficient `nest` first. In our
-- example, this would yield
--
-- >>> let doc = reflow "Indenting these words with nest"
-- >>> putDocW 24 ("prefix" <+> nest 4 doc)
-- prefix Indenting these
--     words with nest
--
-- ```
-- hang i doc = align (nest i doc)
-- ```
 */
fun <A> hang(i: Int, d: Doc<A>): Doc<A> = align(nest(i, d))

/**
-- `indent i x` indents document @x@ by @i@ columns, starting from the
-- current cursor position.
--
-- >>> let doc = reflow "The indent function indents these words!"
-- >>> putDocW 24 ("prefix" <> indent 4 doc)
-- prefix    The indent
--           function
--           indents these
--           words!
--
-- ```
-- indent i d = hang i ({i spaces} <> d)
-- ```
indent
:: Int -- ^ Number of spaces to increase indentation by
-> Doc ann
-> Doc ann
indent i d = hang i (spaces i <> d)
 */
fun <A> indent(i: Int, d: Doc<A>): Doc<A> = hang(i, spaces(i) + d)

/**
 * `encloseSep l r sep xs` concatenates the documents @xs@ separated by
-- @sep@, and encloses the resulting document by @l@ and @r@.
--
-- The documents are laid out horizontally if that fits the page:
--
-- >>> let doc = "list" <+> align (encloseSep lbracket rbracket comma (map pretty [1,20,300,4000]))
-- >>> putDocW 80 doc
-- list [1,20,300,4000]
--
-- If there is not enough space, then the input is split into lines entry-wise
-- therwise they are laid out vertically, with separators put in the front:
--
-- >>> putDocW 10 doc
-- list [1
--      ,20
--      ,300
--      ,4000]
--
-- Note that @doc@ contains an explicit call to 'align' so that the list items
-- are aligned vertically.
--
-- For putting separators at the end of entries instead, have a look at
-- 'punctuate'.
 */
fun <A> encloseSep(leftDelim: Doc<A>, rightDelim: Doc<A>, separator: Doc<A>, inputs: Iterable<Doc<A>>): Doc<A> =
    encloseSep(leftDelim, rightDelim, separator, inputs.iterator())

fun <A> encloseSep(leftDelim: Doc<A>, rightDelim: Doc<A>, separator: Doc<A>, inputs: Sequence<Doc<A>>): Doc<A> =
    encloseSep(leftDelim, rightDelim, separator, inputs.iterator())

private fun <A> encloseSep(leftDelim: Doc<A>, rightDelim: Doc<A>, separator: Doc<A>, inputs: Iterator<Doc<A>>): Doc<A> {
    var sep = leftDelim
    var node: Doc<A> = Doc.Empty
    for (doc in inputs) {
        node = node + sep + doc
        sep = separator
    }
    return if (node == Doc.Empty) {
        leftDelim + rightDelim
    } else {
        node + rightDelim
    }
}


/**
 * Haskell-inspired variant of 'encloseSep' with braces and comma as
-- separator.
--
-- >>> let doc = list (map pretty [1,20,300,4000])
--
-- >>> putDocW 80 doc
-- [1, 20, 300, 4000]
--
-- >>> putDocW 10 doc
-- [ 1
-- , 20
-- , 300
-- , 4000 ]
 */
fun <A> list(docs: Iterable<Doc<A>>): Doc<A> =
    group(encloseSep(flatAlt(lbracketSpace, lbracket), flatAlt(rbracketSpace, rbracket), commaSpace, docs))

fun <A> list(docs: Sequence<Doc<A>>): Doc<A> =
    group(encloseSep(flatAlt(lbracketSpace, lbracket), flatAlt(rbracketSpace, rbracket), commaSpace, docs))


/**
-- | Haskell-inspired variant of 'encloseSep' with parentheses and comma as
-- separator.
--
-- >>> let doc = tupled (map pretty [1,20,300,4000])
--
-- >>> putDocW 80 doc
-- (1, 20, 300, 4000)
--
-- >>> putDocW 10 doc
-- ( 1
-- , 20
-- , 300
-- , 4000 )
 */

fun <A> tupled(docs: Iterable<Doc<A>>): Doc<A> =
    group(encloseSep(flatAlt(lparenSpace, lparen), flatAlt(rparenSpace, rparen), commaSpace, docs))

fun <A> tupled(docs: Sequence<Doc<A>>): Doc<A> =
    group(encloseSep(flatAlt(lparenSpace, lparen), flatAlt(rparenSpace, rparen), commaSpace, docs))

/**
 * `x <+> y` concatenates document @x@ and @y@ with a @space@ in
-- between.
--
-- >>> "hello" <+> "world"
-- hello world
--
-- @
-- x '<+>' y = x '<>' 'space' '<>' y
-- @
 */
infix fun <A> Doc<A>.spc(right: Doc<A>): Doc<A> = this + space + right

/**
-- Concatenate all documents element-wise with a binary function.
--
-- @
-- 'concatWith' _ [] = 'mempty'
-- 'concatWith' (**) [x,y,z] = x ** y ** z
-- @
--
-- Multiple convenience definitions based on 'concatWith' are already predefined,
-- for example:
--
-- @
-- 'hsep'    = 'concatWith' ('<+>')
-- 'fillSep' = 'concatWith' (\\x y -> x '<>' 'softline' '<>' y)
-- @
--
-- This is also useful to define customized joiners:
--
-- >>> concatWith (surround dot) ["Prettyprinter", "Render", "Text"]
-- Prettyprinter.Render.Text
 */
fun <A> concatWith(docs: Iterable<Doc<A>>, func: (Doc<A>, Doc<A>) -> Doc<A>): Doc<A> = concatWith(docs.iterator(), func)
fun <A> concatWith(docs: Sequence<Doc<A>>, func: (Doc<A>, Doc<A>) -> Doc<A>): Doc<A> = concatWith(docs.iterator(), func)

private fun <A> concatWith(iter: Iterator<Doc<A>>, func: (Doc<A>, Doc<A>) -> Doc<A>): Doc<A> {
    if (!iter.hasNext())
        return Doc.Empty
    var node = iter.next()
    while (iter.hasNext()) {
        node = func(node, iter.next())
    }
    return node
}

/**
 * `hsep xs` concatenates all documents @xs@ horizontally with @<+>@,
-- i.e. it puts a space between all entries.
--
-- >>> let docs = Util.words "lorem ipsum dolor sit amet"
--
-- >>> hsep docs
-- lorem ipsum dolor sit amet
--
-- @'hsep'@ does not introduce line breaks on its own, even when the page is too
-- narrow:
--
-- >>> putDocW 5 (hsep docs)
-- lorem ipsum dolor sit amet
--
-- For automatic line breaks, consider using 'fillSep' instead.
 */
fun <A> hsep(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { a, b -> a spc b }
fun <A> hsep(docs: Sequence<Doc<A>>): Doc<A> = concatWith(docs) { a, b -> a spc b }

/**
 * `vsep xs` concatenates all documents @xs@ above each other. If a
-- 'group' undoes the line breaks inserted by @vsep@, the documents are
-- separated with a 'space' instead.
--
-- Using 'vsep' alone yields
--
-- >>> "prefix" <+> vsep ["text", "to", "lay", "out"]
-- prefix text
-- to
-- lay
-- out
--
-- 'group'ing a 'vsep' separates the documents with a 'space' if it fits the
-- page (and does nothing otherwise). See the @'sep'@ convenience function for
-- this use case.
--
-- The 'align' function can be used to align the documents under their first
-- element:
--
-- >>> "prefix" <+> align (vsep ["text", "to", "lay", "out"])
-- prefix text
--        to
--        lay
--        out
--
-- Since 'group'ing a 'vsep' is rather common, 'sep' is a built-in for doing
-- that.
 */
fun <A> vsep(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { x, y -> x + line + y }
fun <A> vsep(docs: Sequence<Doc<A>>): Doc<A> = concatWith(docs) { x, y -> x + line + y }


/**
 * `fillSep xs` concatenates the documents @xs@ horizontally with @<+>@
-- as long as it fits the page, then inserts a @line@ and continues doing that
-- for all documents in @xs@. (@line@ means that if 'group'ed, the documents
-- are separated with a 'space' instead of newlines. Use 'fillCat' if you do not
-- want a 'space'.)
--
-- Let's print some words to fill the line:
--
-- >>> let docs = take 20 (cycle ["lorem", "ipsum", "dolor", "sit", "amet"])
-- >>> putDocW 80 ("Docs:" <+> fillSep docs)
-- Docs: lorem ipsum dolor sit amet lorem ipsum dolor sit amet lorem ipsum dolor
-- sit amet lorem ipsum dolor sit amet
--
-- The same document, printed at a width of only 40, yields
--
-- >>> putDocW 40 ("Docs:" <+> fillSep docs)
-- Docs: lorem ipsum dolor sit amet lorem
-- ipsum dolor sit amet lorem ipsum dolor
-- sit amet lorem ipsum dolor sit amet
fillSep :: [Doc ann] -> Doc ann
fillSep = concatWith (\x y -> x <> softline <> y)
 */
fun <A> fillSep(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { x, y -> x + softline + y }
fun <A> fillSep(docs: Sequence<Doc<A>>): Doc<A> = concatWith(docs) { x, y -> x + softline + y }

/**
 * `sep xs` tries laying out the documents @xs@ separated with 'space's,
-- and if this does not fit the page, separates them with newlines. This is what
-- differentiates it from 'vsep', which always lays out its contents beneath
-- each other.
--
-- >>> let doc = "prefix" <+> sep ["text", "to", "lay", "out"]
-- >>> putDocW 80 doc
-- prefix text to lay out
--
-- With a narrower layout, the entries are separated by newlines:
--
-- >>> putDocW 20 doc
-- prefix text
-- to
-- lay
-- out
--
-- @
-- 'sep' = 'group' . 'vsep'
-- @
 */
fun <A> sep(docs: Iterable<Doc<A>>): Doc<A> = group(vsep(docs))
fun <A> sep(docs: Sequence<Doc<A>>): Doc<A> = group(vsep(docs))

/**

-- | @('hcat' xs)@ concatenates all documents @xs@ horizontally with @'<>'@
-- (i.e. without any spacing).
--
-- It is provided only for consistency, since it is identical to 'mconcat'.
--
-- >>> let docs = Util.words "lorem ipsum dolor"
-- >>> hcat docs
-- loremipsumdolor
hcat :: [Doc ann] -> Doc ann
hcat = concatWith (<>)
 */
fun <A> hcat(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { a, b -> a + b }
fun <A> hcat(docs: Sequence<Doc<A>>): Doc<A> = concatWith(docs) { a, b -> a + b }

/**
-- | @('vcat' xs)@ vertically concatenates the documents @xs@. If it is
-- 'group'ed, the line breaks are removed.
--
-- In other words @'vcat'@ is like @'vsep'@, with newlines removed instead of
-- replaced by 'space's.
--
-- >>> let docs = Util.words "lorem ipsum dolor"
-- >>> vcat docs
-- lorem
-- ipsum
-- dolor
-- >>> group (vcat docs)
-- loremipsumdolor
--
-- Since 'group'ing a 'vcat' is rather common, 'cat' is a built-in shortcut for
-- it.
 */
fun <A> vcat(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { a, b -> a + line_ + b }
fun <A> vcat(docs: Sequence<Doc<A>>): Doc<A> = concatWith(docs) { a, b -> a + line_ + b }

/**
 * `fillCat xs` concatenates documents @xs@ horizontally with @'<>'@ as
-- long as it fits the page, then inserts a @'line''@ and continues doing that
-- for all documents in @xs@. This is similar to how an ordinary word processor
-- lays out the text if you just keep typing after you hit the maximum line
-- length.
--
-- (@'line''@ means that if 'group'ed, the documents are separated with nothing
-- instead of newlines. See 'fillSep' if you want a 'space' instead.)
--
-- Observe the difference between 'fillSep' and 'fillCat'. 'fillSep'
-- concatenates the entries 'space'd when 'group'ed:
--
-- >>> let docs = take 20 (cycle (["lorem", "ipsum", "dolor", "sit", "amet"]))
-- >>> putDocW 40 ("Grouped:" <+> group (fillSep docs))
-- Grouped: lorem ipsum dolor sit amet
-- lorem ipsum dolor sit amet lorem ipsum
-- dolor sit amet lorem ipsum dolor sit
-- amet
--
-- On the other hand, 'fillCat' concatenates the entries directly when
-- 'group'ed:
--
-- >>> putDocW 40 ("Grouped:" <+> group (fillCat docs))
-- Grouped: loremipsumdolorsitametlorem
-- ipsumdolorsitametloremipsumdolorsitamet
-- loremipsumdolorsitamet
fillCat :: [Doc ann] -> Doc ann
fillCat = concatWith (\x y -> x <> softline' <> y)
 */
fun <A> fillCat(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { x, y -> x + softline_ + y }
fun <A> fillCat(docs: Sequence<Doc<A>>): Doc<A> = concatWith(docs) { x, y -> x + softline_ + y }

/**
-- `cat xs` tries laying out the documents @xs@ separated with nothing,
-- and if this does not fit the page, separates them with newlines. This is what
-- differentiates it from 'vcat', which always lays out its contents beneath
-- each other.
--
-- >>> let docs = Util.words "lorem ipsum dolor"
-- >>> putDocW 80 ("Docs:" <+> cat docs)
-- Docs: loremipsumdolor
--
-- When there is enough space, the documents are put above one another:
--
-- >>> putDocW 10 ("Docs:" <+> cat docs)
-- Docs: lorem
-- ipsum
-- dolor
--
-- ```
-- 'cat' = 'group' . 'vcat'
-- ```
cat :: [Doc ann] -> Doc ann
cat = group . vcat
 */
fun <A> cat(docs: Iterable<Doc<A>>): Doc<A> = group(vcat(docs))
fun <A> cat(docs: Sequence<Doc<A>>): Doc<A> = group(vcat(docs))

/**
-- `punctuate p xs` appends @p@ to all but the last document in @xs@.
--
-- >>> let docs = punctuate comma (Util.words "lorem ipsum dolor sit amet")
-- >>> putDocW 80 (hsep docs)
-- lorem, ipsum, dolor, sit, amet
--
-- The separators are put at the end of the entries, which we can see if we
-- position the result vertically:
--
-- >>> putDocW 20 (vsep docs)
-- lorem,
-- ipsum,
-- dolor,
-- sit,
-- amet
--
-- If you want put the commas in front of their elements instead of at the end,
-- you should use 'tupled' or, in general, 'encloseSep'.
punctuate
:: Doc ann -- ^ Punctuation, e.g. 'comma'
-> [Doc ann]
-> [Doc ann]
punctuate p = go
where
go []     = []
go \[d]    = \[d]
go (d:ds) = (d <> p) : go ds
 */
fun <A> punctuate(pun: Doc<A>, docs: Iterable<Doc<A>>): Iterable<Doc<A>> = mapAllExceptLast(docs) { it + pun }
fun <A> punctuate(pun: Doc<A>, docs: Sequence<Doc<A>>): Sequence<Doc<A>> = mapAllExceptLast(docs) { it + pun }

/**
 * Layout a document depending on which column it starts at. 'align' is
-- implemented in terms of 'column'.
--
-- >>> column (\l -> "Columns are" <+> pretty l <> "-based.")
-- Columns are 0-based.
--
-- >>> let doc = "prefix" <+> column (\l -> "| <- column" <+> pretty l)
-- >>> vsep [indent n doc | n <- [0,4,8]]
-- prefix | <- column 7
--     prefix | <- column 11
--         prefix | <- column 15
 */
fun <A> column(react: (Int) -> Doc<A>): Doc<A> = Doc.Column(react)

/**
-- | Layout a document depending on the current 'nest'ing level. 'align' is
-- implemented in terms of 'nesting'.
--
-- >>> let doc = "prefix" <+> nesting (\l -> brackets ("Nested:" <+> pretty l))
-- >>> vsep [indent n doc | n <- [0,4,8]]
-- prefix [Nested: 0]
--     prefix [Nested: 4]
--         prefix [Nested: 8]
 */
fun <A> nesting(react: (Int) -> Doc<A>): Doc<A> = Doc.Nesting(react)

/**
-- `width doc f` lays out the document 'doc', and makes the column width
-- of it available to a function.
--
-- >>> let annotate doc = width (brackets doc) (\w -> " <- width:" <+> pretty w)
-- >>> align (vsep (map annotate ["---", "------", indent 3 "---", vsep ["---", indent 4 "---"]]))
-- [---] <- width: 5
-- [------] <- width: 8
-- [   ---] <- width: 8
-- [---
--     ---] <- width: 8
 */
fun <A> width(doc: Doc<A>, react: (Int) -> Doc<A>): Doc<A> =
    column { colStart -> doc + column { colEnd -> react(colEnd - colStart) } }

/**
 * Layout a document depending on the page width, if one has been specified.
--
-- >>> let prettyPageWidth (AvailablePerLine l r) = "Width:" <+> pretty l <> ", ribbon fraction:" <+> pretty r
-- >>> let doc = "prefix" <+> pageWidth (brackets . prettyPageWidth)
-- >>> putDocW 32 (vsep [indent n doc | n <- [0,4,8]])
-- prefix [Width: 32, ribbon fraction: 1.0]
--     prefix [Width: 32, ribbon fraction: 1.0]
--         prefix [Width: 32, ribbon fraction: 1.0]
 */
fun <A> pageWidth(react: (PageWidth) -> Doc<A>): Doc<A> = Doc.WithPageWidth(react)

/**
 * -- | @('fill' i x)@ lays out the document @x@. It then appends @space@s until
-- the width is equal to @i@. If the width of @x@ is already larger, nothing is
-- appended.
--
-- This function is quite useful in practice to output a list of bindings:
--
-- >>> let types = [("empty","Doc"), ("nest","Int -> Doc -> Doc"), ("fillSep","[Doc] -> Doc")]
-- >>> let ptype (name, tp) = fill 5 (pretty name) <+> "::" <+> pretty tp
-- >>> "let" <+> align (vcat (map ptype types))
-- let empty :: Doc
--     nest  :: Int -> Doc -> Doc
--     fillSep :: [Doc] -> Doc
 */

fun <A> fill(atLeastWidth: Int, doc: Doc<A>): Doc<A> = width(doc) { spaces(atLeastWidth - it) }

/**
-- | @('fillBreak' i x)@ first lays out the document @x@. It then appends @space@s
-- until the width is equal to @i@. If the width of @x@ is already larger than
-- @i@, the nesting level is increased by @i@ and a @line@ is appended. When we
-- redefine @ptype@ in the example given in 'fill' to use @'fillBreak'@, we get
-- a useful variation of the output:
--
-- >>> let types = [("empty","Doc"), ("nest","Int -> Doc -> Doc"), ("fillSep","[Doc] -> Doc")]
-- >>> let ptype (name, tp) = fillBreak 5 (pretty name) <+> "::" <+> pretty tp
-- >>> "let" <+> align (vcat (map ptype types))
-- let empty :: Doc
--     nest  :: Int -> Doc -> Doc
--     fillSep
--           :: [Doc] -> Doc
 */

fun <A> fillBreak(atLeastWidth: Int, doc: Doc<A>): Doc<A> = width(doc) { curWidth ->
    if (curWidth > atLeastWidth) {
        nest(atLeastWidth, line_)
    } else {
        spaces(atLeastWidth - curWidth)
    }
}

/**
-- Insert a number of spaces. Negative values count as 0.
 */
fun spaces(n: Int): DocNo = when {
    n <= 0 -> Doc.Empty
    n == 1 -> space
    else -> Doc.Text(repeatChar(' ', n))
}

/**
 * Repeat an arbitrary character.
 */
fun charTimes(c: Char, n: Int): DocNo = when {
    c == '\n' -> stimes(n, Doc.Line)
    n <= 0 -> Doc.Empty
    n == 1 -> Doc.Char(c)
    else -> Doc.Text(repeatChar(c, n))
}

/**
-- `plural n one many` is @one@ if @n@ is @1@, and @many@ otherwise. A
-- typical use case is  adding a plural "s".
--
-- >>> let things = \[True]
-- >>> let amount = length things
-- >>> pretty things <+> "has" <+> pretty amount <+> plural "entry" "entries" amount
-- \[True] has 1 entry
 */
fun <X> plural(one: X, multiple: X, num: Int): X = when (num) {
    1 -> one
    else -> multiple
}

/**
-- `enclose l r x` encloses document @x@ between documents @l@ and @r@
-- using @'<>'@.
--
-- >>> enclose "A" "Z" "·"
-- A·Z
--
-- @
-- 'enclose' l r x = l '<>' x '<>' r
-- @
 */
fun <A> enclose(left: Doc<A>, right: Doc<A>, doc: Doc<A>): Doc<A> = left + doc + right

/**
-- | @('surround' x l r)@ surrounds document @x@ with @l@ and @r@.
--
-- >>> surround "·" "A" "Z"
-- A·Z
--
-- This is merely an argument reordering of @'enclose'@, but allows for
-- definitions like
--
-- >>> concatWith (surround dot) ["Prettyprinter", "Render", "Text"]
-- Prettyprinter.Render.Text
 */
fun <A> surround(doc: Doc<A>, left: Doc<A>, right: Doc<A>): Doc<A> = left + doc + right

/**
-- | Add an annotation to a @'Doc'@. This annotation can then be used by the
-- renderer to e.g. add color to certain parts of the output. For a full
-- tutorial example on how to use it, see the
-- "Prettyprinter.Render.Tutorials.StackMachineTutorial" or
-- "Prettyprinter.Render.Tutorials.TreeRenderingTutorial" modules.
--
-- This function is only relevant for custom formats with their own annotations,
-- and not relevant for basic prettyprinting. The predefined renderers, e.g.
-- "Prettyprinter.Render.Text", should be enough for the most common
-- needs.
 */
fun <A> annotate(ann: A, doc: Doc<A>): Doc<A> = Doc.Annotated(ann, doc)

/**
-- | Remove all annotations.
--
-- Although 'unAnnotate' is idempotent with respect to rendering,
--
-- @
-- 'unAnnotate' . 'unAnnotate' = 'unAnnotate'
-- @
--
-- it should not be used without caution, for each invocation traverses the
-- entire contained document. If possible, it is preferrable to unannotate after
-- producing the layout by using 'unAnnotateS'.
unAnnotate :: Doc ann -> Doc xxx
unAnnotate = alterAnnotations (const [])
 */
fun <A> unAnnotate(doc: Doc<A>): DocNo = alterAnnotations(doc) { listOf() }

/**
-- Change the annotation of a 'Doc'ument.
--
-- Useful in particular to embed documents with one form of annotation in a more
-- generally annotated document.
--
-- Since this traverses the entire @'Doc'@ tree, including parts that are not
-- rendered due to other layouts fitting better, it is preferrable to reannotate
-- after producing the layout by using @'reAnnotateS'@.
--
-- Since @'reAnnotate'@ has the right type and satisfies @'reAnnotate id = id'@,
-- it is used to define the @'Functor'@ instance of @'Doc'@.
reAnnotate :: (ann -> ann') -> Doc ann -> Doc ann'
reAnnotate re = alterAnnotations (pure . re)
 */
fun <A, B> reAnnotate(doc: Doc<A>, alter: (A) -> B): Doc<B> = alterAnnotations(doc) { listOf(alter(it)) }

/**
-- Change the annotations of a 'Doc'ument. Individual annotations can be
-- removed, changed, or replaced by multiple ones.
--
-- This is a general function that combines 'unAnnotate' and 'reAnnotate', and
-- it is useful for mapping semantic annotations (such as »this is a keyword«)
-- to display annotations (such as »this is red and underlined«), because some
-- backends may not care about certain annotations, while others may.
--
-- Annotations earlier in the new list will be applied earlier, i.e. returning
-- @[Bold, Green]@ will result in a bold document that contains green text, and
-- not vice-versa.
--
-- Since this traverses the entire @'Doc'@ tree, including parts that are not
-- rendered due to other layouts fitting better, it is preferrable to reannotate
-- after producing the layout by using @'alterAnnotationsS'@.
 */
fun <A, B> alterAnnotations(doc: Doc<A>, re: (A) -> Iterable<B>): Doc<B> {
    fun go(d: Doc<A>): Doc<B> = when (d) {
        Doc.Empty -> Doc.Empty
        Doc.Fail -> Doc.Fail
        Doc.Line -> Doc.Line
        is Doc.Char -> Doc.Char(d.char)
        is Doc.Text -> Doc.Text(d.text)
        is Doc.FlatAlt -> Doc.FlatAlt(go(d.first), go(d.second))
        is Doc.Cat -> Doc.Cat(go(d.first), go(d.second))
        is Doc.Nest -> Doc.Nest(d.indent, go(d.doc))
        is Doc.Union -> Doc.Union(go(d.first), go(d.second))
        is Doc.Column -> Doc.Column { go(d.react(it)) }
        is Doc.WithPageWidth -> Doc.WithPageWidth { go(d.react(it)) }
        is Doc.Nesting -> Doc.Nesting { go(d.react(it)) }
        is Doc.Annotated -> foldr(go(d.doc), re(d.ann)) { newAnn, newDoc ->
            Doc.Annotated(newAnn, newDoc)
        }
    }
    return go(doc)
}

/**
-- | The data type @SimpleDocStream@ represents laid out documents and is used
-- by the display functions.
--
-- A simplified view is that @'Doc' = ['SimpleDocStream']@, and the layout
-- functions pick one of the 'SimpleDocStream's based on which one fits the
-- layout constraints best. This means that 'SimpleDocStream' has all complexity
-- contained in 'Doc' resolved, making it very easy to convert it to other
-- formats, such as plain text or terminal output.
--
-- To write your own @'Doc'@ to X converter, it is therefore sufficient to
-- convert from @'SimpleDocStream'@. The »Render« submodules provide some
-- built-in converters to do so, and helpers to create own ones.
 */
sealed class SDS<out A> {
    internal object SFail : SDS<Nothing>()
    internal object SEmpty : SDS<Nothing>()
    internal class SChar<A>(val char: Char, val rest: SDS<A>) : SDS<A>()
    internal class SText<A>(val text: CharSequence, val rest: SDS<A>) : SDS<A>() {
        val length: Int
            get() = text.length
    }

    internal class SLine<A>(val indent: Int, val rest: SDS<A>) : SDS<A>()
    internal class SAnnPush<A>(val ann: A, val rest: SDS<A>) : SDS<A>()
    internal class SAnnPop<A>(val rest: SDS<A>) : SDS<A>()

    /**
     * Remove all annotations. 'unAnnotate' for 'SimpleDocStream'.
     */
    fun unAnnotate(): SDS<Nothing> {
        fun go(sds: SDS<A>): SDS<Nothing> = when (sds) {
            SEmpty -> SEmpty
            SFail -> SFail
            is SChar -> SChar(sds.char, go(sds.rest))
            is SText -> SText(sds.text, go(sds.rest))
            is SLine -> SLine(sds.indent, go(sds.rest))
            is SAnnPop -> go(sds.rest)
            is SAnnPush -> go(sds.rest)
        }
        return go(this)
    }

    /**
     * Change the annotation of a document. 'reAnnotate' for 'SimpleDocStream'.
     */
    fun <B> reAnnotate(func: (A) -> B): SDS<B> {
        fun go(sds: SDS<A>): SDS<B> = when (sds) {
            SEmpty -> SEmpty
            SFail -> SFail
            is SChar -> SChar(sds.char, go(sds.rest))
            is SText -> SText(sds.text, go(sds.rest))
            is SLine -> SLine(sds.indent, go(sds.rest))
            is SAnnPop -> SAnnPop(go(sds.rest))
            is SAnnPush -> SAnnPush(func(sds.ann), go(sds.rest))
        }
        return go(this)
    }

    private enum class Removal { Remove, DontRemove }

    /**
    -- Change the annotation of a document to a different annotation, or none at
    -- all. 'alterAnnotations' for 'SimpleDocStream'.
    --
    -- Note that the 'Doc' version is more flexible, since it allows changing a
    -- single annotation to multiple ones.
    -- ('Prettyprinter.Render.Util.SimpleDocTree.SimpleDocTree' restores
    -- this flexibility again.)
     */
    fun <B> alterAnnotations(re: (A) -> B?): SDS<B> {

        fun go(stack: CList<Removal>, sds: SDS<A>): SDS<B> = when (sds) {
            SFail -> SFail
            SEmpty -> SEmpty
            is SChar -> SChar(sds.char, go(stack, sds.rest))
            is SText -> SText(sds.text, go(stack, sds.rest))
            is SLine -> SLine(sds.indent, go(stack, sds.rest))
            is SAnnPush -> when (val a = re(sds.ann)) {
                null -> go(CList.Cons(Removal.Remove, stack), sds.rest)
                else -> SAnnPush(a, go(CList.Cons(Removal.DontRemove, stack), sds.rest))
            }
            is SAnnPop -> when (stack) {
                CList.Nil -> panicPeekedEmpty()
                is CList.Cons -> when (stack.head) {
                    Removal.DontRemove -> SAnnPop(go(stack.tail, sds.rest))
                    Removal.Remove -> go(stack.tail, sds.rest)
                }
            }
        }
        return go(CList.Nil, this)
    }

    /** WhitespaceStrippingState */
    private sealed class Wss {
        class AnnotationLevel(val annLvl: Int) : Wss() {
            fun minusOne(): Wss = AnnotationLevel(annLvl - 1)
            fun plusOne(): Wss = AnnotationLevel(annLvl + 1)
        }

        class RecordedWhitespace(val withheldLines: CList<Int>, val withheldSpaces: Int) : Wss()
    }

    fun removeTrailingWhitespace(): SDS<A> {
        fun prependEmptyLines(ix: CList<Int>, sds0: SDS<A>): SDS<A> {
            return foldr(sds0, ix) { _, sds -> SLine(0, sds) }
        }

        fun commitWhitespace(withheldLines: CList<Int>, withheldSpaces: Int, sds: SDS<A>): SDS<A> {
            return when (withheldLines) {
                CList.Nil -> when (withheldSpaces) {
                    0 -> sds
                    1 -> SChar(' ', sds)
                    else -> SText(repeatChar(' ', withheldSpaces), sds)
                }
                is CList.Cons -> prependEmptyLines(withheldLines.tail, SLine(withheldLines.head + withheldSpaces, sds))
            }
        }

        fun go(state: Wss, sds: SDS<A>): SDS<A> = when (state) {
            is Wss.AnnotationLevel -> when (sds) {
                SFail -> SFail
                SEmpty -> SEmpty
                is SChar -> SChar(sds.char, go(state, sds.rest))
                is SText -> SText(sds.text, go(state, sds.rest))
                is SLine -> SLine(sds.indent, go(state, sds.rest))
                is SAnnPush -> SAnnPush(sds.ann, go(state.plusOne(), sds.rest))
                is SAnnPop -> when {
                    state.annLvl > 1 -> SAnnPop(go(state.plusOne(), sds.rest))
                    else -> SAnnPop(go(Wss.RecordedWhitespace(CList.Nil, 0), sds.rest))
                }
            }
            is Wss.RecordedWhitespace -> when (sds) {
                SFail -> SFail
                SEmpty -> prependEmptyLines(state.withheldLines, SEmpty)
                is SChar -> when (sds.char) {
                    ' ' -> go(Wss.RecordedWhitespace(state.withheldLines, state.withheldSpaces + 1), sds.rest)
                    else -> commitWhitespace(
                        state.withheldLines,
                        state.withheldSpaces,
                        SChar(sds.char, go(Wss.RecordedWhitespace(CList.Nil, 0), sds.rest))
                    )
                }
                is SText -> {
                    val textLength = sds.length
                    val stripped = sds.text.trimEnd(' ')
                    val trailingLength = textLength - stripped.length
                    val isOnlySpace = stripped.isEmpty()
                    if (isOnlySpace) {
                        go(Wss.RecordedWhitespace(state.withheldLines, state.withheldSpaces + textLength), sds.rest)
                    } else {
                        commitWhitespace(
                            state.withheldLines, state.withheldSpaces,
                            SText(
                                stripped, go(
                                    Wss.RecordedWhitespace(CList.Nil, trailingLength),
                                    sds.rest
                                )
                            )
                        )
                    }
                }
                is SLine -> go(Wss.RecordedWhitespace(CList.Cons(sds.indent, state.withheldLines), 0), sds.rest)
                is SAnnPush -> commitWhitespace(
                    state.withheldLines, state.withheldSpaces,
                    SAnnPush(sds.ann, go(Wss.AnnotationLevel(1), sds.rest))
                )
                is SAnnPop -> panicSkippingInUnannotated()
            }
        }
        return go(Wss.RecordedWhitespace(CList.Nil, 0), this)
    }

    /**
     * Alter the document’s annotations.
    --
    -- This instance makes 'SimpleDocStream' more flexible (because it can be used in
    -- 'Functor'-polymorphic values), but @'fmap'@ is much less readable compared to
    -- using @'reAnnotateST'@ in code that only works for @'SimpleDocStream'@ anyway.
    -- Consider using the latter when the type does not matter.
     *
     * Port note: instance Functor
     */
    fun <B> fmap(func: (A) -> B): SDS<B> = reAnnotate(func)

    /*
     * TODO: foldMap and traverse for instances of Foldable and Traversable
     */
}

typealias SimpleDocumentStream<A> = SDS<A>

/**
 * Decide whether a 'SimpleDocStream' fits the constraints given, namely
--
--   - original indentation of the current line
--   - current column
--   - initial indentation of the alternative 'SimpleDocStream' if it
--     starts with a line break (used by 'layoutSmart')
--   - width in which to fit the first line
 *
 * Port note: (Int, Int, Int?, SimpleDocStream<A>) -> Boolean
 */
fun interface FittingPredicate<A> {
    fun test(lineIndent: Int, currentColumn: Int, initialIndentY: Int?, sdoc: SDS<A>): Boolean
}

/** List of nesting level/document pairs yet to be laid out. */
sealed class Lpl<out A> {
    object Nil : Lpl<Nothing>()

    class Cons<A>(val i: Int, val d: Doc<A>, val ds: Lpl<A>) : Lpl<A>()
    class UndoAnn<A>(val ds: Lpl<A>) : Lpl<A>()
}

typealias LayoutPipeline<A> = Lpl<A>

sealed class PageWidth {
    /**
     * Declaration that layouters should not exceed the specified space per line.
     * `avail` is the number of characters, including whitespace, that fit in a line. A typical value is 80.
     * `ribbon` is the ribbon with, i.e. the fraction of the total page width that can be printed on. This allows
     * limiting the length of printable text per line. Values must be between 0 and 1, and 0.4 to 1 is typical.
     */
    class AvailablePerLine(val lineLength: Int, val ribbonFraction: Double) : PageWidth() {
        init {
            require(lineLength > 0) { "chars available per line must be positive" }
            require(ribbonFraction in 0.0..1.0) { "ribbon must be between 0 and 1; 0.4 to 1 is typical" }
        }
    }

    /** Layouters should not introduce line breaks on their own. */
    object Unbounded : PageWidth()

    companion object {
        val default = AvailablePerLine(80, 1.0)
    }
}


/** The remaining width on the current line. */
internal fun remainingWidth(lineLength: Int, ribbonFraction: Double, lineIndent: Int, currentColumn: Int): Int {
    val columnsLeftInLine = lineLength - currentColumn

    val ribbonWidth = max(0, min(lineLength, floor(lineLength * ribbonFraction).toInt()))
    val columnsLeftInRibbon = lineIndent + ribbonWidth - currentColumn

    return min(columnsLeftInLine, columnsLeftInRibbon)
}

/** Options to influence the layout algorithms. */
data class LayoutOptions(val layoutPageWidth: PageWidth) {
    companion object {
        /**
        -- | The default layout options, suitable when you just want some output, and
        -- don’t particularly care about the details. Used by the 'Show' instance, for
        -- example.
         */
        val default = LayoutOptions(PageWidth.default)
    }
}

/**
-- | This is the default layout algorithm, and it is used by 'show', 'putDoc'
-- and 'hPutDoc'.
--
-- @'layoutPretty'@ commits to rendering something in a certain way if the next
-- element fits the layout constraints; in other words, it has one
-- 'SimpleDocStream' element lookahead when rendering. Consider using the
-- smarter, but a bit less performant, @'layoutSmart'@ algorithm if the results
-- seem to run off to the right before having lots of line breaks.
 */
fun <A> layoutPretty(opts: LayoutOptions, doc: Doc<A>): SDS<A> {
    fun fits(width: Int, sds: SDS<A>): Boolean = if (width < 0) {
        false
    } else {
        when (sds) {
            SDS.SFail -> false
            SDS.SEmpty -> true
            is SDS.SChar -> fits(width - 1, sds.rest)
            is SDS.SText -> fits(width - sds.length, sds.rest)
            is SDS.SLine -> true
            is SDS.SAnnPush -> fits(width, sds.rest)
            is SDS.SAnnPop -> fits(width, sds.rest)
        }
    }

    return when (val pw = opts.layoutPageWidth) {
        is PageWidth.AvailablePerLine ->
            layoutWadlerLeijen(pw, doc) { lineIndent, currentColumn, initialIndentY, sdoc ->
                fits(remainingWidth(pw.lineLength, pw.ribbonFraction, lineIndent, currentColumn), sdoc)
            }
        is PageWidth.Unbounded ->
            layoutUnbounded(doc)
    }
}

/**
-- | A layout algorithm with more lookahead than 'layoutPretty', that introduces
-- line breaks earlier if the content does not (or will not, rather) fit into
-- one line.
--
-- Consider the following python-ish document,
--
-- >>> let fun x = hang 2 ("fun(" <> softline' <> x) <> ")"
-- >>> let doc = (fun . fun . fun . fun . fun) (align (list ["abcdef", "ghijklm"]))
--
-- which we’ll be rendering using the following pipeline (where the layout
-- algorithm has been left open):
--
-- >>> import Data.Text.IO as T
-- >>> import Prettyprinter.Render.Text
-- >>> let hr = pipe <> pretty (replicate (26-2) '-') <> pipe
-- >>> let go layouter x = (T.putStrLn . renderStrict . layouter (LayoutOptions (AvailablePerLine 26 1))) (vsep [hr, x, hr])
--
-- If we render this using 'layoutPretty' with a page width of 26 characters
-- per line, all the @fun@ calls fit into the first line so they will be put
-- there:
--
-- >>> go layoutPretty doc
-- |------------------------|
-- fun(fun(fun(fun(fun(
--                   [ abcdef
--                   , ghijklm ])))))
-- |------------------------|
--
-- Note that this exceeds the desired 26 character page width. The same
-- document, rendered with @'layoutSmart'@, fits the layout contstraints:
--
-- >>> go layoutSmart doc
-- |------------------------|
-- fun(
--   fun(
--     fun(
--       fun(
--         fun(
--           [ abcdef
--           , ghijklm ])))))
-- |------------------------|
--
-- The key difference between 'layoutPretty' and 'layoutSmart' is that the
-- latter will check the potential document until it encounters a line with the
-- same indentation or less than the start of the document. Any line encountered
-- earlier is assumed to belong to the same syntactic structure.
-- 'layoutPretty' checks only the first line.
--
-- Consider for example the question of whether the @A@s fit into the document
-- below:
--
-- > 1 A
-- > 2   A
-- > 3  A
-- > 4 B
-- > 5   B
--
-- 'layoutPretty' will check only line 1, ignoring whether e.g. line 2 might
-- already be too wide.
-- By contrast, 'layoutSmart' stops only once it reaches line 4, where the @B@
-- has the same indentation as the first @A@.
 */
fun <A> layoutSmart(opts: LayoutOptions, doc: Doc<A>): SDS<A> = when(val pw = opts.layoutPageWidth) {
    is PageWidth.AvailablePerLine -> {
        fun fits(lineIndent: Int, currentColumn: Int, initialIndentY: Int?, sds: SDS<A>): Boolean {
            val minNestingLevel = min(currentColumn, initialIndentY ?: currentColumn)
            val availableWidth = remainingWidth(pw.lineLength, pw.ribbonFraction, lineIndent, currentColumn)

            fun go(w: Int, sds: SDS<A>): Boolean = w >= 0 && when (sds) {
                SDS.SFail -> false
                SDS.SEmpty -> false
                is SDS.SChar -> go(w - 1, sds.rest)
                is SDS.SText -> go(w - sds.length, sds.rest)
                is SDS.SLine -> minNestingLevel >= sds.indent || go(pw.lineLength - sds.indent, sds.rest)
                is SDS.SAnnPush -> go(w, sds.rest)
                is SDS.SAnnPop -> go(w, sds.rest)
            }
            return go(availableWidth, sds)
        }
        layoutWadlerLeijen(pw, doc, ::fits)
    }
    PageWidth.Unbounded -> layoutUnbounded(doc)
}

/**
 * Layout a document with @Unbounded@ page width.
 * See the Note [Detecting failure with Unbounded page width].
 */
fun <A> layoutUnbounded(doc: Doc<A>): SDS<A> {
    fun failsOnFirstLine(sds: SDS<A>): Boolean {
        fun go(sds: SDS<A>): Boolean = when (sds) {
            SDS.SFail -> true
            SDS.SEmpty -> false
            is SDS.SChar -> go(sds.rest)
            is SDS.SText -> go(sds.rest)
            is SDS.SLine -> false
            is SDS.SAnnPush -> go(sds.rest)
            is SDS.SAnnPop -> go(sds.rest)
        }
        return go(sds)
    }

    return layoutWadlerLeijen(PageWidth.Unbounded, doc) { _, _, _, sdoc -> !failsOnFirstLine(sdoc) }
}

/** The Wadler/Leijen layout algorithm */
fun <A> layoutWadlerLeijen(pageWidth: PageWidth, doc: Doc<A>, pred: FittingPredicate<A>): SDS<A> {
    fun initialIndentation(sds: SDS<A>): Int? = when (sds) {
        is SDS.SLine -> sds.indent
        is SDS.SAnnPush -> initialIndentation(sds.rest)
        is SDS.SAnnPop -> initialIndentation(sds.rest)
        else -> null
    }

    /**
    -- Select the better fitting of two documents:
    -- Choice A if it fits, otherwise choice B.
    --
    -- The fit of choice B is /not/ checked! It is ultimately the user's
    -- responsibility to provide an alternative that can fit the page even when
    -- choice A doesn't.
     */
    fun selectNicer(lineIndent: Int, currentColumn: Int, x: SDS<A>, y: SDS<A>): SDS<A> =
        if (pred.test(lineIndent, currentColumn, initialIndentation(y), x)) {
            x
        } else {
            y
        }

    /**
    -- * current column >= current nesting level
    -- * current column - current indentaion = number of chars inserted in line
    best

    -- * nl: Current nesting level
    -- * cc: Current column, i.e. "where the cursor is"
    -- * lpl: Documents remaining to be handled (in order)
     */
    fun best(nl: Int, cc: Int, lpl: Lpl<A>): SDS<A> = when (lpl) {
        Lpl.Nil -> SDS.SEmpty
        is Lpl.UndoAnn -> SDS.SAnnPop(best(nl, cc, lpl.ds))
        is Lpl.Cons -> when (val d = lpl.d) {
            Doc.Fail -> SDS.SFail
            Doc.Empty -> best(nl, cc, lpl.ds)
            is Doc.Char -> SDS.SChar(d.char, best(nl, cc + 1, lpl.ds))
            is Doc.Text -> SDS.SText(d.text, best(nl, cc + d.length, lpl.ds))
            Doc.Line -> {
                val x = best(lpl.i, lpl.i, lpl.ds)
                val i_ = when (x) {
                    SDS.SEmpty -> 0
                    is SDS.SLine -> 0
                    else -> lpl.i
                }
                SDS.SLine(i_, x)
            }
            is Doc.FlatAlt -> best(nl, cc, Lpl.Cons(lpl.i, d.first, lpl.ds))
            is Doc.Cat -> best(nl, cc, Lpl.Cons(lpl.i, d.first, Lpl.Cons(lpl.i, d.second, lpl.ds)))
            is Doc.Nest -> best(nl, cc, Lpl.Cons(lpl.i + d.indent, d.doc, lpl.ds))
            is Doc.Union -> {
                val first = best(nl, cc, Lpl.Cons(lpl.i, d.first, lpl.ds))
                val second = best(nl, cc, Lpl.Cons(lpl.i, d.second, lpl.ds))
                selectNicer(nl, cc, first, second)
            }
            is Doc.Column -> best(nl, cc, Lpl.Cons(lpl.i, d.react(cc), lpl.ds))
            is Doc.WithPageWidth -> best(nl, cc, Lpl.Cons(lpl.i, d.react(pageWidth), lpl.ds))
            is Doc.Nesting -> best(nl, cc, Lpl.Cons(lpl.i, d.react(lpl.i), lpl.ds))
            is Doc.Annotated -> SDS.SAnnPush(d.ann, best(nl, cc, Lpl.Cons(lpl.i, d.doc, Lpl.UndoAnn(lpl.ds))))
        }
    }

    return best(0, 0, Lpl.Cons(0, doc, Lpl.Nil))
}

/**
-- | @(layoutCompact x)@ lays out the document @x@ without adding any
-- indentation and without preserving annotations.
-- Since no \'pretty\' printing is involved, this layouter is very
-- fast. The resulting output contains fewer characters than a prettyprinted
-- version and can be used for output that is read by other programs.
--
-- >>> let doc = hang 4 (vsep ["lorem", "ipsum", hang 4 (vsep ["dolor", "sit"])])
-- >>> doc
-- lorem
--     ipsum
--     dolor
--         sit
--
-- >>> let putDocCompact = renderIO System.IO.stdout . layoutCompact
-- >>> putDocCompact doc
-- lorem
-- ipsum
-- dolor
-- sit
 */
fun <A> layoutCompact(doc: Doc<A>): SDS<A> {
    fun scan(col: Int, docs: CList<Doc<A>>): SDS<A> = when(docs) {
        is CList.Nil -> SDS.SEmpty
        is CList.Cons -> when(val d = docs.head) {
            Doc.Fail -> SDS.SFail
            Doc.Empty -> SDS.SEmpty
            is Doc.Char -> SDS.SChar(d.char, scan(col + 1, docs.tail))
            is Doc.Text -> SDS.SText(d.text, scan(col + d.length, docs.tail))
            is Doc.FlatAlt -> scan(col, CList.Cons(d.first, docs.tail))
            Doc.Line -> SDS.SLine(0, scan(0, docs.tail))
            is Doc.Cat -> scan(col, CList.Cons(d.first, CList.Cons(d.second, docs.tail)))
            is Doc.Nest -> scan(col, CList.Cons(d.doc, docs.tail))
            is Doc.Union -> scan(col, CList.Cons(d.second, docs.tail))
            is Doc.Column -> scan(col, CList.Cons(d.react(col), docs.tail))
            is Doc.WithPageWidth -> scan(col, CList.Cons(d.react(PageWidth.Unbounded), docs.tail))
            is Doc.Nesting -> scan(col, CList.Cons(d.react(0), docs.tail))
            is Doc.Annotated -> scan(col, CList.Cons(d.doc, docs.tail))
        }
    }
    return scan(0, CList.Cons(doc, CList.Nil))
}
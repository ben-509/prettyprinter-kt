@file:Suppress("MemberVisibilityCanBePrivate", "unused", "SpellCheckingInspection")

package prettyprinter

/*
 * Ported from [Internal.hs](https://hackage.haskell.org/package/prettyprinter-1.7.1/src/src/Prettyprinter/Internal.hs)
 */

import prettyprinter.render.util.panicPeekedEmpty
import prettyprinter.render.util.panicSkippingInUnannotated
import prettyprinter.symbols.space // only use for documentation!
import kotlin.math.floor
import kotlin.math.max
import kotlin.math.min

sealed class Doc<out A> {
    /** Occurs when flattening a line. The layouter will reject this document, choosing a more suitable rendering. */
    internal object Fail : DocNo() {
        override fun dump(): String = "Fail"
    }

    /** The empty document; conceptually the unit of [Cat] */
    internal object Empty : DocNo() {
        override fun dump(): String = "Empty"
    }

    /** An individual character. Effectively a synonym for Text. */
    internal class Char<A>(char: kotlin.Char) : Doc<A>() {
        val text: String = char.toString()
        val length: Int
            get() = 1

        init {
            require(!char.isSurrogate()) { "Don't use surrogates in a Char, use text." }
            require(char != '\n') { "Doc.Char must not be newline; use Line" }
        }

        override fun dump(): String = "Char($text)"
    }

    /** Invariants exists because Text should never overlap Empty, Char or Line */
    internal class Text<A>(val text: CharSequence) : Doc<A>() {
        val length: Int = countCodepoints(text)

        init {
            require(length >= 2) { "Doc.Text must be at least 2 characters; use PChar or Empty" }
            // TODO: remove the check for newlines
            require('\n' !in text) { "Doc.Text must not contain newline; use Line" }
        }

        override fun dump(): String = "Text($text)"
    }

    /** Hard line break. Use helper functions to get line breaks, e.g. [hardline], [softline] and others. */
    internal object Line : DocNo() {
        override fun dump(): String = "Line"
    }

    /**
     * Lay out the first [Doc], but when flattened (via [group]), prefer
     * the second.
     *
     * The layout algorithms work under the assumption that the first
     * alternative is less wide than the flattened second alternative.
     */
    internal class FlatAlt<A>(val first: Doc<A>, val second: Doc<A>) : Doc<A>() {
        override fun dump(): String = "FlatAlt(${first.dump()}, ${second.dump()})"
    }

    /** Concatenation of two documents */
    internal class Cat<A>(val first: Doc<A>, val second: Doc<A>) : Doc<A>() {
        override fun dump(): String = "Cat(${first.dump()}, ${second.dump()})"
    }

    /** Document indented by a number of columns */
    internal class Nest<A>(val indent: Int, val doc: Doc<A>) : Doc<A>() {
        override fun dump(): String = "Nest($indent, ${doc.dump()})"
    }

    /**
     * Invariant: The first lines of first document should be longer than the
     * first lines of the second one, so the layout algorithm can pick the one
     * that fits best. Used to implement layout alternatives for [group].
     */
    internal class Union<A>(val first: Doc<A>, val second: Doc<A>) : Doc<A>() {
        override fun dump(): String = "Union(${first.dump()}, ${second.dump()})"
    }

    /** React on the current cursor position, see [column] */
    internal class Column<A>(val react: (Int) -> Doc<A>) : Doc<A>() {
        override fun dump(): String = "Column(${react})"
    }

    /** React on the document's width, see [pageWidth] */
    internal class WithPageWidth<A>(val react: (PageWidth) -> Doc<A>) : Doc<A>() {
        override fun dump(): String = "WithPageWidth(${react})"
    }

    /** React on the current nesting level, see [nesting] */
    internal class Nesting<A>(val react: (Int) -> Doc<A>) : Doc<A>() {
        override fun dump(): String = "Nesting(${react})"
    }

    /**
     * Add an annotation to the enclosed [Doc]. Can be used for example to add
     * styling directives or alt texts that can then be used by the renderer.
     */
    internal class Annotated<out A>(val ann: A, val doc: Doc<A>) : Doc<A>() {
        override fun dump(): String = "Annotated($ann, ${doc.dump()})"
    }

    /**
     * Port note: selecting "pretty" per these comments on `instance Show`; this is important so that tests behave.
     * `(show doc)` prettyprints document `doc` with [Opts.default],
     * ignoring all annotations.
     * ```
     * instance Show (Doc ann) where
     * showsPrec _ doc = renderShowS (layoutPretty defaultLayoutOptions doc)
     * ```
     */
    override fun toString(): String = toStringPretty()

    private fun quickRender(showAnn: Boolean, sds: SDS<A>): String =
        (if (showAnn) {
            DiagnosticSink()
        } else {
            AppendableSink<A>()
        }).toString(sds)

    fun toStringCompact(showAnn: Boolean = true): String = quickRender(showAnn, layoutCompact(this))

    /** Replaces most uses of [putDoc] in tests. */
    fun toStringPretty(pw: PageWidth = PageWidth.default, showAnn: Boolean = true): String =
        quickRender(showAnn, layoutPretty(Opts(pw), this))

    /** Replaces most uses of [putDocW] in tests. */
    fun toStringPretty(width: Int, ribbon: Double = 1.0, showAnn: Boolean = true): String =
        toStringPretty(PageWidth.AvailablePerLine(width, ribbon), showAnn = showAnn)

    fun toStringSmart(pw: PageWidth = PageWidth.default, showAnn: Boolean = true): String =
        quickRender(showAnn, layoutSmart(Opts(pw), this))

    fun toStringSmart(width: Int, ribbon: Double = 1.0, showAnn: Boolean = true): String =
        toStringSmart(PageWidth.AvailablePerLine(width, ribbon), showAnn = showAnn)

    /** dump without any layout */
    abstract fun dump(): String

    /**
     * These are defined in a companion object to help with loading order. Any internal methods should be reading from
     * this object rather than depending on a definition in symbols.
     */
    internal companion object {
        val squote: DocNo = Char('\'')
        val dquote: DocNo = Char('"')
        val lparen: DocNo = Char('(')
        val lparenSpace: DocNo = Text("( ")
        val rparen: DocNo = Char(')')
        val rparenSpace: DocNo = Text(" )")
        val langle: DocNo = Char('<')
        val rangle: DocNo = Char('>')
        val lbracket: DocNo = Char('[')
        val lbracketSpace: DocNo = Text("[ ")
        val rbracket: DocNo = Char(']')
        val rbracketSpace: DocNo = Text(" ]")
        val lbrace: DocNo = Char('{')
        val lbraceSpace: DocNo = Text("{ ")
        val rbrace: DocNo = Char('}')
        val rbraceSpace: DocNo = Text(" }")
        val semi: DocNo = Char(';')
        val colon: DocNo = Char(':')
        val comma: DocNo = Char(',')
        val commaSpace: DocNo = Text(", ")
        val space: DocNo = Char(' ')
        val dot: DocNo = Char('.')
        val slash: DocNo = Char('/')
        val backslash: DocNo = Char('\\')
        val equalsChar: DocNo = Char('=')
        val pipe: DocNo = Char('|')
    }
}

internal typealias DocNo = Doc<Nothing>

val emptyDoc: DocNo
    get() = Doc.Empty

// Semigroup operations
/**
 * `doc <> doc` emulated through `doc cat doc`
 *
 * Port note: Unfortunately, there's a `cat` function that does something completely different.
 * I'm not sure what to call this, since it's what it does, Cat is the exact same constructor and, per the definition
 * of [Semigroups](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Semigroup.html), `<>` is concatenation.
 * (Those docs don't name `<>` directly, but you can infer it from the function `sconcat`.)
 *
 * I had been overloading plus because of the Kotlin relation to string concatenation, but that seemed confusing
 * with `<+>`.
 *
 * @see [spc] the infix function replacing `<+>`
 */
infix fun <A> Doc<A>.cat(right: Doc<A>): Doc<A> = Doc.Cat(this, right)

fun <A> sconcat(docs: Iterable<Doc<A>>): Doc<A> = hcat(docs)
fun <A> sconcat(vararg docs: Doc<A>): Doc<A> = hcat(*docs)
fun <A> stimes(n: Int, doc: Doc<A>): Doc<A> = when {
    n <= 0 -> Doc.Empty
    n == 1 -> doc
    else -> when (doc) {
        Doc.Fail, Doc.Empty -> doc
        is Doc.Char -> Doc.Text(repeatText(doc.text, n))
        is Doc.Text -> Doc.Text(repeatText(doc.text, n))
        else -> hcat(repeatThing(doc, n))
    }
}

// Monoid definitions
val mempty: DocNo
    get() = Doc.Empty  // emptyDoc, but Haskell defines out of order.

fun <A> mconcat(docs: Iterable<Doc<A>>): Doc<A> = hcat(docs)
fun <A> mconcat(vararg docs: Doc<A>): Doc<A> = hcat(*docs)
fun <A> mappend(left: Doc<A>, right: Doc<A>): Doc<A> = left cat right

private fun unsafe(cs: CharSequence): DocNo = when {
    cs.isEmpty() -> Doc.Empty
    cs.length == 1 -> Doc.Char(cs[0])
    else -> Doc.Text(cs)
}

/**
 * Convert a string to a [Doc]ument, treating newlines as [hardline]s.
 *
 * Port note: not in the source; the Haskell idiom is to use OverloadedStrings. But this is based on
 * `instance Pretty Text where pretty = vsep . map unsafeTextWithoutNewlines . T.splitOn "\n"`
 *
 * TODO: add modes to give us finer grained control over newlines.
 * @see [pretty] for our rough port of the Pretty typeclass.
 * @see [texts] to create a quick list of text docs
 * @see [words] for another way to create a list of text docs
 * @see [reflow] to reflow a paragraph of text
 */
fun text(cs: CharSequence): DocNo = vsep(cs.split('\n').map { unsafe(it) })

/**
 * Encodes a single BMP character. To encode higher plane characters, use [text].
 *
 * @throws IllegalArgumentException if a surrogate character is passed.
 *
 * Port notes: There's a bit of a disconnect here. Kotlin is a 16-bit char language, while Haskell is a 32-bit char.
 * As the support for individual characters doesn't add anything, and in Haskell it's abstracted away anyway,
 * I'm going to keep the API simple by pushing that support into strings.
 */
fun char(c: Char): DocNo = when {
    c == '\n' -> Doc.Line
    c.isSurrogate() -> throw IllegalArgumentException("Can't create a doc from a surrogate; use `text`.")
    else -> Doc.Char(c)
}

/**
 * Helper for the Haskell rules for prettifying, especially Haskell's list style.
 *
 * This is reimplementing the Haskell style, except that booleans aren't capitalized. It's not meant to be complete,
 * so it's simple and everything useful is public.
 */
fun prettyDefaultHelper(e: Any?): DocNo = when (e) {
    is CharSequence -> pretty(e)
    is Iterable<*> -> pretty(e, ::prettyDefaultHelper)
    is Unit -> prettyUnit()
    is Boolean -> pretty(e)
    is Number -> pretty(e)
    is Char -> pretty(e)
    is Pair<*, *> -> pretty(e, ::prettyDefaultHelper)
    is Triple<*, *, *> -> pretty(e, ::prettyDefaultHelper)
    null -> prettyNull()
    else -> text(e.toString())
}

/** Pretty overload for strings; calls [text]. */
fun pretty(e: CharSequence): DocNo = text(e)

/** Pretty overload for iterables; creates an [align]ed [list]. You can provide your own helper. */
fun <A> pretty(e: Iterable<*>, help: (Any?) -> Doc<A> = ::prettyDefaultHelper): Doc<A> = align(list(e.map(help)))

/** Pretty function for Unit, treated as an empty tuple. */
fun prettyUnit(): DocNo = unsafe("()")

/** Pretty overload for booleans; uses Kotlin's toString. */
fun pretty(e: Boolean): DocNo = unsafe(e.toString())

/** Pretty overload for numbers; uses Kotlin's toString. */
fun pretty(e: Number): DocNo = unsafe(e.toString())

/** Pretty overload for BMP chars; uses [char]. */
fun pretty(e: Char): DocNo = char(e)

/** Pretty overload for [Pair]s; uses [tupled]. You can provide your own helper. */
fun <A> pretty(e: Pair<*, *>, help: (Any?) -> Doc<A> = ::prettyDefaultHelper): Doc<A> =
    tupled(help(e.first), help(e.second))

/** Pretty overload for [Triple]s; uses [tupled]. You can provide your own helper. */
fun <A> pretty(e: Triple<*, *, *>, help: (Any?) -> Doc<A> = ::prettyDefaultHelper): Doc<A> =
    tupled(help(e.first), help(e.second), help(e.third))

/** Pretty function for null, just the word `null`. */
fun prettyNull(): DocNo = unsafe("null")

/**
 * `nest i x` lays out the document `x` with the current nesting level
 * (indentation of the following lines) increased by `i`. Negative values are
 * allowed, and decrease the nesting level accordingly.
 *
 *     >>> vsep [nest 4 (vsep ["lorem", "ipsum", "dolor"]), "sit", "amet"]
 *     lorem
 *         ipsum
 *         dolor
 *     sit
 *     amet
 *
 * See also:
 *
 *  * [hang] ([nest] relative to current cursor position instead of
 *     current nesting level)
 *  * [align] (set nesting level to current cursor position)
 *  * [indent] (increase indentation on the spot, padding with [spaces]).
 */
fun <A> nest(i: Int, x: Doc<A>) = when (i) {
    0 -> x
    else -> Doc.Nest(i, x)
}

/**
 * The [line] document advances to the next line and indents to the current
 * nesting level.
 *
 *     >>> let doc = "lorem ipsum" <> line <> "dolor sit amet"
 *     >>> doc
 *     lorem ipsum
 *     dolor sit amet
 *
 * [line] behaves like [space] if the line break is undone by [group]:
 *
 *     >>> group doc
 *     lorem ipsum dolor sit amet
 */
val line: DocNo
    get() = Doc.FlatAlt(Doc.Line, Doc.space)

/**
 * [line_] is like [line], but behaves like [mempty] if the line break
 * is undone by [group] (instead of [space]).
 *
 *     >>> val doc = "lorem ipsum" <> line_ <> "dolor sit amet"
 *     >>> doc
 *     lorem ipsum
 *     dolor sit amet
 *     >>> group doc
 *     lorem ipsumdolor sit amet
 */
val line_: DocNo
    get() = Doc.FlatAlt(Doc.Line, Doc.Empty)

/**
 * [softline] behaves like [space] if the resulting output fits the page,
 * otherwise like [line].
 *
 * Here, we have enough space to put everything in one line:
 *
 *     >>> let doc = "lorem ipsum" <> softline <> "dolor sit amet"
 *     >>> putDocW 80 doc
 *     lorem ipsum dolor sit amet
 *
 * If we narrow the page to width 10, the layouter produces a line break:
 *
 *     >>> putDocW 10 doc
 *     lorem ipsum
 *     dolor sit amet
 */
val softline: DocNo
    get() = Doc.Union(Doc.space, Doc.Line)

/**
 * [softline_] is like [softline], but behaves like [mempty] if the
 * resulting output does not fit on the page (instead of [space]). In other
 * words, [line] is to [line_] how [softline] is to [softline_].
 *
 * With enough space, we get direct concatenation:
 *
 *     >>> let doc = "ThisWord" <> softline_ <> "IsWayTooLong"
 *     >>> putDocW 80 doc
 *     ThisWordIsWayTooLong
 *
 * If we narrow the page to width 10, the layouter produces a line break:
 *
 *     >>> putDocW 10 doc
 *     ThisWord
 *     IsWayTooLong
 *
 * ```
 * [softline_] = [group] [line]'
 * ```
 */

val softline_: DocNo
    get() = Doc.Union(Doc.Empty, Doc.Line)

/**
 * A [hardline] is _always_ laid out as a line break, even when [group]ed or
 * when there is plenty of space. Note that it might still be simply discarded
 * if it is part of a [flatAlt] inside a [group].
 *
 *     >>> let doc = "lorem ipsum" <> hardline <> "dolor sit amet"
 *     >>> putDocW 1000 doc
 *     lorem ipsum
 *     dolor sit amet
 *
 *     >>> group doc
 *     lorem ipsum
 *     dolor sit amet
 */
val hardline: DocNo
    get() = Doc.Line

/**
 * `group x` tries laying out `x` into a single line by removing the
 * contained line breaks; if this does not fit the page, or when a [hardline]
 * within `x` prevents it from being flattened, `x` is laid out without any
 * changes.
 *
 * The [group] function is key to layouts that adapt to available space nicely.
 *
 * See [vcat], [line], or [flatAlt] for examples that are related, or make good
 * use of it.
 */
fun <A> group(doc: Doc<A>): Doc<A> = when (doc) {
    is Doc.Union -> doc
    is Doc.FlatAlt -> when (val chg = Flatten.changesUpon(doc.second)) {
        is Flatten.Flattened -> Doc.Union(chg.doc, doc.first)
        Flatten.AlreadyFlat -> Doc.Union(doc.second, doc.first)
        Flatten.NeverFlat -> doc.first
    }
    else -> when (val chg = Flatten.changesUpon(doc)) {
        is Flatten.Flattened -> Doc.Union(chg.doc, doc)
        Flatten.AlreadyFlat -> doc
        Flatten.NeverFlat -> doc
    }
}

sealed class Flatten<out F> {
    /** `doc` is likely flatter than the input */
    class Flattened<F>(val doc: F) : Flatten<F>()

    /** The input was already flat, e.g. a [Doc.Text]. */
    object AlreadyFlat : Flatten<Nothing>()

    /** The input couldn't be flattened: It contained a [Doc.Line] or [Doc.Fail]. */
    object NeverFlat : Flatten<Nothing>()

    companion object {
        /**
         * Port note: instance of Functor
         */
        fun <X, Y> fmap(f: (X) -> Y, g: Flatten<X>): Flatten<Y> = when (g) {
            is AlreadyFlat -> AlreadyFlat
            is NeverFlat -> NeverFlat
            is Flattened -> Flattened(f(g.doc))
        }

        /**
         * Choose the first element of each [Doc.Union], and discard the first field of
         * all [Doc.FlatAlt]s.
         *
         * The result is [Flattened] if the element might change depending on the layout
         * algorithm (i.e. contains differently renderable sub-documents), and [AlreadyFlat]
         * if the document is static (e.g. contains only a plain [Doc.Empty] node).
         * [NeverFlat] is returned when the document cannot be flattened because it
         * contains a hard [Doc.Line] or [Doc.Fail].
         * See [Group: special flattening] for further explanations.
         */
        fun <A> changesUpon(doc: Doc<A>): Flatten<Doc<A>> = when (doc) {
            is Doc.FlatAlt -> Flattened(flatten(doc.second))
            Doc.Line -> NeverFlat
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
                    cuFirst is Flattened && cuSecond is Flattened -> Flattened(Doc.Cat(cuFirst.doc, cuSecond.doc))
                    cuFirst is Flattened && cuSecond is AlreadyFlat -> Flattened(Doc.Cat(cuFirst.doc, doc.second))
                    cuFirst is AlreadyFlat && cuSecond is Flattened -> Flattened(Doc.Cat(doc.first, cuSecond.doc))
                    else -> AlreadyFlat
                }
            }
            Doc.Empty, is Doc.Char, is Doc.Text -> AlreadyFlat
            Doc.Fail -> NeverFlat
        }

        /** Flatten, but don???t report whether anything changes. */
        private fun <A> flatten(doc: Doc<A>): Doc<A> = when (doc) {
            is Doc.FlatAlt -> flatten(doc.second)
            is Doc.Cat -> flatten(doc.first) cat flatten(doc.second)
            is Doc.Nest -> nest(doc.indent, flatten(doc.doc))
            Doc.Line -> Doc.Fail
            is Doc.Union -> flatten(doc.first)
            is Doc.Column -> Doc.Column { flatten(doc.react(it)) }
            is Doc.WithPageWidth -> Doc.WithPageWidth { flatten(doc.react(it)) }
            is Doc.Nesting -> Doc.Nesting { flatten(doc.react(it)) }
            is Doc.Annotated -> Doc.Annotated(doc.ann, flatten(doc.doc))
            Doc.Fail, Doc.Empty, is Doc.Char, is Doc.Text -> doc
        }
    }
}

/**
 * By default, `flatAlt x y` renders as `x`. However when [group]ed,
 * `y` will be preferred, with `x` as the fallback for the case when `y`
 * doesn't fit.
 *
 *     >>> let doc = flatAlt "a" "b"
 *     >>> putDoc doc
 *     a
 *     >>> putDoc (group doc)
 *     b
 *     >>> putDocW 0 (group doc)
 *     a
 *
 * [flatAlt] is particularly useful for defining conditional separators such as
 *
 * ```
 * softline = group (flatAlt hardline " ")
 * ```
 *
 *     >>> let hello = "Hello" <> softline <> "world!"
 *     >>> putDocW 12 hello
 *     Hello world!
 *     >>> putDocW 11 hello
 *     Hello
 *     world!
 *
 * ### Example: Haskell's do-notation
 *
 * We can use this to render Haskell's do-notation nicely:
 *
 *     >>> let open        = flatAlt "" "{ "
 *     >>> let close       = flatAlt "" " }"
 *     >>> let separator   = flatAlt "" "; "
 *     >>> let prettyDo xs = group ("do" <+> align (encloseSep open close separator xs))
 *     >>> let statements  = ["name:_ <- getArgs", "let greet = \"Hello, \" <> name", "putStrLn greet"]
 *
 * This is put into a single line with `{;}` style if it fits:
 *
 *     >>> putDocW 80 (prettyDo statements)
 *     do { name:_ <- getArgs; let greet = "Hello, " <> name; putStrLn greet }
 *
 * When there is not enough space the statements are broken up into lines
 * nicely:
 *
 *     >>> putDocW 10 (prettyDo statements)
 *     do name:_ <- getArgs
 *        let greet = "Hello, " <> name
 *        putStrLn greet
 *
 * ### Notes
 *
 * Users should be careful to choose `x` to be less wide than `y`.
 * Otherwise, if `y` turns out not to fit the page, we fall back on an even
 * wider layout:
 *
 *     >>> let ugly = group (flatAlt "even wider" "too wide")
 *     >>> putDocW 7 ugly
 *     even wider
 *
 * Also note that [group] will flatten `y`:
 *
 *     >>> putDoc (group (flatAlt "x" ("y" <> line <> "y")))
 *     y y
 *
 * This also means that an "unflattenable" `y` which contains a hard linebreak
 * will /never/ be rendered:
 *
 *     >>> putDoc (group (flatAlt "x" ("y" <> hardline <> "y")))
 *     x
 */
fun <A> flatAlt(default: Doc<A>, preferred: Doc<A>): Doc<A> = Doc.FlatAlt(default, preferred)

/**
 * [align] lays out the document `x` with the nesting level set to the
 * current column. It is used for example to implement [hang].
 *
 * As an example, we will put a document right above another one, regardless of
 * the current nesting level. Without [align]ment, the second line is put simply
 * below everything we've had so far:
 *
 *     >>> "lorem" <+> vsep ["ipsum", "dolor"]
 *     lorem ipsum
 *     dolor
 *
 * If we add an [align] to the mix, the [vsep]'s contents all start in the
 * same column:
 *
 *     >>> "lorem" <+> align (vsep ["ipsum", "dolor"])
 *     lorem ipsum
 *           dolor
 */
fun <A> align(d: Doc<A>): Doc<A> = column { k -> nesting { i -> nest(k - i, d) } }

/**
 * `hang i x` lays out the document `x` with a nesting level set to the
 * _current column_ plus `i`. Negative values are allowed, and decrease the
 * nesting level accordingly.
 *
 *     >>> let doc = reflow "Indenting these words with hang"
 *     >>> putDocW 24 ("prefix" <+> hang 4 doc)
 *     prefix Indenting these
 *                words with
 *                hang
 *
 * This differs from [nest], which is based on the /current nesting level/ plus
 * `i`. When you're not sure, try the more efficient [nest] first. In our
 * example, this would yield
 *
 *     >>> let doc = reflow "Indenting these words with nest"
 *     >>> putDocW 24 ("prefix" <+> nest 4 doc)
 *     prefix Indenting these
 *         words with nest
 *
 * ```
 * hang i doc = align (nest i doc)
 * ```
 */
fun <A> hang(i: Int, d: Doc<A>): Doc<A> = align(nest(i, d))

/**
 * `indent i x` indents document `x` by `i` columns, starting from the
 * current cursor position.
 *
 *     >>> let doc = reflow "The indent function indents these words!"
 *     >>> putDocW 24 ("prefix" <> indent 4 doc)
 *     prefix    The indent
 *               function
 *               indents these
 *               words!
 *
 * ```
 * indent i d = hang i ({i spaces} <> d)
 * ```
indent
:: Int  * ^ Number of spaces to increase indentation by
-> Doc ann
-> Doc ann
indent i d = hang i (spaces i <> d)
 */
fun <A> indent(i: Int, d: Doc<A>): Doc<A> = hang(i, spaces(i) cat d)

/**
 * `encloseSep l r sep xs` concatenates the documents `xs` separated by
 * `sep`, and encloses the resulting document by `l` and `r`.
 *
 * The documents are laid out horizontally if that fits the page:
 *
 *     >>> let doc = "list" <+> align (encloseSep lbracket rbracket comma (map pretty [1,20,300,4000]))
 *     >>> putDocW 80 doc
 *     list [1,20,300,4000]
 *
 * If there is not enough space, then the input is split into lines entry-wise
 * therwise they are laid out vertically, with separators put in the front:
 *
 *     >>> putDocW 10 doc
 *     list [1
 *          ,20
 *          ,300
 *          ,4000]
 *
 * Note that `doc` contains an explicit call to [align] so that the list items
 * are aligned vertically.
 *
 * For putting separators at the end of entries instead, have a look at
 * [punctuate].
 */

fun <A> encloseSep(leftDelim: Doc<A>, rightDelim: Doc<A>, separator: Doc<A>, inputs: Iterable<Doc<A>>): Doc<A> {
    val docs = inputs.mapWhere { where, doc ->
        when (where) {
            Where.FIRST, Where.ONLY -> leftDelim cat doc
            Where.MIDDLE, Where.LAST -> separator cat doc
        }
    }.toList()
    return if (docs.isEmpty()) {
        leftDelim cat rightDelim
    } else {
        cat(docs) cat rightDelim
    }
}

fun <A> encloseSep(leftDelim: Doc<A>, rightDelim: Doc<A>, separator: Doc<A>, vararg inputs: Doc<A>): Doc<A> =
    encloseSep(leftDelim, rightDelim, separator, inputs.asIterable())

/**
 * Haskell-inspired variant of [encloseSep] with braces and comma as
 * separator.
 *
 *     >>> let doc = list (map pretty [1,20,300,4000])
 *
 *     >>> putDocW 80 doc
 *     [1, 20, 300, 4000]
 *
 *     >>> putDocW 10 doc
 *     [ 1
 *     , 20
 *     , 300
 *     , 4000 ]
 */
fun <A> list(docs: Iterable<Doc<A>>): Doc<A> =
    group(
        encloseSep(
            flatAlt(Doc.lbracketSpace, Doc.lbracket),
            flatAlt(Doc.rbracketSpace, Doc.rbracket),
            Doc.commaSpace,
            docs
        )
    )

fun <A> list(vararg docs: Doc<A>): Doc<A> =
    group(
        encloseSep(
            flatAlt(Doc.lbracketSpace, Doc.lbracket),
            flatAlt(Doc.rbracketSpace, Doc.rbracket),
            Doc.commaSpace,
            *docs
        )
    )


/**
 * Haskell-inspired variant of [encloseSep] with parentheses and comma as
 * separator.
 *
 *     >>> let doc = tupled (map pretty [1,20,300,4000])
 *
 *     >>> putDocW 80 doc
 *     (1, 20, 300, 4000)
 *
 *     >>> putDocW 10 doc
 *     ( 1
 *     , 20
 *     , 300
 *     , 4000 )
 */

fun <A> tupled(docs: Iterable<Doc<A>>): Doc<A> =
    group(encloseSep(flatAlt(Doc.lparenSpace, Doc.lparen), flatAlt(Doc.rparenSpace, Doc.rparen), Doc.commaSpace, docs))

fun <A> tupled(vararg docs: Doc<A>): Doc<A> =
    group(encloseSep(flatAlt(Doc.lparenSpace, Doc.lparen), flatAlt(Doc.rparenSpace, Doc.rparen), Doc.commaSpace, *docs))

/**
 * `x <+> y` concatenates document `x` and `y` with a `space` in
 * between.
 *
 *     >>> "hello" <+> "world"
 *     hello world
 *
 * ```
 * x '<+>' y = x '<>' [space] '<>' y
 * ```
 */
infix fun <A> Doc<A>.spc(right: Doc<A>): Doc<A> = this cat Doc.space cat right

/**
 * Concatenate all documents element-wise with a binary function.
 *
 * ```
 * [concatWith] _ [] = [mempty]
 * [concatWith] (**) [x,y,z] = x ** y ** z
 * ```
 *
 * Multiple convenience definitions based on [concatWith] are already predefined,
 * for example:
 *
 * ```
 * [hsep]    = [concatWith] ('<+>')
 * [fillSep] = [concatWith] (\\x y -> x '<>' [softline] '<>' y)
 * ```
 *
 * This is also useful to define customized joiners:
 *
 *     >>> concatWith (surround dot) ["Prettyprinter", "Render", "Text"]
 *     Prettyprinter.Render.Text
 */
fun <A> concatWith(docs: Iterable<Doc<A>>, func: (Doc<A>, Doc<A>) -> Doc<A>): Doc<A> = concatWith(docs.iterator(), func)
fun <A> concatWith(docs: Array<out Doc<A>>, func: (Doc<A>, Doc<A>) -> Doc<A>): Doc<A> =
    concatWith(docs.iterator(), func)

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
 * `hsep xs` concatenates all documents `xs` horizontally with `<+>`,
 * i.e. it puts a space between all entries.
 *
 *     >>> let docs = Util.words "lorem ipsum dolor sit amet"
 *
 *     >>> hsep docs
 *     lorem ipsum dolor sit amet
 *
 * `[hsep]` does not introduce line breaks on its own, even when the page is too
 * narrow:
 *
 *     >>> putDocW 5 (hsep docs)
 *     lorem ipsum dolor sit amet
 *
 * For automatic line breaks, consider using [fillSep] instead.
 */
fun <A> hsep(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { a, b -> a spc b }
fun <A> hsep(vararg docs: Doc<A>): Doc<A> = concatWith(docs) { a, b -> a spc b }

/**
 * `vsep xs` concatenates all documents `xs` above each other. If a
 * [group] undoes the line breaks inserted by `vsep`, the documents are
 * separated with a [space] instead.
 *
 * Using [vsep] alone yields
 *
 *     >>> "prefix" <+> vsep ["text", "to", "lay", "out"]
 *     prefix text
 *     to
 *     lay
 *     out
 *
 * [group]ing a [vsep] separates the documents with a [space] if it fits the
 * page (and does nothing otherwise). See the `[sep]` convenience function for
 * this use case.
 *
 * The [align] function can be used to align the documents under their first
 * element:
 *
 *     >>> "prefix" <+> align (vsep ["text", "to", "lay", "out"])
 *     prefix text
 *            to
 *            lay
 *            out
 *
 * Since [group]ing a [vsep] is rather common, [sep] is a built-in for doing
 * that.
 */
fun <A> vsep(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { x, y -> x cat line cat y }
fun <A> vsep(vararg docs: Doc<A>): Doc<A> = concatWith(docs) { x, y -> x cat line cat y }


/**
 * `fillSep xs` concatenates the documents `xs` horizontally with `<+>`
 * as long as it fits the page, then inserts a `line` and continues doing that
 * for all documents in `xs`. (`line` means that if [group]ed, the documents
 * are separated with a [space] instead of newlines. Use [fillCat] if you do not
 * want a [space].)
 *
 * Let's print some words to fill the line:
 *
 *     >>> let docs = take 20 (cycle ["lorem", "ipsum", "dolor", "sit", "amet"])
 *     >>> putDocW 80 ("Docs:" <+> fillSep docs)
 *     Docs: lorem ipsum dolor sit amet lorem ipsum dolor sit amet lorem ipsum dolor
 *     sit amet lorem ipsum dolor sit amet
 *
 * The same document, printed at a width of only 40, yields
 *
 *     >>> putDocW 40 ("Docs:" <+> fillSep docs)
 *     Docs: lorem ipsum dolor sit amet lorem
 *     ipsum dolor sit amet lorem ipsum dolor
 *     sit amet lorem ipsum dolor sit amet
fillSep :: [Doc ann] -> Doc ann
fillSep = concatWith (\x y -> x <> softline <> y)
 */
fun <A> fillSep(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { x, y -> x cat softline cat y }
fun <A> fillSep(vararg docs: Doc<A>): Doc<A> = concatWith(docs) { x, y -> x cat softline cat y }

/**
 * `sep xs` tries laying out the documents `xs` separated with [space]s,
 * and if this does not fit the page, separates them with newlines. This is what
 * differentiates it from [vsep], which always lays out its contents beneath
 * each other.
 *
 *     >>> let doc = "prefix" <+> sep ["text", "to", "lay", "out"]
 *     >>> putDocW 80 doc
 *     prefix text to lay out
 *
 * With a narrower layout, the entries are separated by newlines:
 *
 *     >>> putDocW 20 doc
 *     prefix text
 *     to
 *     lay
 *     out
 *
 * ```
 * [sep] = [group] . [vsep]
 * ```
 */
fun <A> sep(docs: Iterable<Doc<A>>): Doc<A> = group(vsep(docs))
fun <A> sep(vararg docs: Doc<A>): Doc<A> = group(vsep(*docs))

/**

 * `([hcat] xs)` concatenates all documents `xs` horizontally with `'<>'`
 * (i.e. without any spacing).
 *
 * It is provided only for consistency, since it is identical to [mconcat].
 *
 *     >>> let docs = Util.words "lorem ipsum dolor"
 *     >>> hcat docs
 *     loremipsumdolor
 */
fun <A> hcat(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { a, b -> a cat b }
fun <A> hcat(vararg docs: Doc<A>): Doc<A> = concatWith(docs) { a, b -> a cat b }

/**
 * `([vcat] xs)` vertically concatenates the documents `xs`. If it is
 * [group]ed, the line breaks are removed.
 *
 * In other words `[vcat]` is like `[vsep]`, with newlines removed instead of
 * replaced by [space]s.
 *
 *     >>> let docs = Util.words "lorem ipsum dolor"
 *     >>> vcat docs
 *     lorem
 *     ipsum
 *     dolor
 *     >>> group (vcat docs)
 *     loremipsumdolor
 *
 * Since [group]ing a [vcat] is rather common, [cat] is a built-in shortcut for
 * it.
 */
fun <A> vcat(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { a, b -> a cat line_ cat b }
fun <A> vcat(vararg docs: Doc<A>): Doc<A> = concatWith(docs) { a, b -> a cat line_ cat b }

/**
 * `fillCat xs` concatenates documents `xs` horizontally with `'<>'` as
 * long as it fits the page, then inserts a `[line_]` and continues doing that
 * for all documents in `xs`. This is similar to how an ordinary word processor
 * lays out the text if you just keep typing after you hit the maximum line
 * length.
 *
 * (`[line_]` means that if [group]ed, the documents are separated with nothing
 * instead of newlines. See [fillSep] if you want a [space] instead.)
 *
 * Observe the difference between [fillSep] and [fillCat]. [fillSep]
 * concatenates the entries [space]d when [group]ed:
 *
 *     >>> let docs = take 20 (cycle (["lorem", "ipsum", "dolor", "sit", "amet"]))
 *     >>> putDocW 40 ("Grouped:" <+> group (fillSep docs))
 *     Grouped: lorem ipsum dolor sit amet
 *     lorem ipsum dolor sit amet lorem ipsum
 *     dolor sit amet lorem ipsum dolor sit
 *     amet
 *
 * On the other hand, [fillCat] concatenates the entries directly when
 * [group]ed:
 *
 *     >>> putDocW 40 ("Grouped:" <+> group (fillCat docs))
 *     Grouped: loremipsumdolorsitametlorem
 *     ipsumdolorsitametloremipsumdolorsitamet
 *     loremipsumdolorsitamet
fillCat :: [Doc ann] -> Doc ann
fillCat = concatWith (\x y -> x <> softline' <> y)
 */
fun <A> fillCat(docs: Iterable<Doc<A>>): Doc<A> = concatWith(docs) { x, y -> x cat softline_ cat y }
fun <A> fillCat(vararg docs: Doc<A>): Doc<A> = concatWith(docs) { x, y -> x cat softline_ cat y }

/**
 * `cat xs` tries laying out the documents `xs` separated with nothing,
 * and if this does not fit the page, separates them with newlines. This is what
 * differentiates it from [vcat], which always lays out its contents beneath
 * each other.
 *
 *     >>> let docs = Util.words "lorem ipsum dolor"
 *     >>> putDocW 80 ("Docs:" <+> cat docs)
 *     Docs: loremipsumdolor
 *
 * When there is enough space, the documents are put above one another:
 *
 *     >>> putDocW 10 ("Docs:" <+> cat docs)
 *     Docs: lorem
 *     ipsum
 *     dolor
 *
 * ```
 * [cat] = [group] . [vcat]
 * ```
 */
fun <A> cat(docs: Iterable<Doc<A>>): Doc<A> = group(vcat(docs))
fun <A> cat(vararg docs: Doc<A>): Doc<A> = group(vcat(*docs))

/**
 * `punctuate p xs` appends `p` to all but the last document in `xs`.
 *
 *     >>> let docs = punctuate comma (Util.words "lorem ipsum dolor sit amet")
 *     >>> putDocW 80 (hsep docs)
 *     lorem, ipsum, dolor, sit, amet
 *
 * The separators are put at the end of the entries, which we can see if we
 * position the result vertically:
 *
 *     >>> putDocW 20 (vsep docs)
 *     lorem,
 *     ipsum,
 *     dolor,
 *     sit,
 *     amet
 *
 * If you want to put the commas in front of their elements instead of at the end,
 * you should use [tupled] or, in general, [encloseSep].
 */
fun <A> punctuate(pun: Doc<A>, docs: Iterable<Doc<A>>): Iterable<Doc<A>> = docs.mapWhere { where, doc: Doc<A> ->
    when (where) {
        Where.LAST -> doc
        else -> doc cat pun
    }
}

fun <A> punctuate(pun: Doc<A>, vararg docs: Doc<A>): Iterable<Doc<A>> = punctuate(pun, *docs)

/**
 * Layout a document depending on which column it starts at. [align] is
 * implemented in terms of [column].
 *
 *     >>> column (\l -> "Columns are" <+> pretty l <> "-based.")
 *     Columns are 0-based.
 *
 *     >>> let doc = "prefix" <+> column (\l -> "| <- column" <+> pretty l)
 *     >>> vsep [indent n doc | n <- [0,4,8]]
 *     prefix | <- column 7
 *         prefix | <- column 11
 *             prefix | <- column 15
 */
fun <A> column(react: (Int) -> Doc<A>): Doc<A> = Doc.Column(react)

/**
 * Layout a document depending on the current [nest]ing level. [align] is
 * implemented in terms of [nesting].
 *
 *     >>> let doc = "prefix" <+> nesting (\l -> brackets ("Nested:" <+> pretty l))
 *     >>> vsep [indent n doc | n <- [0,4,8]]
 *     prefix [Nested: 0]
 *         prefix [Nested: 4]
 *             prefix [Nested: 8]
 */
fun <A> nesting(react: (Int) -> Doc<A>): Doc<A> = Doc.Nesting(react)

/**
 * `width doc f` lays out the document [doc], and makes the column width
 * of it available to a function.
 *
 *     >>> let annotate doc = width (brackets doc) (\w -> " <- width:" <+> pretty w)
 *     >>> align (vsep (map annotate [" *-", " * * *", indent 3 " *-", vsep [" *-", indent 4 " *-"]]))
 *     [ *-] <- width: 5
 *     [ * * *] <- width: 8
 *     [    *-] <- width: 8
 *     [ *-
 *          *-] <- width: 8
 */
fun <A> width(doc: Doc<A>, react: (Int) -> Doc<A>): Doc<A> =
    column { colStart -> doc cat column { colEnd -> react(colEnd - colStart) } }

/**
 * Layout a document depending on the page width, if one has been specified.
 *
 *     >>> let prettyPageWidth (AvailablePerLine l r) = "Width:" <+> pretty l <> ", ribbon fraction:" <+> pretty r
 *     >>> let doc = "prefix" <+> pageWidth (brackets . prettyPageWidth)
 *     >>> putDocW 32 (vsep [indent n doc | n <- [0,4,8]])
 *     prefix [Width: 32, ribbon fraction: 1.0]
 *         prefix [Width: 32, ribbon fraction: 1.0]
 *             prefix [Width: 32, ribbon fraction: 1.0]
 */
fun <A> pageWidth(react: (PageWidth) -> Doc<A>): Doc<A> = Doc.WithPageWidth(react)

/**
 * `([fill] i x)` lays out the document `x`. It then appends `space`s until
 * the width is equal to `i`. If the width of `x` is already larger, nothing is
 * appended.
 *
 * This function is quite useful in practice to output a list of bindings:
 *
 *     >>> let types = [("empty","Doc"), ("nest","Int -> Doc -> Doc"), ("fillSep","[Doc] -> Doc")]
 *     >>> let ptype (name, tp) = fill 5 (pretty name) <+> "::" <+> pretty tp
 *     >>> "let" <+> align (vcat (map ptype types))
 *     let empty :: Doc
 *         nest  :: Int -> Doc -> Doc
 *         fillSep :: [Doc] -> Doc
 */

fun <A> fill(atLeastWidth: Int, doc: Doc<A>): Doc<A> = width(doc) { spaces(atLeastWidth - it) }

/**
 * `([fillBreak] i x)` first lays out the document `x`. It then appends `space`s
 * until the width is equal to `i`. If the width of `x` is already larger than
 * `i`, the nesting level is increased by `i` and a `line` is appended. When we
 * redefine `ptype` in the example given in [fill] to use `[fillBreak]`, we get
 * a useful variation of the output:
 *
 *     >>> let types = [("empty","Doc"), ("nest","Int -> Doc -> Doc"), ("fillSep","[Doc] -> Doc")]
 *     >>> let ptype (name, tp) = fillBreak 5 (pretty name) <+> "::" <+> pretty tp
 *     >>> "let" <+> align (vcat (map ptype types))
 *     let empty :: Doc
 *         nest  :: Int -> Doc -> Doc
 *         fillSep
 *               :: [Doc] -> Doc
 */

fun <A> fillBreak(atLeastWidth: Int, doc: Doc<A>): Doc<A> = width(doc) { curWidth ->
    if (curWidth > atLeastWidth) {
        nest(atLeastWidth, line_)
    } else {
        spaces(atLeastWidth - curWidth)
    }
}

/**
 * Insert a number of spaces. Negative values count as 0.
 */
fun spaces(n: Int): DocNo = when {
    n <= 0 -> Doc.Empty
    n == 1 -> Doc.space
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
 * Repeat an arbitrary string.
 *
 * Port note: See notes on [char]; this also provides the functionality of repeating an abritrary
 * codepoint that's missing from charTimes.
 */
fun textTimes(s: CharSequence, n: Int): DocNo = text(repeatText(s, n))

/**
 * `plural n one many` is `one` if `n` is `1`, and `many` otherwise. A
 * typical use case is  adding a plural "s".
 *
 * ```
 * >>> let things = [True]
 * >>> let amount = length things
 * >>> pretty things <+> "has" <+> pretty amount <+> plural "entry" "entries" amount
 * [True] has 1 entry
 * ```
 */
fun <X> plural(one: X, multiple: X, num: Int): X = when (num) {
    1 -> one
    else -> multiple
}

/**
 * `enclose l r x` encloses document `x` between documents `l` and `r`
 * using `'<>'`.
 *
 *     >>> enclose "A" "Z" "??"
 *     A??Z
 *
 * ```
 * [enclose] l r x = l '<>' x '<>' r
 * ```
 */
fun <A> enclose(left: Doc<A>, right: Doc<A>, doc: Doc<A>): Doc<A> = left cat doc cat right

/**
 * `([surround] x l r)` surrounds document `x` with `l` and `r`.
 *
 *     >>> surround "??" "A" "Z"
 *     A??Z
 *
 * This is merely an argument reordering of `[enclose]`, but allows for
 * definitions like
 *
 *     >>> concatWith (surround dot) ["Prettyprinter", "Render", "Text"]
 *     Prettyprinter.Render.Text
 */
fun <A> surround(doc: Doc<A>, left: Doc<A>, right: Doc<A>): Doc<A> = left cat doc cat right

/**
 * Add an annotation to a `[Doc]`. This annotation can then be used by the
 * renderer to e.g. add color to certain parts of the output. For a full
 * tutorial example on how to use it, see the
 * "Prettyprinter.Render.Tutorials.StackMachineTutorial" or
 * "Prettyprinter.Render.Tutorials.TreeRenderingTutorial" modules.
 *
 * This function is only relevant for custom formats with their own annotations,
 * and not relevant for basic prettyprinting. The predefined renderers, e.g.
 * "Prettyprinter.Render.Text", should be enough for the most common
 * needs.
 */
fun <A> annotate(ann: A, doc: Doc<A>): Doc<A> = Doc.Annotated(ann, doc)

/**
 * Remove all annotations.
 *
 * Although [unAnnotate] is idempotent with respect to rendering,
 *
 * ```
 * [unAnnotate] . [unAnnotate] = [unAnnotate]
 * ```
 *
 * it should not be used without caution, for each invocation traverses the
 * entire contained document. If possible, it is preferrable to unannotate after
 * producing the layout by using [SDS.unAnnotate].
 */
fun <A> unAnnotate(doc: Doc<A>): DocNo = alterAnnotations(doc) { listOf() }

/**
 * Change the annotation of a [Doc]ument.
 *
 * Useful in particular to embed documents with one form of annotation in a more
 * generally annotated document.
 *
 * Since this traverses the entire `[Doc]` tree, including parts that are not
 * rendered due to other layouts fitting better, it is preferrable to reannotate
 * after producing the layout by using `[SDS.reAnnotate]`.
 *
 * Since `[reAnnotate]` has the right type and satisfies `[reAnnotate] id = id`,
 * it is used to define the `Functor` instance of `[Doc]`.
 */
fun <A, B> reAnnotate(doc: Doc<A>, alter: (A) -> B): Doc<B> = alterAnnotations(doc) { listOf(alter(it)) }

/**
 * Change the annotations of a [Doc]ument. Individual annotations can be
 * removed, changed, or replaced by multiple ones.
 *
 * This is a general function that combines [unAnnotate] and [reAnnotate], and
 * it is useful for mapping semantic annotations (such as ??this is a keyword??)
 * to display annotations (such as ??this is red and underlined??), because some
 * backends may not care about certain annotations, while others may.
 *
 * Annotations earlier in the new list will be applied earlier, i.e. returning
 * `[Bold, Green]` will result in a bold document that contains green text, and
 * not vice-versa.
 *
 * Since this traverses the entire `[Doc]` tree, including parts that are not
 * rendered due to other layouts fitting better, it is preferrable to reannotate
 * after producing the layout by using `[SDS.alterAnnotations]`.
 */
@Suppress("UNCHECKED_CAST")
fun <A, B> alterAnnotations(doc: Doc<A>, re: (A) -> Iterable<B>): Doc<B> {
    fun go(d: Doc<A>): Doc<B> = when (d) {
        Doc.Empty, Doc.Fail, Doc.Line, is Doc.Char, is Doc.Text -> d as Doc<B>
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
 * The data type [SimpleDocStream] represents laid out documents and is used
 * by the display functions.
 *
 * A simplified view is that `[Doc] = [List]<[SimpleDocStream]>`, and the layout
 * functions pick one of the [SimpleDocStream]s based on which one fits the
 * layout constraints best. This means that [SimpleDocStream] has all complexity
 * contained in [Doc] resolved, making it very easy to convert it to other
 * formats, such as plain text or terminal output.
 *
 * To write your own `[Doc]` to X converter, it is therefore sufficient to
 * convert from `[SimpleDocStream]`. The ??Render?? submodules provide some
 * built-in converters to do so, and helpers to create own ones.
 */
sealed class SDS<out A> {
    internal object SFail : SDS<Nothing>()
    internal object SEmpty : SDS<Nothing>()
    internal class SChar<A>(val text: CharSequence, rest: () -> SDS<A>) : SDS<A>() {
        constructor(char: Char, rest: () -> SDS<A>) : this(char.toString(), rest)

        val rest: SDS<A> by lazy(rest)
        val length: Int = countCodepoints(text)
    }

    internal class SText<A>(val text: CharSequence, rest: () -> SDS<A>) : SDS<A>() {
        val rest: SDS<A> by lazy(rest)
        val length: Int = countCodepoints(text)
    }

    internal class SLine<A>(val indent: Int, rest: () -> SDS<A>) : SDS<A>() {
        val rest: SDS<A> by lazy(rest)
    }

    internal class SAnnPush<A>(val ann: A, rest: () -> SDS<A>) : SDS<A>() {
        val rest: SDS<A> by lazy(rest)
    }

    internal class SAnnPop<A>(rest: () -> SDS<A>) : SDS<A>() {
        val rest: SDS<A> by lazy(rest)
    }

    /**
     * Remove all annotations. [unAnnotate] for [SimpleDocStream].
     */
    fun unAnnotate(): SDS<Nothing> {
        @Suppress("NON_TAIL_RECURSIVE_CALL")
        tailrec fun go(sds: SDS<A>): SDS<Nothing> = when (sds) {
            SEmpty -> SEmpty
            SFail -> SFail
            is SChar -> SChar(sds.text) { go(sds.rest) }
            is SText -> SText(sds.text) { go(sds.rest) }
            is SLine -> SLine(sds.indent) { go(sds.rest) }
            is SAnnPop -> go(sds.rest)
            is SAnnPush -> go(sds.rest)
        }
        return go(this)
    }

    /**
     * Change the annotation of a document. [reAnnotate] for [SimpleDocStream].
     */
    fun <B> reAnnotate(func: (A) -> B): SDS<B> {
        fun go(sds: SDS<A>): SDS<B> = when (sds) {
            SEmpty -> SEmpty
            SFail -> SFail
            is SChar -> SChar(sds.text) { go(sds.rest) }
            is SText -> SText(sds.text) { go(sds.rest) }
            is SLine -> SLine(sds.indent) { go(sds.rest) }
            is SAnnPop -> SAnnPop { go(sds.rest) }
            is SAnnPush -> SAnnPush(func(sds.ann)) { go(sds.rest) }
        }
        return go(this)
    }

    private enum class Removal { Remove, DontRemove }

    /**
     * Change the annotation of a document to a different annotation, or none at
     * all. [alterAnnotations] for [SimpleDocStream].
     *
     * Note that the [Doc] version is more flexible, since it allows changing a
     * single annotation to multiple ones.
     * ('Prettyprinter.Render.Util.SimpleDocTree.SimpleDocTree' restores
     * this flexibility again.)
     */
    fun <B> alterAnnotations(re: (A) -> B?): SDS<B> {
        fun go(stack: CList<Removal>, sds: SDS<A>): SDS<B> = when (sds) {
            SFail -> SFail
            SEmpty -> SEmpty
            is SChar -> SChar(sds.text) { go(stack, sds.rest) }
            is SText -> SText(sds.text) { go(stack, sds.rest) }
            is SLine -> SLine(sds.indent) { go(stack, sds.rest) }
            is SAnnPush -> when (val a = re(sds.ann)) {
                null -> go(CList.Cons(Removal.Remove, stack), sds.rest)
                else -> SAnnPush(a) { go(CList.Cons(Removal.DontRemove, stack), sds.rest) }
            }
            is SAnnPop -> when (stack) {
                CList.Nil -> panicPeekedEmpty()
                is CList.Cons -> when (stack.head) {
                    Removal.DontRemove -> SAnnPop { go(stack.tail, sds.rest) }
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
        fun prependEmptyLines(ix: CList<Int>, sds0: SDS<A>): SDS<A> = foldr(sds0, ix) { _, sds -> SLine(0) { sds } }

        fun commitWhitespace(withheldLines: CList<Int>, withheldSpaces: Int, sds: SDS<A>): SDS<A> =
            when (withheldLines) {
                CList.Nil -> when (withheldSpaces) {
                    0 -> sds
                    1 -> SChar(' ') { sds }
                    else -> SText(repeatChar(' ', withheldSpaces)) { sds }
                }
                is CList.Cons -> prependEmptyLines(withheldLines.tail, SLine(
                    withheldLines.head + withheldSpaces
                ) { sds })
            }

        fun go(state: Wss, sds: SDS<A>): SDS<A> = when (state) {
            is Wss.AnnotationLevel -> when (sds) {
                SFail -> SFail
                SEmpty -> SEmpty
                is SChar -> SChar(sds.text) { go(state, sds.rest) }
                is SText -> SText(sds.text) { go(state, sds.rest) }
                is SLine -> SLine(sds.indent) { go(state, sds.rest) }
                is SAnnPush -> SAnnPush(sds.ann) { go(state.plusOne(), sds.rest) }
                is SAnnPop -> when {
                    state.annLvl > 1 -> SAnnPop { go(state.plusOne(), sds.rest) }
                    else -> SAnnPop { go(Wss.RecordedWhitespace(CList.Nil, 0), sds.rest) }
                }
            }
            is Wss.RecordedWhitespace -> when (sds) {
                SFail -> SFail
                SEmpty -> prependEmptyLines(state.withheldLines, SEmpty)
                is SChar -> when (sds.text) {
                    " " -> go(Wss.RecordedWhitespace(state.withheldLines, state.withheldSpaces + 1), sds.rest)
                    else -> commitWhitespace(
                        state.withheldLines,
                        state.withheldSpaces,
                        SChar(sds.text) { go(Wss.RecordedWhitespace(CList.Nil, 0), sds.rest) }
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
                                stripped
                            ) {
                                go(
                                    Wss.RecordedWhitespace(CList.Nil, trailingLength),
                                    sds.rest
                                )
                            }
                        )
                    }
                }
                is SLine -> go(Wss.RecordedWhitespace(CList.Cons(sds.indent, state.withheldLines), 0), sds.rest)
                is SAnnPush -> commitWhitespace(
                    state.withheldLines, state.withheldSpaces,
                    SAnnPush(sds.ann) { go(Wss.AnnotationLevel(1), sds.rest) }
                )
                is SAnnPop -> panicSkippingInUnannotated()
            }
        }
        return go(Wss.RecordedWhitespace(CList.Nil, 0), this)
    }

    /**
     * Alter the document???s annotations.
     *
     * This instance makes [SimpleDocStream] more flexible (because it can be used in
     * 'Functor'-polymorphic values), but `'fmap'` is much less readable compared to
     * using `[SDS.reAnnotate]` in code that only works for `[SimpleDocStream]` anyway.
     * Consider using the latter when the type does not matter.
     *
     * Port note: instance Functor
     */
    fun <B> fmap(func: (A) -> B): SDS<B> = reAnnotate(func)

    /*
     * TODO: foldMap and traverse for instances of Foldable and Traversable
     */
}

typealias SimpleDocStream<A> = SDS<A>

/**
 * Decide whether a [SimpleDocStream] fits the constraints given, namely
 *
 *   - original indentation of the current line
 *   - current column
 *   - initial indentation of the alternative [SimpleDocStream] if it
 *     starts with a line break (used by [layoutSmart])
 *   - width in which to fit the first line
 *
 * Port note: `(Int, Int, Int?, SimpleDocStream<A>) -> Boolean`
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
     *
     * * `avail` is the number of characters, including whitespace, that fit in a line. A typical value is 80.
     * * `ribbon` is the ribbon with, i.e. the fraction of the total page width that can be printed on. This allows
     *   limiting the length of printable text per line. Values must be between 0 and 1, and 0.4 to 1 is typical.
     */
    class AvailablePerLine(val lineLength: Int, val ribbonFraction: Double) : PageWidth() {
        init {
            require(lineLength >= 0) { "chars available per line must be positive" }
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
data class Opts(val layoutPageWidth: PageWidth) {
    companion object {
        /**
         * The default layout options, suitable when you just want some output, and
         * don???t particularly care about the details. Used by the 'Show' instance, for
         * example.
         */
        val default = Opts(PageWidth.default)
        /** Selecting unbounded options. */
        val unbounded = Opts(PageWidth.Unbounded)
        /** Selecting a specific line length and ribbon. */
        fun std(lineLength: Int, ribbonFraction: Double = 1.0) = Opts(PageWidth.AvailablePerLine(lineLength, ribbonFraction))
    }
}

typealias LayoutOptions = Opts


/**
 * This is the default layout algorithm, and it is used by `show`, [putDoc]
 * and `hPutDoc`. (Port note: see [AppendableSink])
 *
 * `[layoutPretty]` commits to rendering something in a certain way if the next
 * element fits the layout constraints; in other words, it has one
 * [SimpleDocStream] element lookahead when rendering. Consider using the
 * smarter, but a bit less performant, `[layoutSmart]` algorithm if the results
 * seem to run off to the right before having lots of line breaks.
 */
fun <A> layoutPretty(opts: Opts, doc: Doc<A>): SDS<A> {
    tailrec fun fits(width: Int, sds: SDS<A>): Boolean = if (width < 0) {
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
            layoutWadlerLeijen(pw, doc) { lineIndent, currentColumn, _, sdoc ->
                fits(remainingWidth(pw.lineLength, pw.ribbonFraction, lineIndent, currentColumn), sdoc)
            }
        is PageWidth.Unbounded ->
            layoutUnbounded(doc)
    }
}

/**
 * A layout algorithm with more lookahead than [layoutPretty], that introduces
 * line breaks earlier if the content does not (or will not, rather) fit into
 * one line.
 *
 * Consider the following python-ish document,
 *
 *     >>> let fun x = hang 2 ("fun(" <> softline' <> x) <> ")"
 *     >>> let doc = (fun . fun . fun . fun . fun) (align (list ["abcdef", "ghijklm"]))
 *
 * which we???ll be rendering using the following pipeline (where the layout
 * algorithm has been left open):
 *
 *     >>> import Data.Text.IO as T
 *     >>> import Prettyprinter.Render.Text
 *     >>> let hr = pipe <> pretty (replicate (26-2) '-') <> pipe
 *     >>> let go layouter x = (T.putStrLn . renderStrict . layouter (LayoutOptions (AvailablePerLine 26 1))) (vsep [hr, x, hr])
 *
 * If we render this using [layoutPretty] with a page width of 26 characters
 * per line, all the `fun` calls fit into the first line so they will be put
 * there:
 *
 *     >>> go layoutPretty doc
 *     | * * * * * * * * * * * *|
 *     fun(fun(fun(fun(fun(
 *                       [ abcdef
 *                       , ghijklm ])))))
 *     | * * * * * * * * * * * *|
 *
 * Note that this exceeds the desired 26 character page width. The same
 * document, rendered with `[layoutSmart]`, fits the layout contstraints:
 *
 *     >>> go layoutSmart doc
 *     | * * * * * * * * * * * *|
 *     fun(
 *       fun(
 *         fun(
 *           fun(
 *             fun(
 *               [ abcdef
 *               , ghijklm ])))))
 *     | * * * * * * * * * * * *|
 *
 * The key difference between [layoutPretty] and [layoutSmart] is that the
 * latter will check the potential document until it encounters a line with the
 * same indentation or less than the start of the document. Any line encountered
 * earlier is assumed to belong to the same syntactic structure.
 * [layoutPretty] checks only the first line.
 *
 * Consider for example the question of whether the `A`s fit into the document
 * below:
 *
 *     > 1 A
 *     > 2   A
 *     > 3  A
 *     > 4 B
 *     > 5   B
 *
 * [layoutPretty] will check only line 1, ignoring whether e.g. line 2 might
 * already be too wide.
 * By contrast, [layoutSmart] stops only once it reaches line 4, where the `B`
 * has the same indentation as the first `A`.
 */
fun <A> layoutSmart(opts: Opts, doc: Doc<A>): SDS<A> = when (val pw = opts.layoutPageWidth) {
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
 * Layout a document with `Unbounded` page width.
 * See the Note [Detecting failure with Unbounded page width].
 */
fun <A> layoutUnbounded(doc: Doc<A>): SDS<A> {
    fun failsOnFirstLine(sds: SDS<A>): Boolean {
        tailrec fun go(sds: SDS<A>): Boolean = when (sds) {
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
    tailrec fun initialIndentation(sds: SDS<A>): Int? = when (sds) {
        is SDS.SLine -> sds.indent
        is SDS.SAnnPush -> initialIndentation(sds.rest)
        is SDS.SAnnPop -> initialIndentation(sds.rest)
        else -> null
    }

    /**
     * Select the better fitting of two documents:
     * Choice A if it fits, otherwise choice B.
     *
     * The fit of choice B is /not/ checked! It is ultimately the user's
     * responsibility to provide an alternative that can fit the page even when
     * choice A doesn't.
     */
    fun selectNicer(lineIndent: Int, currentColumn: Int, x: SDS<A>, y: SDS<A>): SDS<A> =
        if (pred.test(lineIndent, currentColumn, initialIndentation(y), x)) {
            x
        } else {
            y
        }

    /**
     * * current column >= current nesting level`
     * * current column - current indentation = number of chars inserted in line
    best
     *
     * * nl: Current nesting level
     * * cc: Current column, i.e. "where the cursor is"
     * * lpl: Documents remaining to be handled (in order)
     */
    @Suppress("NON_TAIL_RECURSIVE_CALL")
    tailrec fun best(nl: Int, cc: Int, lpl: Lpl<A>): SDS<A> = when (lpl) {
        Lpl.Nil -> SDS.SEmpty
        is Lpl.UndoAnn -> SDS.SAnnPop { best(nl, cc, lpl.ds) }
        is Lpl.Cons -> when (val d = lpl.d) {
            Doc.Fail -> SDS.SFail
            Doc.Empty -> best(nl, cc, lpl.ds)
            is Doc.Char -> SDS.SChar(d.text) { best(nl, cc + d.length, lpl.ds) }
            is Doc.Text -> SDS.SText(d.text) { best(nl, cc + d.length, lpl.ds) }
            Doc.Line -> {
                val x = best(lpl.i, lpl.i, lpl.ds)
                val iPrime = when (x) {
                    SDS.SEmpty -> 0
                    is SDS.SLine -> 0
                    else -> lpl.i
                }
                SDS.SLine(iPrime) { x }
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
            is Doc.Annotated -> SDS.SAnnPush(d.ann) { best(nl, cc, Lpl.Cons(lpl.i, d.doc, Lpl.UndoAnn(lpl.ds))) }
        }
    }

    return best(0, 0, Lpl.Cons(0, doc, Lpl.Nil))
}

/**
 * `([layoutCompact] x)` lays out the document `x` without adding any
 * indentation and without preserving annotations.
 * Since no _pretty_ printing is involved, this layouter is very
 * fast. The resulting output contains fewer characters than a prettyprinted
 * version and can be used for output that is read by other programs.
 *
 *     >>> let doc = [hang] 4 ([vsep] ["lorem", "ipsum", hang 4 (vsep ["dolor", "sit"])])
 *     >>> doc
 *     lorem
 *         ipsum
 *         dolor
 *             sit
 *
 *     >>> let putDocCompact = renderIO System.IO.stdout . layoutCompact
 *     >>> putDocCompact doc
 *     lorem
 *     ipsum
 *     dolor
 *     sit
 */
fun <A> layoutCompact(doc: Doc<A>): SDS<A> {
    @Suppress("NON_TAIL_RECURSIVE_CALL")
    tailrec fun scan(col: Int, docs: CList<Doc<A>>): SDS<A> = when (docs) {
        is CList.Nil -> SDS.SEmpty
        is CList.Cons -> when (val d = docs.head) {
            Doc.Fail -> SDS.SFail
            Doc.Empty -> scan(col, docs.tail)
            is Doc.Char -> SDS.SChar(d.text) { scan(col + d.length, docs.tail) }
            is Doc.Text -> SDS.SText(d.text) { scan(col + d.length, docs.tail) }
            is Doc.FlatAlt -> scan(col, CList.Cons(d.first, docs.tail))
            Doc.Line -> SDS.SLine(0) { scan(0, docs.tail) }
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

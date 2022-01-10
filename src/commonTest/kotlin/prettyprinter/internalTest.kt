package prettyprinter

import prettyprinter.symbols.brackets
import prettyprinter.symbols.comma
import prettyprinter.symbols.dot
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Convenience function to keep whitespace clear.
 */
fun ll(vararg lines: String): String {
    return lines.joinToString("\n")
}

/**
 * Any test labeled Dt is a doctest, find the associated function and the string >>> in the comments for the original
 * test and commentary.
 */
@Suppress("SpellCheckingInspection")
class InternalTests {
    @Test
    fun nestDt1() {
        val actual: Doc<Nothing> = vsep(
            listOf(
                nest(4, vsep(texts("lorem", "ipsum", "dolor"))),
                text("sit"),
                text("amet")
            )
        )
        val expected = ll(
            "lorem",
            "    ipsum",
            "    dolor",
            "sit",
            "amet"
        )
        assertEquals(expected, actual.toStringPretty())
    }

    @Test
    fun lineDt1() {
        val actual = text("lorem ipsum") cat line cat text("dolor sit amet")
        val expected = ll(
            "lorem ipsum",
            "dolor sit amet"
        )
        assertEquals(expected, actual.toStringPretty())
    }

    @Test
    fun lineDt2() {
        val actual = group(text("lorem ipsum") cat line cat text("dolor sit amet"))
        val expected = ll(
            "lorem ipsum dolor sit amet"
        )
        assertEquals(expected, actual.toStringPretty())
    }

    @Test
    fun lineBarDt1() {
        val actual = text("lorem ipsum") cat line_ cat text("dolor sit amet")
        val expected = ll(
            "lorem ipsum",
            "dolor sit amet"
        )
        assertEquals(expected, actual.toStringPretty())
    }

    @Test
    fun lineBarDt2() {
        val actual = group(text("lorem ipsum") cat line_ cat text("dolor sit amet"))
        val expected = ll(
            "lorem ipsumdolor sit amet"
        )
        assertEquals(expected, actual.toStringPretty())
    }

    @Test
    fun softLineDt1() {
        val actual = text("lorem ipsum") cat softline cat text("dolor sit amet")
        val expected = ll(
            "lorem ipsum dolor sit amet"
        )
        assertEquals(expected, actual.toStringPretty(80))
    }

    @Test
    fun softLineDt2() {
        val actual = text("lorem ipsum") cat softline cat text("dolor sit amet")
        val expected = ll(
            "lorem ipsum",
            "dolor sit amet"
        )
        assertEquals(expected, actual.toStringPretty(10))
    }

    @Test
    fun softLineBarDt1() {
        val actual = text("ThisWord") cat softline_ cat text("IsWayTooLong")
        val expected = ll(
            "ThisWordIsWayTooLong"
        )
        assertEquals(expected, actual.toStringPretty(80))
    }

    @Test
    fun softLineBarDt2() {
        val actual = text("ThisWord") cat softline_ cat text("IsWayTooLong")
        val expected = ll(
            "ThisWord",
            "IsWayTooLong"
        )
        assertEquals(expected, actual.toStringPretty(10))
    }

    @Test
    fun hardlineDt1() {
        val actual = text("lorem ipsum") cat hardline cat text("dolor sit amet")
        val expected = ll(
            "lorem ipsum",
            "dolor sit amet"
        )
        assertEquals(expected, actual.toStringPretty(1_000))
    }

    @Test
    fun hardlineDt2() {
        val actual = group(text("lorem ipsum") cat hardline cat text("dolor sit amet"))
        val expected = ll(
            "lorem ipsum",
            "dolor sit amet"
        )
        assertEquals(expected, actual.toStringPretty())
    }

    @Test
    fun flatAltDt1() {
        val doc = flatAlt(text("a"), text("b"))
        assertEquals(ll("a"), doc.toStringPretty())
        assertEquals(ll("b"), group(doc).toStringPretty())
        assertEquals(ll("a"), group(doc).toStringPretty(0))
    }

    @Test
    fun flatAltDt2() {
        val hello = text("Hello") cat softline cat text("world!")
        assertEquals(ll("Hello world!"), hello.toStringPretty(12))
        assertEquals(ll("Hello", "world!"), hello.toStringPretty(11))
    }

    @Test
    fun flatAltDt3() {
        val open = flatAlt(text(""), text("{ "))
        val close = flatAlt(text(""), text(" }"))
        val separator = flatAlt(text(""), text("; "))
        fun prettyDo(xs: Iterable<DocNo>) = group(text("do") spc align(encloseSep(open, close, separator, xs)))
        val statements = listOf(
            text("name:_ <- getArgs"),
            text("let greet = \"Hello, \" <> name"),
            text("putStrLn greet")
        )

        assertEquals(
            ll("do { name:_ <- getArgs; let greet = \"Hello, \" <> name; putStrLn greet }"),
            prettyDo(statements).toStringPretty(80)
        )
        assertEquals(
            ll(
                "do name:_ <- getArgs",
                "   let greet = \"Hello, \" <> name",
                "   putStrLn greet"
            ),
            prettyDo(statements).toStringPretty(10)
        )
    }

    @Test
    fun flatAltDt4() {
        val ugly = group(flatAlt(text("even wider"), text("too wide")))
        assertEquals(ll("even wider"), ugly.toStringPretty(7))
    }

    @Test
    fun flatAltDt5() {
        assertEquals(ll("y y"), group(flatAlt(text("x"), text("y") cat line cat text("y"))).toStringPretty())
    }

    @Test
    fun flatAltDt6() {
        assertEquals(ll("x"), group(flatAlt(text("x"), text("y") cat hardline cat text("y"))).toStringPretty())
    }

    @Test
    fun concatWithDt1() {
        val actual = concatWith(listOf(text("Prettyprinter"), text("Render"), text("Text"))) { x, y ->
            surround(dot, x, y)
        }
        assertEquals(ll("Prettyprinter.Render.Text"), actual.toStringPretty())
    }

    @Test
    fun hsepDt1() {
        val docs = words("lorem ipsum dolor sit amet")
        assertEquals(ll("lorem ipsum dolor sit amet"), hsep(docs).toStringPretty())
        assertEquals(ll("lorem ipsum dolor sit amet"), hsep(docs).toStringPretty(5))
    }

    @Test
    fun vsepDt1() {
        val actual = text("prefix") spc vsep(words("text to lay out"))
        assertEquals(ll("prefix text", "to", "lay", "out"), actual.toStringPretty())
    }

    @Test
    fun vsepDt2() {
        val actual = text("prefix") spc align(vsep(words("text to lay out")))
        assertEquals(ll("prefix text", "       to", "       lay", "       out"), actual.toStringPretty())
    }

    private fun <E> List<E>.takeNCycle(take: Int): List<E> = List(take) { this[it % this.size] }

    @Test
    fun fillSepDt1() {
        val docs = words("lorem ipsum dolor sit amet").toList().takeNCycle(20)
        val actual = text("Docs:") spc fillSep(docs)

        assertEquals(
            ll(
                "Docs: lorem ipsum dolor sit amet lorem ipsum dolor sit amet lorem ipsum dolor",
                "sit amet lorem ipsum dolor sit amet"
            ), actual.toStringPretty(80)
        )

        assertEquals(
            ll(
                "Docs: lorem ipsum dolor sit amet lorem",
                "ipsum dolor sit amet lorem ipsum dolor",
                "sit amet lorem ipsum dolor sit amet"
            ), actual.toStringPretty(40)
        )
    }

    @Test
    fun sepDt1() {
        val doc = text("prefix") spc sep(words("text to lay out"))
        assertEquals(
            ll("prefix text to lay out"),
            doc.toStringPretty(80)
        )
        assertEquals(
            ll("prefix text", "to", "lay", "out"),
            doc.toStringPretty(20)
        )
    }

    @Test
    fun hcatDt1() {
        val docs = words("lorem ipsum dolor")
        assertEquals(ll("loremipsumdolor"), hcat(docs).toStringPretty())
    }

    @Test
    fun vcatDt1() {
        val docs = words("lorem ipsum dolor")
        assertEquals(
            ll("lorem", "ipsum", "dolor"),
            vcat(docs).toStringPretty()
        )
        assertEquals(
            ll("loremipsumdolor"),
            group(vcat(docs)).toStringPretty()
        )
    }

    @Test
    fun fillCatDt1() {
        val docs = texts("lorem", "ipsum", "dolor", "sit", "amet").takeNCycle(20)
        assertEquals(
            ll(
                "Grouped: lorem ipsum dolor sit amet",
                "lorem ipsum dolor sit amet lorem ipsum",
                "dolor sit amet lorem ipsum dolor sit",
                "amet"
            ), (text("Grouped:") spc group(fillSep(docs))).toStringPretty(40)
        )

        assertEquals(
            ll(
                "Grouped: loremipsumdolorsitametlorem",
                "ipsumdolorsitametloremipsumdolorsitamet",
                "loremipsumdolorsitamet"
            ), (text("Grouped:") spc group(fillCat(docs))).toStringPretty(40)
        )
    }

    @Test
    fun catDt1() {
        val docs = words("lorem ipsum dolor")
        assertEquals(
            ll("Docs: loremipsumdolor"),
            (text("Docs:") spc cat(docs)).toStringPretty(80)
        )

        assertEquals(
            ll("Docs: lorem", "ipsum", "dolor"),
            (text("Docs:") spc cat(docs)).toStringPretty(10)
        )
    }

    @Test
    fun punctuateDt1() {
        val docs = punctuate(comma, words("lorem ipsum dolor sit amet"))
        assertEquals(
            ll("lorem, ipsum, dolor, sit, amet"),
            hsep(docs).toStringPretty(80)
        )
        assertEquals(
            ll("lorem,", "ipsum,", "dolor,", "sit,", "amet"),
            vsep(docs).toStringPretty(80)
        )
    }

    @Test
    fun columnDt1() {
        assertEquals(
            ll("Columns are 0-based."),
            column { l -> text("Columns are") spc pretty(l) cat text("-based.") }.toStringPretty()
        )

        val doc = text("prefix") spc column { l -> text("| <- column") spc pretty(l) }
        val actual = vsep(listOf(0, 4, 8).map { indent(it, doc) })
        assertEquals(
            ll(
                "prefix | <- column 7",
                "    prefix | <- column 11",
                "        prefix | <- column 15"
            ), actual.toStringPretty()
        )
    }

    @Test
    fun nestingDt1() {
        val doc = text("prefix") spc nesting { l -> brackets(text("Nested:") spc pretty(l)) }
        val actual = vsep(listOf(0, 4, 8).map { indent(it, doc) })
        assertEquals(
            ll(
                "prefix [Nested: 0]",
                "    prefix [Nested: 4]",
                "        prefix [Nested: 8]"
            ), actual.toStringPretty()
        )
    }

    @Test
    fun widthDt1() {
        fun <A> annotate(doc: Doc<A>) = width(brackets(doc)) { w -> text(" <- width:") spc pretty(w) }
        val actual = align(
            vsep(
                listOf(
                    text(" *-"),
                    text(" * * *"),
                    indent(3, text(" *-")),
                    vsep(
                        listOf(
                            text(" *-"),
                            indent(4, text(" *-"))
                        )
                    )
                ).map(::annotate)
            )
        )
        assertEquals(
            ll(
                "[ *-] <- width: 5",
                "[ * * *] <- width: 8",
                "[    *-] <- width: 8",
                "[ *-",
                "     *-] <- width: 8"
            ), actual.toStringPretty()
        )
    }
}

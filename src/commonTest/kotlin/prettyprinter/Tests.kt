package prettyprinter

import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Avoids using trim so we're not wondering if things don't line up.
 */
fun ll(vararg lines: String): String {
    return lines.joinToString("\n")
}

/**
 * Any test labeled Dt is a doctest, find the associated function and the string >>> in the comments for the original
 * test and commentary.
 */
@Suppress("SpellCheckingInspection")
class Tests {
    @Test
    fun nestDt1() {
        val actual: Doc<Nothing> = vsep(
            listOf(
                nest(4, vsep(listOf(text("lorem"), text("ipsum"), text("dolor")))),
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
        assertEquals(expected, actual.toString())
    }

    @Test
    fun lineDt1() {
        val actual = text("lorem ipsum") + line + text("dolor sit amet")
        val expected = ll(
            "lorem ipsum",
            "dolor sit amet"
        )
        assertEquals(expected, actual.toString())
    }
    @Test
    fun lineDt2() {
        val actual = group(text("lorem ipsum") + line + text("dolor sit amet"))
        val expected = ll(
            "lorem ipsum dolor sit amet"
        )
        assertEquals(expected, actual.toString())
    }

    @Test
    fun lineBarDt1() {
        val actual = text("lorem ipsum") + line_ + text("dolor sit amet")
        val expected = ll(
            "lorem ipsum",
            "dolor sit amet"
        )
        assertEquals(expected, actual.toString())
    }
    @Test
    fun lineBarDt2() {
        val actual = group(text("lorem ipsum") + line_ + text("dolor sit amet"))
        val expected = ll(
            "lorem ipsumdolor sit amet"
        )
        assertEquals(expected, actual.toString())
    }

    @Test
    fun softLineDt1() {
        val actual = text("lorem ipsum") + softline + text("dolor sit amet")
        val expected = ll(
            "lorem ipsum dolor sit amet"
        )
        assertEquals(expected, putDocW(80, actual))
    }

    @Test
    fun softLineDt2() {
        val actual = text("lorem ipsum") + softline + text("dolor sit amet")
        val expected = ll(
            "lorem ipsum",
            "dolor sit amet"
        )
        assertEquals(expected, putDocW(10, actual))
    }

    @Test
    fun softLineBarDt1() {
        val actual = text("ThisWord") + softline_ + text("IsWayTooLong")
        val expected = ll(
            "ThisWordIsWayTooLong"
        )
        assertEquals(expected, putDocW(80, actual))
    }

    @Test
    fun softLineBarDt2() {
        val actual = text("ThisWord") + softline_ + text("IsWayTooLong")
        val expected = ll(
            "ThisWord",
            "IsWayTooLong"
        )
        assertEquals(expected, putDocW(10, actual))
    }

    @Test
    fun hardlineDt1() {
        val actual = text("lorem ipsum") + hardline + text("dolor sit amet")
        val expected = ll(
            "lorem ipsum",
            "dolor sit amet"
        )
        assertEquals(expected, putDocW(1_000, actual))
    }
    @Test
    fun hardlineDt2() {
        val actual = group(text("lorem ipsum") + hardline + text("dolor sit amet"))
        val expected = ll(
            "lorem ipsum",
            "dolor sit amet"
        )
        assertEquals(expected, actual.toString())
    }
}
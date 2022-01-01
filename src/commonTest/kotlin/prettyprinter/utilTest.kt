@file:Suppress("SpellCheckingInspection")

package prettyprinter

import kotlin.test.Test
import kotlin.test.assertEquals

class UtilTests {
    @Test
    fun wordsDt1() {
        val actual = tupled(words("Lorem ipsum dolor"))
        assertEquals(ll("(Lorem, ipsum, dolor)"), actual.toStringPretty())
    }

    @Test
    fun texts() {
        val actual = tupled(texts("Lorem ipsum", "dolor"))
        assertEquals(ll("(Lorem ipsum, dolor)"), actual.toString())
    }

    @Test
    fun reflowDt1() {
        val actual = reflow(
            "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod " +
                    "tempor incididunt ut labore et dolore magna aliqua."
        )
        assertEquals(
            ll(
                "Lorem ipsum dolor sit amet,",
                "consectetur adipisicing elit,",
                "sed do eiusmod tempor incididunt",
                "ut labore et dolore magna",
                "aliqua."
            ), actual.toStringPretty(32)
        )
    }

    @Test
    fun repeatChar1() {
        val subj = repeatChar('x', 3)
        val expect = "xxx"
        assertEquals(expect, subj.toString())
    }

    @Test
    fun repeatChar2() {
        val subj = repeatChar('x', 13).subSequence(4, 9)
        val expect = "xxxxx"
        assertEquals(expect, subj.toString())
    }

    @Test
    fun repeatText1() {
        val subj = repeatText("abc", 3)
        val expect = "abcabcabc"
        assertEquals(expect, subj.toString())
    }

    @Test
    fun repeatText2() {
        val subj = repeatText("abc", 5).subSequence(7, 13)
        val expect = "bcabca"
        assertEquals(expect, subj.toString())
    }

    @Test
    fun mapWhere1() {
        assertEquals(
            listOf("one - FIRST", "two - MIDDLE", "three - MIDDLE", "four - LAST"),
            listOf("one", "two", "three", "four").mapWhere { w, e -> "$e - $w" }.toList()
        )
    }

    @Test
    fun mapWhere2() {
        assertEquals(
            listOf("one - ONLY"),
            listOf("one").mapWhere { w, e -> "$e - $w" }.toList()
        )
    }
}

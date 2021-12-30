@file:Suppress("SpellCheckingInspection")

package prettyprinter

import kotlin.test.Test
import kotlin.test.assertEquals

class UtilTests {
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
}

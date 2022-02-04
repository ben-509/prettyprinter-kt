package prettyprinter

import kotlin.test.Test
import kotlin.test.assertEquals

class ReadmeTest {
    /**
     * ```haskell
     * let prettyType = align . sep . zipWith (<+>) ("::" : repeat "->")
     *     prettySig name ty = pretty name <+> prettyType ty
     * in  prettySig "example" ["Int", "Bool", "Char", "IO ()"]
     * ```
     */

    private fun prettyType(items: List<String>): DocNo = align(sep(
        items.mapWhere { where, doc -> when(where) {
            Where.FIRST -> text("::") spc text(doc)
            else -> text("->") spc text(doc)
        } }
    ))

    private fun prettySig(name: String, vararg ty: String): DocNo {
        return text(name) spc prettyType(ty.toList())
    }

    @Test
    fun exampleWideEnough() {
        val actualDoc = prettySig("example", "Int", "Bool", "Char", "IO ()")
        val actual = actualDoc.toStringPretty(80)
        assertEquals(ll("example :: Int -> Bool -> Char -> IO ()"), actual)
    }

    @Test
    fun exampleNarrow() {
        val actualDoc = prettySig("example", "Int", "Bool", "Char", "IO ()")
        val actual = actualDoc.toStringPretty(20)
        assertEquals(ll(
            "example :: Int",
            "        -> Bool",
            "        -> Char",
            "        -> IO ()"
        ), actual)
    }
}

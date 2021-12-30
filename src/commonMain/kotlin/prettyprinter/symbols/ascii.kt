@file:Suppress("unused", "SpellCheckingInspection")

package prettyprinter.symbols

/*
 * Ported from [Ascii.hs](https://hackage.haskell.org/package/prettyprinter-1.7.1/src/src/Prettyprinter/Symbols/Ascii.hs)
 * Internal functions should not use any `val`s defined here, instead define and use them from the Doc companion object.
 */

import prettyprinter.Doc
import prettyprinter.Doc.Char
import prettyprinter.DocNo
import prettyprinter.enclose

fun <A> sqoutes(doc: Doc<A>): Doc<A> = enclose(Doc.squote, Doc.squote, doc)
fun <A> dqoutes(doc: Doc<A>): Doc<A> = enclose(Doc.dquote, Doc.dquote, doc)
fun <A> parens(doc: Doc<A>): Doc<A> = enclose(Doc.lparen, Doc.rparen, doc)
fun <A> angles(doc: Doc<A>): Doc<A> = enclose(Doc.langle, Doc.rangle, doc)
fun <A> brackets(doc: Doc<A>): Doc<A> = enclose(Doc.lbracket, Doc.rbracket, doc)
fun <A> braces(doc: Doc<A>): Doc<A> = enclose(Doc.lbrace, Doc.rbrace, doc)

val squote: DocNo = Doc.squote
val dquote: DocNo = Doc.dquote
val lparen: DocNo = Doc.lparen
val rparen: DocNo = Doc.rparen
val langle: DocNo = Doc.langle
val rangle: DocNo = Doc.rangle
val lbracket: DocNo = Doc.lbracket
val rbracket: DocNo = Doc.rbracket
val lbrace: DocNo = Doc.lbrace
val rbrace: DocNo = Doc.rbrace
val semi: DocNo = Doc.semi
val colon: DocNo = Doc.colon
val comma: DocNo = Doc.comma
val space: DocNo = Doc.space
val dot: DocNo = Doc.dot
val slash: DocNo = Doc.slash
val backslash: DocNo = Doc.backslash
val equals: DocNo = Doc.equalsChar
val pipe: DocNo = Doc.pipe

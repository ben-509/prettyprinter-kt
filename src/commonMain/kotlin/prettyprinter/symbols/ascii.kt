@file:Suppress("unused", "SpellCheckingInspection")

/**
 * Port of https://hackage.haskell.org/package/prettyprinter-1.7.1/src/src/Prettyprinter/Symbols/Ascii.hs
 */

package prettyprinter.symbols

import prettyprinter.Doc
import prettyprinter.Doc.Char
import prettyprinter.DocNo
import prettyprinter.enclose

fun <A> sqoutes(doc: Doc<A>): Doc<A> = enclose(squote, squote, doc)
fun <A> dqoutes(doc: Doc<A>): Doc<A> = enclose(dquote, dquote, doc)
fun <A> parens(doc: Doc<A>): Doc<A> = enclose(lparen, rparen, doc)
fun <A> angles(doc: Doc<A>): Doc<A> = enclose(langle, rangle, doc)
fun <A> brackets(doc: Doc<A>): Doc<A> = enclose(lbracket, rbracket, doc)
fun <A> braces(doc: Doc<A>): Doc<A> = enclose(lbrace, rbrace, doc)

val squote: DocNo = Char('\'')
val dquote: DocNo = Char('"')
val lparen: DocNo = Char('(')
val lparenSpace: DocNo = Doc.Text("( ")
val rparen: DocNo = Char('(')
val rparenSpace: DocNo = Doc.Text(" )")
val langle: DocNo = Char('<')
val rangle: DocNo = Char('>')
val lbracket: DocNo = Char('[')
val lbracketSpace: DocNo = Doc.Text("[ ")
val rbracket: DocNo = Char(']')
val rbracketSpace: DocNo = Doc.Text(" ]")
val lbrace: DocNo = Char('{')
val lbraceSpace: DocNo = Doc.Text("{ ")
val rbrace: DocNo = Char('}')
val rbraceSpace: DocNo = Doc.Text(" }")
val semi: DocNo = Char(';')
val colon: DocNo = Char(':')
val comma: DocNo = Char(',')
val commaSpace: DocNo = Doc.Text(", ")
val space: DocNo = Char(' ')
val dot: DocNo = Char('.')
val slash: DocNo = Char('/')
val backslash: DocNo = Char('\\')
val equals: DocNo = Char('=')
val pipe: DocNo = Char('|')

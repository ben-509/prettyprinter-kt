package prettyprinter

import prettyprinter.Doc.Char

fun <A> sqoutes(doc: Doc<A>): Doc<A> = enclose(squote, squote, doc)

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

package prettyprinter

/*
 * Imperative ports of the render package.
 *
 * Right now, this is quite bare-bones, but it's enough to get a lot of tests passing.
 */

import prettyprinter.render.util.panicUncaughtFail

/**
 * Based on renderS, but modified to suit a Kotlin [Appendable].
 */
interface SimpleSink<in A> {
    fun openAnn(ann: A)
    fun closeAnn(ann: A)
    fun emit(cs: CharSequence)

    // Derived methods
    fun emitLine() = emit("\n")
    fun emitIndent(num: Int) = emit(repeatChar(' ', num))

    fun render(sds: SDS<A>) {
        val stack: ArrayDeque<A> = ArrayDeque()
        var node = sds
        while (true) {
            when (node) {
                is SDS.SAnnPop -> {
                    this.closeAnn(stack.removeLast())
                    node = node.rest
                }
                is SDS.SAnnPush -> {
                    this.openAnn(node.ann)
                    stack.addLast(node.ann)
                    node = node.rest
                }
                is SDS.SChar -> {
                    this.emit(node.text)
                    node = node.rest
                }
                SDS.SEmpty -> break
                SDS.SFail -> panicUncaughtFail()
                is SDS.SLine -> {
                    this.emitLine()
                    this.emitIndent(node.indent)
                    node = node.rest
                }
                is SDS.SText -> {
                    this.emit(node.text)
                    node = node.rest
                }
            }
        }
    }
}

open class AppendableSink<A>(protected val tgt: Appendable = StringBuilder()) : SimpleSink<A> {
    override fun openAnn(ann: A) {
    }

    override fun closeAnn(ann: A) {
    }

    override fun emit(cs: CharSequence) {
        tgt.append(cs)
    }

    /** To quickly get string output, `AppendableSink().toString(sds)` */
    fun toString(sds: SDS<A>): String {
        render(sds)
        return tgt.toString()
    }
}

class DiagnosticSink<A>(tgt: Appendable = StringBuilder()) : AppendableSink<A>(tgt) {
    override fun openAnn(ann: A) {
        this.emit("[")
        this.emit(ann.toString())
        this.emit("]")
    }

    override fun closeAnn(ann: A) {
        this.emit("[/")
        this.emit(ann.toString())
        this.emit("]")
    }
}

class HtmlAnnotation(val tag: String, val attrs: List<Pair<String, String>>)

@Suppress("unused")
class HtmlSink(tgt: Appendable) : AppendableSink<HtmlAnnotation>(tgt) {
    override fun openAnn(ann: HtmlAnnotation) {
        tgt.append('<').append(ann.tag)
        ann.attrs.forEach { (n, v) ->
            tgt.append(' ').append(n).append("=\"").append(escape(v)).append('"')
        }
        tgt.append('>')
    }

    override fun closeAnn(ann: HtmlAnnotation) {
        tgt.append("</").append(ann.tag).append('>')
    }

    override fun emit(cs: CharSequence) {
        tgt.append(escape(cs))
    }

    private fun escape(v: CharSequence): String = v.replace(htmlRe) { htmlToEscape.getOrElse(it.value) { it.value } }

    companion object {
        private val htmlRe = Regex("[<&>\"]")
        private val htmlToEscape = mapOf("<" to "&lt;", ">" to "&gt;", "&" to "&amp;", "\"" to "&quot;")
    }
}

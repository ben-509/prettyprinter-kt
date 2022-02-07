package prettyprinter

import io.kotest.property.Arb
import io.kotest.property.RandomSource
import io.kotest.property.arbitrary.alphanumeric
import io.kotest.property.arbitrary.bind
import io.kotest.property.arbitrary.flatMap
import io.kotest.property.arbitrary.int
import io.kotest.property.arbitrary.list
import io.kotest.property.arbitrary.map
import io.kotest.property.arbitrary.next
import io.kotest.property.arbitrary.string
import prettyprinter.symbols.lparen
import prettyprinter.symbols.rparen
import kotlin.math.max
import kotlin.math.min
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.time.Duration
import kotlin.time.ExperimentalTime
import kotlin.time.measureTime

data class Program(val binds: Binds)

data class Binds(val map: Map<String, LambdaForm>)

data class LambdaForm(val free: List<String>, val bound: List<String>, val body: Expr)

sealed class Expr {
    data class Let(val binds: Binds, val body: Expr): Expr()
    data class Case(val scrutinee: Expr, val alts: List<Alt>): Expr()
    data class AppF(val f: String, val args: List<String>): Expr()
    data class AppC(val c: String, val args: List<String>): Expr()
    data class AppP(val op: String, val x: String, val y: String): Expr()
    data class LitE(val lit: Int): Expr()
}

data class Alt(val con: String, val args: List<String>, val body: Expr)

/* Set up value generators. */

val keywordArb: Arb<String> = Arb.string(5..7, Arb.alphanumeric())
val keywordUcArb: Arb<String> = keywordArb.map { it.replaceFirstChar { c -> c.uppercaseChar() } }
val keywordsArb: Arb<List<String>> = Arb.list(keywordArb, 0..2)

fun programArb(size: Int): Arb<Program> =
    bindsArb(size).map(::Program)

fun bindsArb(size: Int): Arb<Binds> {
    val inSize = min(size, 60)
    return Arb.map(keywordArb, lambdaFormArb(inSize), 1, max(size, 2)).map(::Binds)
}

fun lambdaFormArb(size: Int): Arb<LambdaForm> =
    Arb.bind(keywordsArb, keywordsArb, exprArb(size), ::LambdaForm)

// Arb.choice is too eager.
fun exprArb(size: Int): Arb<Expr> {
    val inSize: Int = size * 2 / 3
    return Arb.int(1..6).flatMap {
        when (it) {
            1 -> letArb(inSize)
            2 -> caseArb(inSize)
            3 -> appFArb
            4 -> appCArb
            5 -> appPArb
            else -> litEArb  // weight towards simple literals.
        }
    }
}

fun letArb(size: Int): Arb<Expr.Let> =
    Arb.bind(bindsArb(size), exprArb(size), Expr::Let)

fun caseArb(size: Int): Arb<Expr.Case> =
    Arb.bind(exprArb(size), Arb.list(altArb(size), 0..3)) { expr, alts -> Expr.Case(expr, alts) }

val appFArb = Arb.bind(keywordArb, keywordsArb, Expr::AppF)
val appCArb = Arb.bind(keywordArb, keywordsArb, Expr::AppC)
val appPArb = Arb.bind(keywordArb, keywordArb, keywordArb, Expr::AppP)
val litEArb = Arb.int(-1000, 1000).map(Expr::LitE)

fun altArb(size: Int): Arb<Alt> =
    Arb.bind(keywordUcArb, keywordsArb, exprArb(size), ::Alt)

fun pretty(program: Program) : DocNo = pretty(program.binds)

fun pretty(binds: Binds): DocNo = align(vsep(
    binds.map.entries.map { (variable, lambdaForm) ->
        text(variable) spc text("=") spc pretty(lambdaForm)
    }
))

/*
instance Pretty LambdaForm where
    pretty (LambdaForm free bound body) = (prettyExp . (<+> "->") . prettyBound . prettyFree) "\\"
      where
        prettyFree | null free = id
                   | otherwise = (<> lparen <> hsep (map pretty free) <> rparen)
        prettyBound | null bound = id
                    | null free = (<> hsep (map pretty bound))
                    | otherwise = (<+> hsep (map pretty bound))
        prettyExp = (<+> pretty body)
 */
fun pretty(lambdaForm: LambdaForm): DocNo {
    fun prettyFree(node: DocNo): DocNo = if (lambdaForm.free.isEmpty()) {
        node cat lparen cat hsep(lambdaForm.free.map(::pretty)) cat rparen
    } else {
        node
    }
    fun prettyBound(node: DocNo): DocNo = when {
        lambdaForm.bound.isEmpty() -> node
        lambdaForm.free.isEmpty() -> node cat hsep(lambdaForm.bound.map(::pretty))
        else -> node spc hsep(lambdaForm.bound.map(::pretty))
    }
    fun prettyExp(node: DocNo): DocNo = node spc pretty(lambdaForm.body)

    fun arrow(node: DocNo): DocNo = node spc text("->")

    return prettyExp(arrow(prettyBound(prettyFree(text("\\")))))
}

/*instance Pretty Expr where
    pretty = \expr -> case expr of
        Let binds body ->
            align (vsep [ "let" <+> align (pretty binds)
                        , "in" <+> pretty body ])

        Case scrutinee alts -> vsep
            [ "case" <+> pretty scrutinee <+> "of"
            , indent 4 (align (vsep (map pretty alts))) ]

        AppF f [] -> pretty f
        AppF f args -> pretty f <+> hsep (map pretty args)

        AppC c [] -> pretty c
        AppC c args -> pretty c <+> hsep (map pretty args)

        AppP op x y -> pretty op <+> pretty x <+> pretty y

        LitE lit -> pretty lit
*/
fun pretty(expr: Expr): DocNo = when(expr) {
    is Expr.Let -> align(vsep(text("let") spc align(pretty(expr.binds)), text("in") spc pretty(expr.body)))
    is Expr.Case -> vsep(
        text("case") spc pretty(expr.scrutinee) spc text("of"),
        indent(4, align(vsep(expr.alts.map(::pretty))))
    )
    is Expr.AppF -> if(expr.args.isEmpty()) { pretty(expr.f) } else { pretty(expr.f) spc hsep(expr.args.map(::pretty)) }
    is Expr.AppC -> if(expr.args.isEmpty()) { pretty(expr.c) } else { pretty(expr.c) spc hsep(expr.args.map(::pretty)) }
    is Expr.AppP -> text(expr.op) spc text(expr.x) spc text(expr.y)
    is Expr.LitE -> pretty(expr.lit)
}

fun pretty(alt: Alt): DocNo = if(alt.args.isEmpty()) {
    pretty(alt.con) spc text("->") spc pretty(alt.body)
} else {
    pretty(alt.con) spc hsep(alt.args.map(::pretty)) spc text("->") spc pretty(alt.body)
}

/** QuickCheck has [a size parameter] but we want to limit the number of internal bindings to avoid stack overflow.
 * [a size parameter]: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:getSize
 */
fun randomProgram(rs: RandomSource, size: Int): Program {
    return programArb(size).next(rs=rs)
}


enum class RenderStyle(val render: (Doc<Nothing>) -> SDS<Nothing>) {
    Pretty8050({doc -> layoutPretty(Opts.std(80, 0.5), doc)}),
    Smart8050({doc -> layoutSmart(Opts.std(80, 0.5), doc) }),
    PrettyUnbound({doc -> layoutPretty(Opts.unbounded, doc) }),
    SmartUnbound({doc -> layoutSmart(Opts.unbounded, doc) }),
    Compact({doc -> layoutCompact(doc) }),
}

class CountSink<A> : SimpleSink<A> {
    var count = 0
    override fun openAnn(ann: A) {
    }

    override fun closeAnn(ann: A) {
    }

    override fun emit(cs: CharSequence) {
        count += cs.length
    }
}

data class Times(
    val build: Duration,
    val render: Duration,
    val sink: Duration,
    val count: Int
)

/**
 * There are dedicated benchmarking modules out there, but at this stage I want to see if we can render large documents
 * without falling over.
 */
class Benchmark {
    @OptIn(ExperimentalTime::class)
    fun benchmark(subject: Program, style: RenderStyle) {
        val doc: DocNo
        val buildTime = measureTime {
            doc = pretty(subject)
        }
        val docStream: SDS<Nothing>
        val renderTime = measureTime {
            docStream = style.render(doc)
        }
        val counter = CountSink<Nothing>()
        val sinkTime = measureTime {
            counter.render(docStream)
        }
        // println(AppendableSink<Nothing>().toString(docStream))
        println(Times(buildTime, renderTime, sinkTime, counter.count))
    }

    @Test
    fun benchmark60Compact() {
        benchmark(p60, RenderStyle.Compact)
    }

    @Test
    fun benchmark60Pretty8050() {
        benchmark(p60, RenderStyle.Pretty8050)
    }

    @Test
    fun benchmark60Smart8050() {
        benchmark(p60, RenderStyle.Smart8050)
    }

    @Test
    fun benchmark60PrettyUnbound() {
        benchmark(p60, RenderStyle.PrettyUnbound)
    }

    @Test
    fun benchmark60SmartUnbound() {
        benchmark(p60, RenderStyle.SmartUnbound)
    }

    @Test
    fun benchmark120Compact() {
        benchmark(p120, RenderStyle.Compact)
    }

    @Test
    fun benchmark120Pretty8050() {
        benchmark(p120, RenderStyle.Pretty8050)
    }

    @Test
    fun benchmark120Smart8050() {
        benchmark(p120, RenderStyle.Smart8050)
    }

    @Test
    fun benchmark120PrettyUnbound() {
        benchmark(p120, RenderStyle.PrettyUnbound)
    }

    @Test
    fun benchmark120SmartUnbound() {
        benchmark(p120, RenderStyle.SmartUnbound)
    }

    @Test
    fun benchmark240Compact() {
        benchmark(p240, RenderStyle.Compact)
    }

    @Test
    fun benchmark240Pretty8050() {
        benchmark(p240, RenderStyle.Pretty8050)
    }

    @Test
    fun benchmark240Smart8050() {
        benchmark(p240, RenderStyle.Smart8050)
    }

    @Test
    fun benchmark240PrettyUnbound() {
        benchmark(p240, RenderStyle.PrettyUnbound)
    }

    @Test
    fun benchmark240SmartUnbound() {
        benchmark(p240, RenderStyle.SmartUnbound)
    }
    
    @Test
    fun benchmark480Compact() {
        benchmark(p480, RenderStyle.Compact)
    }

    @Test
    fun benchmark480Pretty8050() {
        benchmark(p480, RenderStyle.Pretty8050)
    }

    @Test
    fun benchmark480Smart8050() {
        benchmark(p480, RenderStyle.Smart8050)
    }

    @Test
    fun benchmark480PrettyUnbound() {
        benchmark(p480, RenderStyle.PrettyUnbound)
    }

    @Test
    fun benchmark480SmartUnbound() {
        benchmark(p480, RenderStyle.SmartUnbound)
    }

    companion object {
        private val p60 = randomProgram(RandomSource.seeded(1L), 60)
        private val p120 = randomProgram(RandomSource.seeded(1L), 120)
        private val p240 = randomProgram(RandomSource.seeded(1L), 240)
        private val p480 = randomProgram(RandomSource.seeded(1L), 480)
    }
}

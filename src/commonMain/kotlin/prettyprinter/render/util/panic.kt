package prettyprinter.render.util

/**
 * Ported from [Panic.hs](https://hackage.haskell.org/package/prettyprinter-1.7.1/src/src/Prettyprinter/Render/Util/Panic.hs).
 */

class Panic(msg: String) : Error(msg)

private const val pleaseReport = "Please report this as a bug"
private const val layoutBug = "This is a bug in the layout algorithm!"

/**
 * Raise a hard 'error' if there is a 'Prettyprinter.SFail' in a 'Prettyprinter.SimpleDocStream'.
 */
fun panicUncaughtFail(): Nothing = throw Panic("»SFail« must not appear in a rendered »SimpleDocStream«. $layoutBug $pleaseReport")

/**
 * Raise a hard 'error' when an annotation terminator is encountered in an unannotated region.
 */
fun panicUnpairedPop(): Nothing = throw Panic("An unpaired style terminator was encountered. $layoutBug $pleaseReport")

/**
 *  Raise a hard generic 'error' when the 'Prettyprinter.SimpleDocStream' to
 *  'Prettyprinter.Render.Util.SimpleDocTree.SimpleDocTree' conversion fails.
 */
fun panicSimpleDocTreeConversionFailed(): Nothing = throw Panic("Conversion from SimpleDocStream to SimpleDocTree failed! $pleaseReport")

/**
 * Raise a hard 'error' when the »to 'Prettyprinter.Render.Util.SimpleDocTree.SimpleDocTree'« parser finishes
 * without consuming the full input.
 */
fun panicInputNotFullyConsumed(): Nothing = throw Panic("Conversion from SimpleDocStream to SimpleDocTree left unconsumed input! $pleaseReport")

fun panicPeekedEmpty(): Nothing = throw Panic("Peeked an empty style stack! $pleaseReport")

fun panicPoppedEmpty(): Nothing = throw Panic("Popped an empty style stack! $pleaseReport")

fun panicSkippingInUnannotated(): Nothing = throw Panic("Tried skipping spaces in unannotated data! $pleaseReport")
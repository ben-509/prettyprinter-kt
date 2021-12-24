package prettyprinter

fun repeatChar(c: Char, n: Int): CharSequence = when {
    n <= 0 -> ""
    else -> object : CharSequence {
        override val length: Int = n
        override fun get(index: Int): Char = c
        override fun subSequence(startIndex: Int, endIndex: Int): CharSequence = repeatChar(c, endIndex - startIndex)
    }
}

fun repeatText(cs: CharSequence, n: Int): CharSequence = repeatText(cs, 0, cs.length * n)

/**
 * Suppose the sequence repeats endlessly, so 'abc' would mean that
 * 0 = a, 1 = b, 2 = c, 3 = a, 4 = b, 5 = c, etc
 * This is then the subsequence of that repetition from s to e.
 */
private fun repeatText(cs: CharSequence, s: Int, e: Int): CharSequence = when {
    cs.isEmpty() -> ""
    cs.length == 1 -> repeatChar(cs[0], e - s)
    e - s <= 0 -> ""
    else -> object : CharSequence {
        override val length: Int = e - s
        override fun get(index: Int): Char = cs[(index + s) % cs.length]
        override fun subSequence(startIndex: Int, endIndex: Int): CharSequence =
            repeatText(cs, s + startIndex, s + endIndex)
    }
}

fun <E> repeatThing(elem: E, n: Int): List<E> = when {
    n <= 0 -> listOf()
    n == 1 -> listOf(elem)
    else -> object : AbstractList<E>() {
        override val size: Int = n
        override fun get(index: Int): E = elem
        override fun contains(element: E): Boolean {
            return elem == element
        }

        override fun subList(fromIndex: Int, toIndex: Int): List<E> {
            return repeatThing(elem, toIndex - fromIndex)
        }
    }
}

/**
 * Applies a mapping function to everything but the last element.
 */
fun <E> mapAllExceptLast(seq: Sequence<E>, func: (E) -> E): Sequence<E> = object : Sequence<E> {
    override fun iterator(): Iterator<E> = mapAllExceptLast(seq.iterator(), func)
}

fun <E> mapAllExceptLast(iterable: Iterable<E>, func: (E) -> E): Iterable<E> = object : Iterable<E> {
    override fun iterator(): Iterator<E> = mapAllExceptLast(iterable.iterator(), func)
}

fun <E> mapAllExceptLast(iter: Iterator<E>, func: (E) -> E): Iterator<E> =
    object : Iterator<E> {
        override fun hasNext(): Boolean {
            return iter.hasNext()
        }

        override fun next(): E {
            val elem = iter.next()
            return if (iter.hasNext()) {
                func(elem)
            } else {
                elem
            }
        }
    }

fun <A, B> foldr(init: B, list: Iterable<A>, func: (A, B) -> B): B {
    var accum: B = init
    for (elem in list) {
        accum = func(elem, accum)
    }
    return accum
}

sealed class HList<out E> : AbstractCollection<E>() {
    abstract val isSingleton: Boolean

    class Cons<E>(val head: E, val tail: HList<E>) : HList<E>() {
        override val size: Int
            get() = tail.size + 1

        override fun isEmpty(): Boolean {
            return false
        }

        override val isSingleton: Boolean
            get() = tail is Nil
    }

    object Nil : HList<Nothing>() {
        override val size: Int = 0

        override fun isEmpty(): Boolean {
            return true
        }

        override val isSingleton: Boolean
            get() = false
    }

    override fun iterator(): Iterator<E> {
        class I(private var node: HList<E>) : Iterator<E> {
            override fun hasNext(): Boolean = node is Cons
            override fun next(): E {
                when (val n = node) {
                    Nil -> throw NoSuchElementException()
                    is Cons -> {
                        node = n.tail
                        return n.head
                    }
                }
            }
        }
        return I(this)
    }
}

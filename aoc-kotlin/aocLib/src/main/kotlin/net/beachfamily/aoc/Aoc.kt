package net.beachfamily.aoc

import com.google.common.collect.ImmutableMultimap
import com.google.common.collect.ImmutableSetMultimap
import java.nio.file.Files
import java.nio.file.Paths

fun readInput(name: String) : String {
    val dir = Paths.get("/Users/brianb/sandbox/advent-of-code/problems")  // TODO: use env var
    val path = dir.resolve(name)
    return Files.readAllBytes(path).decodeToString()
}


/**
 * Returns a Sequence of all possible pairs of items chosen from the given list.
 * Each pair contains two distinct items from the list, in the same order they
 * appear in the list.
 */
fun <T> allPairs(list: List<T>): Sequence<Pair<T, T>> = sequence {
    for (i in list.indices) {
        for (j in (i + 1) ..< list.size) {
            yield(Pair(list[i], list[j]))
        }
    }
}

/**
 * Same as `allPairs`, but every pair is returned in both orders.
 */
fun <T> allPairsBothOrders(list: List<T>): Sequence<Pair<T, T>> = sequence {
    for ((first, second) in allPairs(list)) {
        yield(Pair(first, second))
        yield(Pair(second, first))
    }
}

/**
 * If the input Sequence contains exactly one item, returns that item.
 * In all other cases, throws an exception.
 */
fun <T> singleItem(sequence: Sequence<T>): T {
    val iterator = sequence.iterator()
    if (!iterator.hasNext()) throw IllegalArgumentException("Sequence is empty")
    val singleItem = iterator.next()
    if (iterator.hasNext()) throw IllegalArgumentException("Sequence contains more than one item")
    return singleItem
}

/**
 * Splits the input string at newlines and returns a list of all non-blank lines.
 *
 * @param s Input string
 * @return A list of non-blank lines from the input string.
 */
fun lines(s: String) : List<String> =
    s.split('\n').filter { it.isNotBlank() }

/**
 * Splits the input string into words, where words are defined as sequences of non-whitespace
 * characters separated by sequences of whitespace characters. Only non-blank words are retained.
 * Words include any non-whitespace characters, including punctuation.
 *
 * @param s Input string to be processed.
 * @return A list of non-blank words from the input string.
 */
fun words(s: String) : List<String> =
    s.split(Regex("\\s+")).filter { it.isNotBlank() }

/**
 * Transposes a sequence of sequences.
 * 
 * The first item in the returned sequence is a sequence of the first item
 * in each of the input sequences.  The length of the output sequence is
 * the minimum of the lengths of the input sequences.
 */
fun <T> Sequence<Sequence<T>>.transpose(): Sequence<Sequence<T>> {
    val input = this
    return sequence {
        // Get iterators for each sequence in the input
        val iterators = input.map { it.iterator() }.toList()

        // Loop until any of the iterators has no next element
        while (iterators.all { it.hasNext() }) {
            // Yield a sequence from the next elements of each iterator
            yield(iterators.map { it.next() }.asSequence())
        }
    }
}

/**
 * Takes a sequence of values and produces a map from value to the number of times
 * that value occurs in the sequence.
 */
fun <T> Sequence<T>.countOccurrences(): Map<T, Int> {
    return this.fold(mutableMapOf()) { acc, item ->
        acc[item] = acc.getOrDefault(item, 0) + 1
        acc
    }
}

/**
 * Takes a Sequence of values and returns a Sequence of Pairs of adjacent values.
 */
fun <T> Sequence<T>.pairs(): Sequence<Pair<T, T>> = sequence {
    val iterator = this@pairs.iterator()
    if (iterator.hasNext()) {
        var previous = iterator.next()
        while (iterator.hasNext()) {
            val current = iterator.next()
            yield(Pair(previous, current))
            previous = current
        }
    }
}

/**
 * Parses a multi-line string where each line is a whitespace-separated list of Ints.
 */
fun seqOfSeqOfInt(input: String): Sequence<Sequence<Int>> {
    return lines(input).asSequence().map { words(it).asSequence().map { it.toInt() } }
}

/**
 * Sorts a Sequence.
 */
fun <T : Comparable<T>> Sequence<T>.sortedSeq(): Sequence<T> {
    return this.toList().sorted().asSequence()
}

/**
 * Convert a Sequence of two things into a Pair
 */
fun <T> Sequence<T>.asPair(): Pair<T, T> {
    require(this.count() == 2)
    return this.toList().let { it[0] to it[1] }
}

/**
 * A location in a grid.
 */
data class Point(val x: Int, val y: Int)

/**
 * A bounded grid of things.
 */
data class Grid<T>(
    val xmin: Int,
    val xmax: Int,
    val ymin: Int,
    val ymax: Int,
    val data: Map<Point, T>
) {
    operator fun get(point: Point): T? {
        return data[point]
    }
}

/**
 * Make a grid from the given map entries.
 */
fun <T> makeGrid(items: Sequence<Pair<Point, T>>): Grid<T> {
    return Grid(
        items.map { it.first.x }.min(),
        items.map { it.first.x }.max(),
        items.map { it.first.y }.min(),
        items.map { it.first.y }.max(),
        items.toMap()
    )
}

/**
 * Convert input into a grid of chars.
 */
fun gridOfChar(input: String): Grid<Char> {
    return makeGrid(
        sequence {
            for ((y, line) in lines(input).withIndex()) {
                for ((x, c) in line.withIndex()) {
                    yield(Pair(Point(x, y), c))
                }
            }
        }
    )
}

/**
 * Builds a multimap from the pairs of keys/values provided.
 */
fun <K, V> Sequence<Pair<K, V>>.toSetMultimap(): ImmutableMultimap<K, V> {
    val builder = ImmutableSetMultimap.builder<K, V>()
    for ((k, v) in this) {
        builder.put(k!!, v!!)
    }
    return builder.build()
}

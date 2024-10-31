package net.beachfamily.aoc

fun main() {
    val input = readInput("y2017d06")
    val bins = words(input).map { it.toInt() }
    println(part1(bins))
    println(part2(input))
}

fun part1(start : List<Int>) : Int {
    return indexOfFirstRepeated(generateSequence(start) { step(it) }) ?: -1
}

fun part2(s: String) : Int {
    return s.length
}

fun step(bins: List<Int>) : List<Int> {
    val i = indexOfBiggest(bins.asSequence())
    val binSize = bins[i]
    
    val result = bins.toMutableList()
    result[i] = 0
    for (j in 1..binSize) {
        result[(i + j) % bins.size] += 1
    }

    return result.toList()
}

fun indexOfBiggest(bins: Sequence<Int>) : Int {
    return bins.withIndex().maxWith(::compareIndexedValues).index ;
}

/**
 * Returns the index of the first repeated item in the Sequence.
 */
fun <T> indexOfFirstRepeated(sequence: Sequence<T>): Int? {
    val seen = mutableSetOf<T>()
    sequence.forEachIndexed { index, item ->
        if (!seen.add(item)) return index
    }
    return null
}

fun compareIndexedValues(pair1: IndexedValue<Int>, pair2: IndexedValue<Int>): Int {
    val firstComparison = pair1.value.compareTo(pair2.value)
    return if (firstComparison != 0) firstComparison else - pair1.index.compareTo(pair2.index)
}

/**
 * Compares two Pair objects whose elements are comparable.
 *
 * The primary comparison is on the first element, falling back to the
 * second element when the first elements are equal.
 */
fun <A : Comparable<A>, B : Comparable<B>> comparePairs(pair1: Pair<A, B>, pair2: Pair<A, B>): Int {
    val firstComparison = pair1.first.compareTo(pair2.first)
    return if (firstComparison != 0) firstComparison else pair1.second.compareTo(pair2.second)
}

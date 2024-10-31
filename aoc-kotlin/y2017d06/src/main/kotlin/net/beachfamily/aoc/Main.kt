package net.beachfamily.aoc

fun main() {
    val input = readInput("y2017d06")
    val bins = words(input).map { it.toInt() }
    println(part1(bins))
    println(part2(bins))
}

fun part1(start : List<Int>) : Int {
    return indexOfFirstRepeated(generateSequence(start) { step(it) }) ?: -1
}

fun part2(start : List<Int>) : Int {
    val (a, b) = firstCycle(generateSequence(start) { step(it) })!!
    return b - a
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
    return bins.withIndex().maxWith(::compareIndexedValues).index
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

/**
 * Returns a pair of indices that are the start of the first cycle in
 * the Sequenc, and the next occurrance of the same value.
 */
fun <T> firstCycle(seq: Sequence<T>): Pair<Int, Int>? {
    val seen = mutableMapOf<T, Int>()
    seq.forEachIndexed { index, item ->
        if (seen.containsKey(item)) {
            return seen[item]!! to index
        }
        seen[item] = index
    }
    return null
}

/**
 * Compares two indexed values, sorting primarily on the value, and secondarily
 * on the inverse of the index -- when there's a tie on value, the first one wins.
 */

fun compareIndexedValues(pair1: IndexedValue<Int>, pair2: IndexedValue<Int>): Int {
    val firstComparison = pair1.value.compareTo(pair2.value)
    return if (firstComparison != 0) firstComparison else - pair1.index.compareTo(pair2.index)
}

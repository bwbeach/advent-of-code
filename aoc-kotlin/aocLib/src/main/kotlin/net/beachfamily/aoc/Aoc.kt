package net.beachfamily.aoc

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